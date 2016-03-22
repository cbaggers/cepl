(in-package :jungl)

;; extract details from args and delegate to %def-gpu-function
;; for the main logic
(defmacro defun-g (name args &body body)
  "Define a function that runs on the gpu."
  ;; The code here splits and validates the arguments but the meat
  ;; of gpu function definition happens in the %def-gpu-function call
  ;; at the tail
  ;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
  ;; seperate any doc-string or declarations from the body
  (let ((doc-string (when (stringp (first body)) (pop body)))
        (declarations (when (and (listp (car body)) (eq (caar body) 'declare))
                        (pop body))))
    ;; split the argument list into the categoried we care aboutn
    (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
                 (instancing :&instancing))
        (varjo:lambda-list-split '(:&uniform :&context :&instancing) args)
      ;; check the arguments are sanely formatted
      (mapcar #'(lambda (x) (assert-arg-format name x)) in-args)
      (mapcar #'(lambda (x) (assert-arg-format name x)) uniforms)
      ;; equivalent types need to be swapped out
      (let* ((in-args in-args)
	     (uniforms uniforms))
	;; now the meat
        (%def-gpu-function name in-args uniforms context body instancing
                           nil nil doc-string declarations)))))

(defun assert-arg-format (gfunc-name x)
  (unless (listp x)
    (error 'gfun-invalid-arg-format :gfun-name gfunc-name :invalid-pair x))
  x)

(defun undefine-gpu-function (name)
  (%unsubscibe-from-all name)
  (remhash name *gpu-func-specs*)
  (remhash name *dependent-gpu-functions*)
  nil)


;;--------------------------------------------------

(defun %def-gpu-function (name in-args uniforms context body instancing
                          equivalent-inargs equivalent-uniforms
                          doc-string declarations)
  "This is the meat of defun-g. it is broken down as follows:

   [0] makes a gpu-func-spec that will be populated a stored later.

   [1] turn this function definiton into a macro that injects a labels form
       containing the function body and whos body is a call to that local
       function. This may seem like madness but it will make sense soon.
       So now this means that wherever you call the function in shader code
       there will be a local function and call.

       One of the optimizations that varjo does is find all functions that are
       identical and merge them.
       This works as there are no local functions in glsl, they are all moved to
       shader-global scope.

   [2] %test-&-update-spec compiles the code to check for errors and log
       dependencies. (this is called at runtime)

   [3] %make-gpu-func-spec is called at expand time to write a lisp function
       with the same signature as the gpu-function. This gives code hinting and
       also a decent error message if you try calling it from the cpu.

   [4] the purpose of %recompile-gpu-function-and-pipelines is to recompile and
       functions or pipelines that depend on this gpu function. It does this
       by calling %recompile-gpu-function-and-pipelines on all the gpu function
       that depend on this func and then the recompile-function for all
       pipelines that depend on this gpu function. To this end it walks depth
       first too all affected pipelines.

   [5] At runtime this looks for any gpu function that listed this function as
       one of it's missing dependencies and calls %test-&-update-spec on them.
       Note that this will (possibly) update the spec but will not trigger a
       recompile in the pipelines."
  (let ((spec (%make-gpu-func-spec name in-args uniforms context body instancing
                                   equivalent-inargs equivalent-uniforms nil nil
                                   doc-string declarations nil))) ;;[0]
    ;; this gets the functions used in the body of this function
    ;; it is *not* recursive
    (let* ((in-arg-names (mapcar #'first in-args))
	   (uniform-names (mapcar #'first uniforms))
	   (macro-func-name (gensym (symbol-name name)))
	   (fbody `(list 'varjo-lang:labels-no-implicit ;;[1]
			 '((,macro-func-name (,@in-args ,@uniforms) ,@body))
			 (list ',macro-func-name
			       ,@in-arg-names ,@uniform-names)))
	   (to-compile `(lambda (,@in-arg-names ,@uniform-names) ;;[1]
			  ,fbody)))
      (%update-gpu-function-data spec nil nil)
      (varjo::add-macro name (compile nil to-compile)
			context varjo::*global-env*);;[1]
      `(progn
         (%test-&-update-spec ,(%serialize-gpu-func-spec spec));;[2]
         ,(%make-stand-in-lisp-func spec);;[3]
         (%recompile-gpu-function-and-pipelines ',name);;[4]
	 (update-specs-with-missing-dependencies);;[5]
	 ',name))))

(defvar *warn-when-cant-test-compile* t)

(defun %test-&-update-spec (spec)
  "Use varjo to compile the code.
   [0] If the compilation throws a could-not-find-function error, then record
   that missing function's name as a missing dependency.

   [1] If it succeeds then look at the list of used-macros, and check which of
   the names of the macros match the names of a gpu function. The ones that
   match are the dependencies.

   [2] We also record the uniforms in the compiled result. The uniforms in the
   definition are the public interface, but the compiler may have removed or
   modified the uniforms. To this end we store the final uniforms and the forms
   that transform between the public uniform arguments and the internal ones."
  (with-gpu-func-spec spec
    (handler-case
	(varjo:with-stemcell-infer-hook
	    #'try-guessing-a-varjo-type-for-symbol
	  (let ((compiled
		 (v-translate in-args uniforms
			      (union '(:vertex :fragment :iuniforms :330)
				     context)
			      `(progn ,@body)
			      nil)))
	    (setf actual-uniforms (uniforms compiled) ;;[2]
		  uniform-transforms (with-hash (uv 'uniform-vals)
					 (third-party-metadata compiled)
				       (map-hash #'list uv)))
	    (%update-gpu-function-data
	     spec
	     (remove-if-not #'gpu-func-spec (varjo:used-macros compiled)) ;;[1]
	     compiled)))
      (varjo-conditions:could-not-find-function (e) ;;[0]
	(setf missing-dependencies (list (slot-value e 'varjo::name)))
	(when *warn-when-cant-test-compile*
	  (format t "~% jungl: the function ~s was not found when compiling ~s"
		  (first missing-dependencies) name))
	(%update-gpu-function-data spec nil nil)))))

(defun %recompile-gpu-function-and-pipelines (name)
  "Recompile all pipelines that depend on the named gpu function or any other
   gpu function that depends on the named gpu function. It does this by doing
   the following:

   [0] Recursively call this function on all gpu functions that use the
       gpu function named in the argument

   [1] Trigger a recompile on all pipelines that depend on this gpu function"
  ;; recompile gpu-funcs that depends on name
  (mapcar #'%recompile-gpu-function-and-pipelines
	  (funcs-that-use-this-func name));;[0]
  ;; and recompile pipelines that depend on name
  (recompile-pipelines-that-use-this-as-a-stage name))

(defun %update-gpu-function-data (spec depends-on compiled)
  "[0] Add or update the spec

   [1] (re)subscribe to all the dependencies

   [2] cache the compile result so we can retrieve it with #'pull1-g
       or the code with #'pull-g"
  (with-slots (name) spec
    (%unsubscibe-from-all name);;[1]
    (mapcar (lambda (_) (%subscribe-to-gpu-func name _)) depends-on);;[1]
    (when +cache-last-compile-result+
      (setf (slot-value spec 'cached-compile-results) compiled));;[2]
    (setf (gpu-func-spec name) spec)));;[0]

(defun %update-glsl-stage-data (spec)
  "[0] Add or update the spec"
  (with-slots (name) spec
    (setf (gpu-func-spec name) spec)));;[0]

(defun %subscribe-to-gpu-func (name subscribe-to-name)
  "As the name would suggest this makes one function dependent on another
   It is used by #'%test-&-update-spec via #'%update-gpu-function-data "
  (assert (not (eq name subscribe-to-name)))
  (symbol-macrolet ((func-specs (funcs-that-use-this-func subscribe-to-name)))
    (when (and (gpu-func-spec subscribe-to-name)
               (not (member name func-specs)))
      (format t "; func ~s subscribed to ~s~%" name subscribe-to-name)
      (push name func-specs))))

(defun %unsubscibe-from-all (name)
  "As the name would suggest this removes one function's dependency on another
   It is used by #'%test-&-update-spec via #'%update-gpu-function-data"
  (labels ((%remove-gpu-function-from-dependancy-table (func-name dependencies)
             (when (member name dependencies)
               (setf (funcs-that-use-this-func func-name)
                     (remove name dependencies)))))
    (maphash #'%remove-gpu-function-from-dependancy-table
             *dependent-gpu-functions*)))

(defun %make-stand-in-lisp-func (spec)
  "Makes a regular lisp function with the same names and arguments
  (where possible) as the gpu function who's spec is provided.

  If called the function will throw an error saying that the function
  can't currently be used from the cpu.

  This means we get function arg hints, doc-string and also we have the
  opportunity to provide a cpu implementation one day we want to."
  (with-gpu-func-spec spec
    (let ((arg-names (mapcar #'first in-args))
          (uniform-names (mapcar #'first uniforms)))
      `(defun ,name (,@arg-names
		     ,@(when uniforms (cons (symb :&key) uniform-names)))
	 ,@(when doc-string (list doc-string))
         (declare (ignore ,@arg-names ,@uniform-names))
         (warn "GPU Functions cannot currently be used from the cpu")))))

;;--------------------------------------------------

(defun %aggregate-uniforms (uniforms &optional accum)
  "The meat behind the uniform aggregation functions
   The reason we need to aggregate uniforms is as follows:
   - pipelines are made of composed gpu functions
   - each gpu function may introduce uniforms
   - to this end we need to make sure the different functions' uniforms are
     compatible and then return a final list of aggregated uniforms.

   The criteria for a uniform being valid is that:
   [0] there is no other uniform with matching name, hense no collision
   [1] the uniform matches perfectly so no collision
   [2] otherwise it's a clash"
  (if uniforms
      (let ((u (first uniforms)))
        (cond
	  ;;[0]
          ((not (find (first u) accum :test #'equal :key #'first))
           (%aggregate-uniforms (rest uniforms) (cons u accum)))
          ;;[1]
          ((find u accum :test #'equal)
           (%aggregate-uniforms (rest uniforms) accum))
          ;;[2]
          (t (error "Uniforms for the functions are incompatible: ~a ~a"
                    u accum))))
      accum))

(defun aggregate-uniforms (names &optional accum interal-uniforms-p)
  "[0] Aggregates the uniforms from the named gpu-functions,

   The reason we need to aggregate uniforms is as follows:
   - pipelines are made of composed gpu functions
   - each gpu function may introduce uniforms
   - to this end we need to make sure the different functions' uniforms are
     compatible and then return a final list of aggregated uniforms."
  (if names
      (aggregate-uniforms
       (rest names)
       (%aggregate-uniforms;;[0]
	(with-gpu-func-spec (gpu-func-spec (first names))
	  (if interal-uniforms-p
	      actual-uniforms
	      uniforms))
	accum)
       interal-uniforms-p)
      accum))

;;--------------------------------------------------

(defun get-func-as-stage-code (name)
  (with-gpu-func-spec (gpu-func-spec name)
    (list in-args uniforms context body)))

;;--------------------------------------------------

(defun %varjo-compile-as-pipeline (parsed-gpipe-args)
  "Compile the gpu functions for a pipeline
   The argument to this function is a list of pairs.
   Each pair contains:
   - the shader stage (e.g. vertex fragment etc)
   - the name of the gpu function to use for this stage"
  (varjo:with-stemcell-infer-hook #'try-guessing-a-varjo-type-for-symbol
    (v-rolling-translate
     (mapcar #'parsed-gpipe-args->v-translate-args parsed-gpipe-args))))

(defun parsed-gpipe-args->v-translate-args (stage-pair)
  "%varjo-compile-as-pipeline simply takes (stage . gfunc-name) pairs from
   %compile-link-and-upload needs to call v-rolling-translate. To do this
   we need to look up the gpu function spec and turn them into valid arguments
   for the v-rolling-translate function.
   That is what this function does.
   It also:
   [0] if it's a glsl-stage then it is already compiled. Pass the compile result
       and let varjo handle it
   [1] enables implicit uniforms
   [2] validate that either the gpu-function's context didnt specify a stage
       explicitly or that, if it did, that it matches the stage it is being used
       for now"
  (dbind (stage-type . stage-name) stage-pair
    (if (typep (gpu-func-spec stage-name) 'glsl-stage-spec)
	(with-glsl-stage-spec (gpu-func-spec stage-name)
	  compiled);;[0]
	(dbind (in-args uniforms context code) (get-func-as-stage-code stage-name)
	  ;;[2]
	  (let ((n (count-if (lambda (_) (member _ varjo:*stage-types*))
			     context)))
	    (assert (and (<= n 1) (if (= n 1) (member stage-type context) t))))
	  (let ((context (cons :iuniforms ;;[1]
			       (cons stage-type
				     (remove stage-type context)))))
	    (list in-args
		  uniforms
		  context
		  `(progn ,@code)))))))

;;--------------------------------------------------

(defun parse-gpipe-args (args)
  "Gets the stage pairs and context for the given gpipe form.
   If there are only two gpu functions named and no explicit stages then
   it is assumed that the first is the vertex stage and the second the fragment
   stage.
   Otherwise you are expected to name the stages. You can name one and then let
   this function fill in the rest, but I think that's a bit hairy and will
   probably be removed.

   stage pairs are of the form (stage-name . gpu-function-name)"
  (let ((cut-pos (or (position :context args) (length args))))
    (destructuring-bind (&key context) (subseq args cut-pos)
      (list
       (let ((args (subseq args 0 cut-pos)))
         (if (and (= (length args) 2) (not (some #'keywordp args)))
             `((:vertex . ,(cadar args)) (:fragment . ,(cadadr args)))
             (let* ((stages (copy-list varjo:*stage-types*))
                     (results (loop :for a :in args
                                :if (keywordp a) :do (setf stages (cons a (subseq stages (1+ (position a stages)))))
                                :else :collect (cons (or (pop stages) (error "Invalid gpipe arguments, no more stages"))
                                                     (cadr a)))))
               (remove :context results :key #'car))))
       context))))

;;--------------------------------------------------

(defun try-guessing-a-varjo-type-for-symbol (s)
  "This function is provided to varjo to allow inference of the
   types of implicit uniforms."
  ;; only works on specials because of symbol-value
  (when (boundp s)
    (guess-a-varjo-type (symbol-value s))))

(defun guess-a-varjo-type (x)
  (typecase x
    (number (guess-a-varjo-number-type x))
    (array (guess-a-varjo-array-type x))
    (boolean (guess-a-varjo-bool-type x))
    (cepl.space:space 'cepl.space:space-g)
    (t (error "Cant guess a suitable type for ~s" x))))

(defun guess-a-varjo-bool-type (x)
  (if (eql x t)
      :bool
      (error "Cant guess a suitable type for ~s" x)))

(defun guess-a-varjo-array-type (x)
  (typecase x
    ((simple-array single-float (2)) :vec2)
    ((simple-array single-float (3)) :vec3)
    ((simple-array single-float (4)) :vec4)
    ((simple-array single-float (9)) :mat3)
    ((simple-array single-float (16)) :mat4)))

(defun guess-a-varjo-number-type (x)
  (typecase x
    ((or single-float double-float) (guess-a-varjo-float-type x))
    (integer (guess-a-varjo-integer-type x))
    (t (error "Cant guess a suitable type for ~s" x))))

(defun guess-a-varjo-float-type (x)
  (if (typep x 'single-float)
      :float
      :double))

(defun guess-a-varjo-integer-type (x)
  (typecase x
    ((signed-byte 32) :int)
    ((unsigned-byte 32) :uint)
    (t (error "Cant guess a suitable type for ~s" x))))
