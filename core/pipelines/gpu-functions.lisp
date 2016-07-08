(in-package :cepl.pipelines)
(in-readtable fn:fn-reader)

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
      (assert (null context))
      ;; now the meat
      (%def-gpu-function name in-args uniforms body instancing
                         doc-string declarations))))

(defun assert-arg-format (gfunc-name x)
  (unless (listp x)
    (error 'gfun-invalid-arg-format :gfun-name gfunc-name :invalid-pair x))
  x)


;;--------------------------------------------------

(defun %def-gpu-function (name in-args uniforms body instancing
                          doc-string declarations)
  "This is the meat of defun-g. it is broken down as follows:

   [0] makes a gpu-func-spec that will be populated a stored later.

   [1] Adds a external function definition to varjo

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
       one of its missing dependencies and calls %test-&-update-spec on them.
       Note that this will (possibly) update the spec but will not trigger a
       recompile in the pipelines."
  (let ((spec (%make-gpu-func-spec name in-args uniforms nil body instancing
                                   nil nil nil nil doc-string declarations
                                   nil)));;[0]
    ;; this gets the functions used in the body of this function
    ;; it is *not* recursive
    (%update-gpu-function-data spec nil nil)
    (varjo::add-external-function name in-args uniforms body);;[1]
    `(progn
       (%test-&-update-spec ,(serialize-gpu-func-spec spec));;[2]
       ,(make-stand-in-lisp-func spec);;[3]
       (%recompile-gpu-function-and-pipelines ,(inject-func-key spec));;[4]
       (update-specs-with-missing-dependencies);;[5]
       ',name)))

(defun get-version-from-context (context)
  (first (remove-if-not λ(member _ varjo::*supported-versions*)
			context)))

(defun swap-version (glsl-version context)
  (cons glsl-version (remove-if λ(find _ varjo::*supported-versions*) context)))

(defun compute-glsl-version (&rest contexts)
  (let* ((versions (mapcar #'get-version-from-context contexts))
	 (trimmed (remove-duplicates (remove nil versions))))
    (case= (length trimmed)
      (0 (cepl.context::get-best-glsl-version))
      (1 (first trimmed))
      (otherwise nil))))

(defvar *warn-when-cant-test-compile* t)

(defun %test-&-update-spec (spec)
  "Use varjo to compile the code.
   [0] If the compilation throws a could-not-find-function error, then record
   that missing function's name as a missing dependency.

   [1] If it succeeds then look at the list of used external-functions, and
   check which of the names of the macros match the names of a gpu function.
   The ones that match are the dependencies.

   [2] We also record the uniforms in the compiled result. The uniforms in the
   definition are the public interface, but the compiler may have removed or
   modified the uniforms. To this end we store the final uniforms and the forms
   that transform between the public uniform arguments and the internal ones."
  (with-gpu-func-spec spec
    (handler-case
	(varjo:with-constant-inject-hook #'try-injecting-a-constant
	  (varjo:with-stemcell-infer-hook #'try-guessing-a-varjo-type-for-symbol
	    (let* ((context (union '(:vertex :fragment :iuniforms) context))
		   (context (swap-version
			     (or (compute-glsl-version context)
				 (error 'glsl-version-conflict-in-gpu-func
					:name name
					:context context))
			     context))
		   (compiled
		    (v-translate in-args uniforms context `(progn ,@body) nil)))
	      (setf actual-uniforms (uniforms compiled) ;;[2]
		    uniform-transforms (with-hash (uv 'uniform-vals)
					   (third-party-metadata compiled)
					 (map-hash #'list uv)))
	      (%update-gpu-function-data
	       spec
	       (remove-if-not #'gpu-func-spec
			      (varjo::used-external-functions compiled)) ;;[1]
	       compiled))))
      ;; vv- called if failed
      (varjo-conditions:could-not-find-function (e) ;;[0]
        (setf missing-dependencies (list (slot-value e 'varjo::name)))
        (when *warn-when-cant-test-compile*
          (format t "~% cepl: the function ~s was not found when compiling ~s"
                  (first missing-dependencies) name))
        (%update-gpu-function-data spec nil nil)))))

(defmethod %recompile-gpu-function-and-pipelines (key)
  (%recompile-gpu-function-and-pipelines (func-key key)))

(defmethod %recompile-gpu-function-and-pipelines ((key func-key))
  "Recompile all pipelines that depend on the named gpu function or any other
   gpu function that depends on the named gpu function. It does this by doing
   the following:

   [0] Recursively call this function on all gpu functions that use the
       gpu function named in the argument

   [1] Trigger a recompile on all pipelines that depend on this gpu function"
  ;; recompile gpu-funcs that depends on name
  (mapcar #'%recompile-gpu-function-and-pipelines
	  (funcs-that-use-this-func key));;[0]
  ;; and recompile pipelines that depend on name
  (recompile-pipelines-that-use-this-as-a-stage key))

(defun %update-gpu-function-data (spec depends-on compiled)
  "[0] Add or update the spec

   [1] (re)subscribe to all the dependencies

   [2] cache the compile result so we can retrieve it with #'pull1-g
       or the code with #'pull-g"
  (%unsubscibe-from-all spec);;[1]
  (map nil λ(%subscribe-to-gpu-func spec _) depends-on);;[1]
  (when +cache-last-compile-result+
    (setf (slot-value spec 'cached-compile-results) compiled));;[2]
  (setf (gpu-func-spec spec) spec));;[0]

(defun %update-glsl-stage-data (spec)
  "[0] Add or update the spec"
  (setf (gpu-func-spec spec) spec));;[0]

(defmethod %subscribe-to-gpu-func (func subscribe-to)
  "As the name would suggest this makes one function dependent on another
   It is used by #'%test-&-update-spec via #'%update-gpu-function-data "
  (let ((func (func-key func))
	(subscribe-to (func-key subscribe-to)))
    (assert (not (func-key= func subscribe-to)))
    (symbol-macrolet ((func-specs (funcs-that-use-this-func subscribe-to)))
      (when (and (gpu-func-spec subscribe-to)
		 (not (member func func-specs :test #'func-key=)))
	(format t "; func ~s subscribed to ~s~%"
		(name func)
		(name subscribe-to))
	(push func func-specs)))))

(defun make-stand-in-lisp-func (spec)
  "Makes a regular lisp function with the same names and arguments
  (where possible) as the gpu function who's spec is provided.

  If called the function will throw an error saying that the function
  can't currently be used from the cpu.

  This means we get function arg hints, doc-string and also we have the
  opportunity to provide a cpu implementation one day we want to."
  (with-gpu-func-spec spec
    (let ((arg-names (mapcar #'first in-args))
          (uniform-names (mapcar #'first uniforms)))
      `(setf (symbol-function ',name)
	     (lambda (,@arg-names
		      ,@(when uniforms (cons (symb :&key) uniform-names)))
	       ,@(when doc-string (list doc-string))
	       (declare (ignore ,@arg-names ,@uniform-names))
	       (warn "GPU Functions cannot currently be used from the cpu"))))))

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

(defun aggregate-uniforms (keys &optional accum interal-uniforms-p)
  "[0] Aggregates the uniforms from the named gpu-functions,

   The reason we need to aggregate uniforms is as follows:
   - pipelines are made of composed gpu functions
   - each gpu function may introduce uniforms
   - to this end we need to make sure the different functions' uniforms are
     compatible and then return a final list of aggregated uniforms."
  (if keys
      (aggregate-uniforms
       (rest keys)
       (%aggregate-uniforms;;[0]
	(with-gpu-func-spec (gpu-func-spec (first keys))
	  (if interal-uniforms-p
	      actual-uniforms
	      uniforms))
	accum)
       interal-uniforms-p)
      accum))

;;--------------------------------------------------

(defun get-func-as-stage-code (stage)
  (with-gpu-func-spec stage
    (list in-args uniforms context body)))

;;--------------------------------------------------

(defun %varjo-compile-as-pipeline (parsed-gpipe-args)
  "Compile the gpu functions for a pipeline
   The argument to this function is a list of pairs.
   Each pair contains:
   - the shader stage (e.g. vertex fragment etc)
   - the name of the gpu function to use for this stage"
  (varjo:with-constant-inject-hook #'try-injecting-a-constant
    (varjo:with-stemcell-infer-hook #'try-guessing-a-varjo-type-for-symbol
      (v-rolling-translate
       (mapcar #'parsed-gpipe-args->v-translate-args
	       parsed-gpipe-args)))))

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
  (dbind (stage-type . stage) stage-pair
    (if (typep (gpu-func-spec stage) 'glsl-stage-spec)
	(with-glsl-stage-spec (gpu-func-spec stage)
	  compiled);;[0]
	(dbind (in-args uniforms context code) (get-func-as-stage-code stage)
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

(defun get-stage-key (stage-designator)
  (cond
    ((and (listp stage-designator) (eq (first stage-designator) 'function))
     (get-stage-key (second stage-designator)))
    ((symbolp stage-designator)
     (let* ((name stage-designator)
	    (funcs (gpu-func-specs name)))
       (case= (length funcs)
	 (0 (error 'stage-not-found :designator name))
	 (1 (func-key (first funcs)))
	 (otherwise
	  (error 'multiple-gpu-func-matches
		 :designator stage-designator
		 :possible-choices (mapcar λ(with-gpu-func-spec _
					      (cons stage-designator
						    (mapcar #'second in-args)))
					   funcs))))))
    ((listp stage-designator)
     (let ((key (new-func-key (first stage-designator) (rest stage-designator))))
       (if (gpu-func-spec key)
	   key
	   (error 'stage-not-found :designator stage-designator))))
    (t (error "CEPL: Bug in get-stage-key - ~s" stage-designator))))

(defun parse-gpipe-args (args)
  "Gets the stage pairs and context for the given gpipe form.
   If there are only two gpu functions named and no explicit stages then
   it is assumed that the first is the vertex stage and the second the fragment
   stage.
   Otherwise you are expected to name the stages. You can name one and then let
   this function fill in the rest, but I think that's a bit hairy and will
   probably be removed.

   stage pairs are of the form (stage-name . gpu-function-name)"
  (let ((cut-pos (or (position :post args) (length args))))
    (destructuring-bind (&key post) (subseq args cut-pos)
      (let ((args (subseq args 0 cut-pos)))
	(list
	 (if (and (= (length args) 2) (not (some #'keywordp args)))
	     (list (cons :vertex (get-stage-key (first args)))
		   (cons :fragment (get-stage-key (second args))))
	     (dbind (&key vertex tesselation-control
			  tesselation-evaluation geometry
			  fragment) args
	       (remove nil
		       (list (when vertex
			       (cons :vertex (get-stage-key vertex)))
			     (when tesselation-control
			       (cons :tesselation-control
				     (get-stage-key tesselation-control)))
			     (when tesselation-evaluation
			       (cons :tesselation-evaluation
				     (get-stage-key tesselation-evaluation)))
			     (when geometry
			       (cons :geometry (get-stage-key geometry)))
			     (when fragment
			       (cons :fragment (get-stage-key fragment)))))))
	 post)))))

;;--------------------------------------------------

(defun try-injecting-a-constant (constant-name)
  (assert (constantp constant-name))
  (let ((val (symbol-value constant-name)))
    (typecase val
      (single-float val)
      (double-float val)
      ((signed-byte 32) val)
      ((unsigned-byte 32) val))))

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
    (cepl.space:vec-space 'cepl.space::vec-space-g)
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
