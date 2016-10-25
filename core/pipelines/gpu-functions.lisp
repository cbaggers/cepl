(in-package :cepl.pipelines)
(in-readtable fn:fn-reader)

;; extract details from args and delegate to %def-gpu-function
;; for the main logic
(defmacro defun-g (name args &body body)
  (defun-g-common name args body nil))

(defmacro defun-g-equiv (name args &body body)
  (defun-g-common name args body t))

(defun defun-g-common (name args body equiv)
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
		;; now the meat
		(%def-gpu-function name in-args uniforms body instancing
				   doc-string declarations equiv context))))

(defun assert-arg-format (gfunc-name x)
  (unless (listp x)
    (error 'gfun-invalid-arg-format :gfun-name gfunc-name :invalid-pair x))
  x)


;;--------------------------------------------------

(defun %def-gpu-function (name in-args uniforms body instancing
                          doc-string declarations equiv context)
  "This is the meat of defun-g. it is broken down as follows:

   [0] makes a gpu-func-spec that will be populated a stored later.

   [1] Adds a external function definition to varjo also make sure it will be
       called on load

   [2] %test-&-update-spec compiles the code to check for errors and log
       dependencies. (this is called at runtime)

   [3] %make-gpu-func-spec is called at expand time to write a lisp function
       with the same signature as the gpu-function. This gives code hinting and
       also a decent error message if you try calling it from the cpu.
       We don't do this when using defun-g-equiv as we want to shadow the lisp
       function.

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
  (let ((spec (%make-gpu-func-spec name in-args uniforms context body instancing
                                   nil nil nil nil doc-string declarations
                                   nil));;[0]
	(valid-glsl-versions (get-versions-from-context context)))
    ;; this gets the functions used in the body of this function
    ;; it is *not* recursive
    (%update-gpu-function-data spec nil nil)
    (varjo::add-external-function name in-args uniforms body
				  valid-glsl-versions);;[1]
    `(progn
       (varjo::add-external-function ',name ',in-args ',uniforms ',body
				     ',valid-glsl-versions);;[1]
       (%test-&-update-spec ,(serialize-gpu-func-spec spec));;[2]
       ,(unless equiv (make-stand-in-lisp-func spec));;[3]
       (%recompile-gpu-function-and-pipelines ,(inject-func-key spec));;[4]
       (update-specs-with-missing-dependencies);;[5]
       ',name)))

(defun get-versions-from-context (context)
  (%sort-versions
   (remove-if-not λ(member _ varjo::*supported-versions*)
		  context)))

(defun %sort-versions (versions)
  (mapcar #'first
	  (sort (mapcar λ(list _ (parse-integer (symbol-name _)))
			versions)
		#'< :key #'second)))

(defun swap-version (glsl-version context)
  (cons glsl-version (remove-if λ(find _ varjo::*supported-versions*) context)))

(defun lowest-suitable-glsl-version (context)
  (let* ((versions (or (get-versions-from-context context)
		       (list (cepl.context::get-best-glsl-version)))))
    (case= (length versions)
      (0 (cepl.context::get-best-glsl-version))
      (otherwise (first versions)))))

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
		   (context (swap-version (lowest-suitable-glsl-version context)
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

(defvar *print-gpu-function-subscriptions* nil)

(defmethod %subscribe-to-gpu-func (func subscribe-to)
  "As the name would suggest this makes one function dependent on another
   It is used by #'%test-&-update-spec via #'%update-gpu-function-data "
  (let ((func (func-key func))
	(subscribe-to (func-key subscribe-to)))
    (assert (not (func-key= func subscribe-to)))
    (symbol-macrolet ((func-specs (funcs-that-use-this-func subscribe-to)))
      (when (and (gpu-func-spec subscribe-to)
		 (not (member func func-specs :test #'func-key=)))
	(when *print-gpu-function-subscriptions*
	  (format t "; func ~s subscribed to ~s~%"
		  (name func)
		  (name subscribe-to)))
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

(defun get-possible-designators-for-name (name)
  (mapcar λ(with-gpu-func-spec _
             (cons name (mapcar #'second in-args)))
          (gpu-func-specs name)))

(defun get-stage-key (stage-designator &optional (error-on-symbol t))
  (cond
    ((and (listp stage-designator) (eq (first stage-designator) 'function))
     (get-stage-key (second stage-designator)))
    ((listp stage-designator)
     (let ((key (new-func-key (first stage-designator) (rest stage-designator))))
       (if (gpu-func-spec key)
	   key
	   (error 'stage-not-found :designator stage-designator))))
    ((symbolp stage-designator)
     (when error-on-symbol
       (error 'symbol-stage-designator
              :designator stage-designator
              :possible-choices (get-possible-designators-for-name
                                 stage-designator))))
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
             (parse-gpipe-args-implicit args)
	     (parse-gpipe-args-explicit args))
	 post)))))

(defun parse-gpipe-args-implicit (args)
  (destructuring-bind (v-key f-key) (validate-stage-names args)
    (list (cons :vertex v-key)
          (cons :fragment f-key))))

(defun parse-gpipe-args-explicit (args)
  (dbind (&key vertex tesselation-control tesselation-evaluation
               geometry fragment) args
    (dbind (v-key tc-key te-key g-key f-key)
        (validate-stage-names (list vertex tesselation-control
                                    tesselation-evaluation
                                    geometry fragment))
      (remove nil
              (list (when vertex
                      (cons :vertex v-key))
                    (when tesselation-control
                      (cons :tesselation-control tc-key))
                    (when tesselation-evaluation
                      (cons :tesselation-evaluation te-key))
                    (when geometry
                      (cons :geometry g-key))
                    (when fragment
                      (cons :fragment f-key)))))))

(defun validate-stage-names (names)
  (let* (invalid
         (valid
          (loop :for name :in names :collect
             (when name
               (let ((sn (get-stage-key name nil)))
                 (if sn
                     sn
                     (progn
                       (push (list name
                                   (get-possible-designators-for-name name))
                             invalid)
                       nil)))))))
    (case= (length invalid)
      (0 valid)
      (1 (let ((fail (first invalid)))
           (error 'symbol-stage-designator
                  :designator (first fail)
                  :possible-choices (second fail))))
      (otherwise (error 'symbol-stage-designators
                        :designator-choice-pairs (reverse invalid))))))

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

;;--------------------------------------------------

(defmethod delete-gpu-function ((gfunc-description null)
				&optional (error-if-missing t))
  (when error-if-missing
    (error 'gpu-func-spec-not-found :name gfunc-description :types nil)))

(defmethod delete-gpu-function ((gfunc-description symbol)
				&optional (error-if-missing t))
  (let ((choices (gpu-functions gfunc-description)))
    (if (= (length choices) 1)
	(delete-gpu-function (first choices) error-if-missing)
	(restart-case (error 'delete-multi-func-error
			     :name gfunc-description
			     :choices choices)
	  (use-value ()
	    (interactive-delete-gpu-function gfunc-description))))))

(defmethod delete-gpu-function ((gfunc-description func-key)
				&optional (error-if-missing t))
  (delete-gpu-function
   (cons (name gfunc-description) (in-args gfunc-description))
   error-if-missing))

(defmethod delete-gpu-function ((gfunc-description list)
				&optional (error-if-missing t))
  (dbind (name . in-arg-types) gfunc-description
    (let* ((func-key (new-func-key name in-arg-types))
	   (spec (gpu-func-spec func-key nil)))
      (if spec
	  (progn
	    (setf *gpu-func-specs* (remove func-key *gpu-func-specs*
					   :test #'func-key= :key #'car))
	    (varjo::delete-external-function name in-arg-types))
	  (when error-if-missing
	    (error 'gpu-func-spec-not-found
		   :name (name func-key)
		   :types (in-args func-key))))
      gfunc-description)))



(defun interactive-delete-gpu-function (name)
  (let ((picked
	 (read-gpu-function-choice
	  "Please choose which of the following functions you wish to delete"
	  name)))
    (when picked
      (format t "~%Deleting ~s" picked)
      (delete-gpu-function picked))))
