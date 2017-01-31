(in-package :cepl.pipelines)
(in-readtable fn:fn-reader)

;;--------------------------------------------------

(defvar *gpu-func-specs* nil)
(defvar *dependent-gpu-functions* nil)
(defvar *gpu-pipeline-specs* (make-hash-table :test #'eq))

;;--------------------------------------------------

(defclass lambda-pipeline-spec ()
  ((cached-compile-results :initarg :cached-compile-results :initform nil)))

(defclass pipeline-spec ()
  ((name :initarg :name)
   (context :initarg :context)
   (cached-compile-results :initform nil)
   (vertex-stage :initarg :vertex-stage)
   (tesselation-control-stage
    :initarg :tesselation-control-stage :initform nil)
   (tesselation-evaluation-stage
    :initarg :tesselation-evaluation-stage :initform nil)
   (geometry-stage
    :initarg :geometry-stage :initform nil)
   (fragment-stage
    :initarg :fragment-stage :initform nil)))

(defclass gpu-func-spec ()
  ((name :initarg :name)
   (in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (actual-uniforms :initarg :actual-uniforms)
   (context :initarg :context)
   (body :initarg :body)
   (instancing :initarg :instancing)
   (equivalent-inargs :initarg :equivalent-inargs)
   (equivalent-uniforms :initarg :equivalent-uniforms)
   (doc-string :initarg :doc-string)
   (declarations :initarg :declarations)
   (missing-dependencies :initarg :missing-dependencies :initform nil)
   (cached-compile-results :initarg :compiled :initform nil)))

(defmethod pipeline-stages ((spec pipeline-spec))
  (with-slots (vertex-stage
               tesselation-control-stage
               tesselation-evaluation-stage
               geometry-stage
               fragment-stage) spec
    (list vertex-stage
          tesselation-control-stage
          tesselation-evaluation-stage
          geometry-stage
          fragment-stage)))

(defmethod pipeline-stage-pairs ((spec pipeline-spec))
  (with-slots (vertex-stage
               tesselation-control-stage
               tesselation-evaluation-stage
               geometry-stage
               fragment-stage) spec
    (remove-if-not
     #'cdr
     (list (cons :vertex vertex-stage)
           (cons :tesselation-control tesselation-control-stage)
           (cons :tesselation-evaluation tesselation-evaluation-stage)
           (cons :geometry geometry-stage)
           (cons :fragment fragment-stage)))))

;;--------------------------------------------------

(defclass glsl-stage-spec (gpu-func-spec) ())

(defun %make-gpu-func-spec (name in-args uniforms context body instancing
                            equivalent-inargs equivalent-uniforms
			    actual-uniforms
                            doc-string declarations missing-dependencies)
  (make-instance 'gpu-func-spec
                 :name name
                 :in-args (mapcar #'listify in-args)
                 :uniforms (mapcar #'listify uniforms)
                 :context context
                 :body body
                 :instancing instancing
                 :equivalent-inargs equivalent-inargs
                 :equivalent-uniforms equivalent-uniforms
		 :actual-uniforms actual-uniforms
                 :doc-string doc-string
                 :declarations declarations
		 :missing-dependencies missing-dependencies))

(defun %make-glsl-stage-spec (name in-args uniforms context body-string
			      compiled)
  (let ((uniforms (mapcar #'listify uniforms)))
    (make-instance 'glsl-stage-spec
		   :name name
		   :in-args (mapcar #'listify in-args)
		   :uniforms uniforms
		   :context context
		   :body body-string
		   :compiled compiled
		   :instancing nil
		   :equivalent-inargs nil
		   :equivalent-uniforms nil
		   :actual-uniforms uniforms
		   :doc-string nil
		   :declarations nil
		   :missing-dependencies nil)))

(defmacro with-gpu-func-spec (func-spec &body body)
  `(with-slots (name in-args uniforms actual-uniforms context body instancing
                     equivalent-inargs equivalent-uniforms doc-string
                     declarations missing-dependencies) ,func-spec
     (declare (ignorable name in-args uniforms actual-uniforms context body
			 instancing equivalent-inargs equivalent-uniforms
                         doc-string declarations missing-dependencies))
     ,@body))

(defmacro with-glsl-stage-spec (glsl-stage-spec &body body)
  `(with-slots (name in-args uniforms outputs context body
		     (compiled cached-compile-results))
       ,glsl-stage-spec
     (declare (ignorable name in-args uniforms outputs context compiled))
     ,@body))

(defmethod make-load-form ((spec gpu-func-spec) &optional environment)
  (declare (ignore environment))
  (with-gpu-func-spec spec
    `(%make-gpu-func-spec
      ',name ',in-args ',uniforms ',context ',body
      ',instancing ',equivalent-inargs ',equivalent-uniforms
      ',actual-uniforms
      ,doc-string ',declarations ',missing-dependencies)))

(defun clone-stage-spec (spec &key new-name new-in-args new-uniforms new-context
				new-body new-instancing new-equivalent-inargs
				new-equivalent-uniforms	new-actual-uniforms
				new-doc-string new-declarations
                                new-missing-dependencies)
  (with-gpu-func-spec spec
    (make-instance
     (etypecase spec
       (gpu-func-spec 'gpu-func-spec)
       (glsl-stage-spec 'glsl-stage-spec))
     :name (or name new-name)
     :in-args (or in-args new-in-args)
     :uniforms (or uniforms new-uniforms)
     :context (or context new-context)
     :body (or body new-body)
     :instancing (or instancing new-instancing)
     :equivalent-inargs (or equivalent-inargs new-equivalent-inargs)
     :equivalent-uniforms (or equivalent-uniforms new-equivalent-uniforms)
     :actual-uniforms (or actual-uniforms new-actual-uniforms)
     :doc-string (or doc-string new-doc-string)
     :declarations (or declarations new-declarations)
     :missing-dependencies (or missing-dependencies
			       new-missing-dependencies))))

;;--------------------------------------------------

(defclass func-key ()
  ((name :initarg :name :reader name)
   (in-arg-types :initarg :types :reader in-args)))

(defmethod func-key->name ((key func-key))
  (cons (name key) (in-args key)))

(defmethod make-load-form ((key func-key) &optional environment)
  (declare (ignore environment))
  `(new-func-key ',(name key) ',(in-args key)))

(defun new-func-key (name in-args-types)
  (make-instance
   'func-key
   :name name
   :types in-args-types))

(defmethod print-object ((obj func-key) stream)
  (format stream "#<GPU-FUNCTION (~s~{ ~s~})>"
	  (name obj) (in-args obj)))

(defmethod func-key ((spec gpu-func-spec))
  (new-func-key (slot-value spec 'name)
		(mapcar #'second (slot-value spec 'in-args))))

(defmethod func-key ((spec varjo:external-function))
  (new-func-key (varjo:name spec)
		(mapcar #'second (varjo:in-args spec))))

(defmethod func-key ((key func-key))
  key)

(defmethod spec->func-key ((spec gpu-func-spec))
  (new-func-key (slot-value spec 'name)
                (mapcar #'second (slot-value spec 'in-args))))

(defmethod spec->func-key ((spec func-key))
  spec)

(defmethod func-key= ((x func-key) (y func-key))
  (unless (or (null x) (null y))
    (and (eq (name x) (name y))
	 (equal (in-args x) (in-args y)))))

(defmethod func-key= (x (y func-key))
  (unless (or (null x) (null y))
    (func-key= (func-key x) y)))

(defmethod func-key= ((x func-key) y)
  (unless (or (null x) (null y))
    (func-key= x (func-key y))))

(defmethod func-key= (x y)
  (unless (or (null x) (null y))
    (func-key= (func-key x) (func-key y))))

;;--------------------------------------------------

(defmethod gpu-func-spec (key &optional error-if-missing)
  (gpu-func-spec (func-key key) error-if-missing))

(defmethod gpu-func-spec ((func-key func-key) &optional error-if-missing)
  (or (assocr func-key *gpu-func-specs* :test #'func-key=)
      (when error-if-missing
        (error 'gpu-func-spec-not-found
	       :name (name func-key)
	       :types (in-args func-key)))))

(defmethod (setf gpu-func-spec) (value key &optional error-if-missing)
  (setf (gpu-func-spec (func-key key) error-if-missing) value))

(defmethod (setf gpu-func-spec) (value (key func-key) &optional error-if-missing)
  (when error-if-missing
    (gpu-func-spec key t))
  (setf *gpu-func-specs*
	(remove-duplicates (acons key value *gpu-func-specs*)
			   :key #'car :test #'func-key=
			   :from-end t)))

(defun gpu-func-specs (name &optional error-if-missing)
  (or (remove nil
	      (mapcar λ(dbind (k . v) _
			 (when (eq (name k) name)
			   v))
		      *gpu-func-specs*))
      (when error-if-missing
        (error 'gpu-func-spec-not-found :spec-name name))))

(defmethod %unsubscibe-from-all (spec)
  (%unsubscibe-from-all (func-key spec)))

(defmethod %unsubscibe-from-all ((func-key func-key))
  "As the name would suggest this removes one function's dependency on another
   It is used by #'%test-&-update-spec via #'%update-gpu-function-data"
  (labels ((%remove-gpu-function-from-dependancy-table (pair)
	     (dbind (key . dependencies) pair
	       (when (member func-key dependencies :test #'func-key=)
		 (setf (funcs-that-use-this-func key)
		       (remove func-key dependencies :test #'func-key=))))))
    (map nil #'%remove-gpu-function-from-dependancy-table
	 *dependent-gpu-functions*)))

(defmethod funcs-that-use-this-func (key)
  (funcs-that-use-this-func (func-key key)))

(defmethod funcs-that-use-this-func ((key func-key))
  (assocr key *dependent-gpu-functions*
	  :test #'func-key=))

(defmethod (setf funcs-that-use-this-func) (value key)
  (setf (funcs-that-use-this-func (func-key key)) value))

(defmethod (setf funcs-that-use-this-func) (value (key func-key))
  (setf *dependent-gpu-functions*
	(remove-duplicates (acons key value *dependent-gpu-functions*)
			   :key #'car :test #'func-key=
			   :from-end t)))

(defun funcs-these-funcs-use (names &optional (include-names t))
  (remove-duplicates
   (append (apply #'concatenate 'list
		  (mapcar #'funcs-this-func-uses names))
	   (when include-names names))
   :from-end t
   :test #'eq))

(defun funcs-this-func-uses (key)
  "Recursivly searches for functions by this function.
Sorts the list of function names by dependency so the earlier
names are depended on by the functions named later in the list"
  (mapcar #'car
          (remove-duplicates
           (sort (%funcs-this-func-uses key) #'> :key #'cdr)
	   :from-end t
           :key #'car)))

(defmethod %funcs-this-func-uses ((key func-key) &optional (depth 0))
  (let ((this-func-calls
         (remove nil (mapcar
                      λ(dbind (k . v) _
			 (when (member key v)
			   (cons k depth)))
                      *dependent-gpu-functions*))))
    (append this-func-calls
            (apply #'append
                   (mapcar λ(%funcs-this-func-uses (car _) (1+ depth))
                           this-func-calls)))))

(defmethod pipelines-that-use-this-as-a-stage ((func-key func-key))
  (remove-duplicates
   (remove nil
           (map-hash
            (lambda (k v)
              (when (and (symbolp k)
                         (typep v 'pipeline-spec)
                         (member func-key (pipeline-stages v) :test #'func-key=))
                k))
            *gpu-pipeline-specs*))
   :test #'eq))

(defun update-specs-with-missing-dependencies ()
  (map 'nil λ(dbind (k . v) _
	       (with-gpu-func-spec v
		 (when missing-dependencies
		   (%test-&-update-spec v)
		   k)))
       *gpu-func-specs*))

(defmethod recompile-pipelines-that-use-this-as-a-stage ((key func-key))
  "Recompile all pipelines that depend on the named gpu function or any other
   gpu function that depends on the named gpu function. It does this by
   triggering a recompile on all pipelines that depend on this glsl-stage"
  (mapcar λ(let ((recompile-pipeline-name (recompile-name _)))
	     (when (fboundp recompile-pipeline-name)
	       (funcall (symbol-function recompile-pipeline-name))))
          (pipelines-that-use-this-as-a-stage key)))

(defmethod %gpu-function ((name symbol))
  (let ((choices (gpu-functions name)))
    (case= (length choices)
      (0 (error 'gpu-func-spec-not-found :name name :types nil))
      (1 (%gpu-function (first choices)))
      (otherwise (restart-case
		     (error 'multi-func-error :name name :choices choices)
		   (use-value ()
		     (%gpu-function (interactive-pick-gpu-function name))))))))

(defmethod %gpu-function ((name null))
  (error 'gpu-func-spec-not-found :name name :types nil))

(defmethod %gpu-function ((name list))
  (dbind (name . in-arg-types) name
    (let ((key (new-func-key name in-arg-types)))
      (if (gpu-func-spec key)
	  key
	  (error 'gpu-func-spec-not-found :name name :types in-arg-types)))))

(defmacro gpu-function (name)
  (%gpu-function name))

(defun gpu-functions (name)
  (mapcar λ(cons (slot-value _ 'name)
		 (mapcar #'second (slot-value _ 'in-args)))
	  (gpu-func-specs name)))

(defun read-gpu-function-choice (intro-text gfunc-name)
  (let ((choices (gpu-functions gfunc-name)))
    (when choices
      (format t "~%~a~{~%~a: ~a~}~%Choice: "
	      intro-text
	      (mapcat #'list (alexandria:iota (length choices)) choices))
      (let ((choice (read-integers)))
	(if (= 1 (length choice))
	    (elt choices (first choice))
	    nil)))))

(defun interactive-pick-gpu-function (name)
  (read-gpu-function-choice
   "Please choose which of the following functions you wish to use"
   name))

;;--------------------------------------------------

(defvar +cache-last-compile-result+ t)

(defun make-lambda-pipeline-spec (compiled-stages)
  (make-instance 'lambda-pipeline-spec
                 :cached-compile-results compiled-stages))

(defun make-pipeline-spec (name stages context)
  (dbind (&key vertex tesselation-control tesselation-evaluation
               geometry fragment) (flatten stages)
    (make-instance 'pipeline-spec
                   :name name
                   :vertex-stage vertex
                   :tesselation-control-stage tesselation-control
                   :tesselation-evaluation-stage tesselation-evaluation
                   :geometry-stage geometry
                   :fragment-stage fragment
                   :context context)))

(defun pipeline-spec (name)
  (gethash name *gpu-pipeline-specs*))

(defun (setf pipeline-spec) (value name)
  (setf (gethash name *gpu-pipeline-specs*) value))

(defun update-pipeline-spec (spec)
  (setf (pipeline-spec (slot-value spec 'name)) spec))

(defun add-compile-results-to-pipeline (name compiled-results)
  (setf (slot-value (pipeline-spec name) 'cached-compile-results)
        compiled-results))

(defun function-keyed-pipeline (func)
  (assert (typep func 'function))
  (let ((name (gethash func *gpu-pipeline-specs*)))
    (when name (pipeline-spec name))))

(defun (setf function-keyed-pipeline) (name func)
  (assert (typep func 'function))
  (assert (symbolp name))
  (setf (gethash func *gpu-pipeline-specs*)
        name))

(defun %pull-spec-common (asset-name)
  (labels ((gfunc-spec (x)
	     (let ((specs (gpu-func-specs x)))
	       (case= (length specs)
		 (0 nil)
		 (1 (first specs))
		 (otherwise :too-many)))))
    (if +cache-last-compile-result+
	(let ((spec (or (pipeline-spec asset-name) (gfunc-spec asset-name))))
	  (typecase spec
	    (null (warn 'pull-g-not-cached :asset-name asset-name))
	    (keyword (let ((alt (pull-g-soft-multi-func-message asset-name)))
		       (when alt
			 (slot-value (gpu-func-spec alt) 'cached-compile-results))))
	    (otherwise (slot-value spec 'cached-compile-results))))
        (error 'pull*-g-not-enabled))))

(defmethod pull1-g ((asset-name symbol))
  (or (%pull-spec-common asset-name)
      (warn 'pull-g-not-cached :asset-name asset-name)))

(defmethod pull1-g ((asset-name list))
  (or (%pull-spec-common asset-name)
      (warn 'pull-g-not-cached :asset-name asset-name)))

(defmethod pull-g ((asset-name symbol))
  (let ((compiled (%pull-spec-common asset-name)))
    (etypecase compiled
      (null (warn 'pull-g-not-cached :asset-name asset-name))
      (string compiled)
      (list (mapcar #'varjo:glsl-code compiled))
      (varjo:varjo-compile-result (glsl-code compiled)))))

(defun pull-g-soft-multi-func-message (asset-name)
  (let ((choices (gpu-functions asset-name)))
    (restart-case
	(error 'multi-func-error :name asset-name :choices choices)
      (use-value ()
	(%gpu-function (interactive-pick-gpu-function asset-name))))))

(defmethod pull-g ((pipeline-func function))
  (let ((pipeline (function-keyed-pipeline pipeline-func)))
    (etypecase pipeline
      (null (warn 'func-keyed-pipeline-not-found
                  :callee 'pull-g :func pipeline-func))
      ((or pipeline-spec lambda-pipeline-spec)
       (let ((compiled (slot-value pipeline 'cached-compile-results)))
         (if compiled
             (mapcar #'varjo:glsl-code compiled)
             (warn 'func-keyed-pipeline-not-found
                   :callee 'pull-g :func pipeline-func)))))))

(defmethod pull1-g ((pipeline-func function))
  (let ((pipeline (function-keyed-pipeline pipeline-func)))
    (etypecase pipeline
      (null (warn 'func-keyed-pipeline-not-found
                  :callee 'pull1-g :func pipeline-func))
      ((or pipeline-spec lambda-pipeline-spec)
       (or (slot-value pipeline 'cached-compile-results)
           (warn 'func-keyed-pipeline-not-found
                  :callee 'pull1-g :func pipeline-func))))))

;;--------------------------------------------------

(defun request-program-id-for (name)
  (with-slots (cepl.context::map-of-pipeline-names-to-gl-ids) *cepl-context*
    (if name
        (or (gethash name cepl.context::map-of-pipeline-names-to-gl-ids)
            (setf (gethash name cepl.context::map-of-pipeline-names-to-gl-ids)
                  (gl:create-program)))
        (gl:create-program))))

;;--------------------------------------------------

(let ((stage-names '((:vertex . :vertex-shader)
                     (:fragment . :fragment-shader)
                     (:geometry . :geometry-shader)
                     (:compute . :compute-shader)
                     (:tesselation-evaluation . :tess-evaluation-shader)
                     (:tesselation-control . :tess-control-shader))))
  (defun varjo->gl-stage-names (stage-name)
    (or (cdr (assoc stage-name stage-names))
        (error "CEPL: ~a is not a known type of shader stage" stage-name))))

;;--------------------------------------------------

(defvar |*instance-count*| 0)
(defmacro with-instances (count &body body)
  `(let ((|*instance-count*| ,count))
     (unless (> |*instance-count*| 0)
       (error "Instance count must be greater than 0"))
     ,@body))

;;--------------------------------------------------

(defun recompile-name (name) (symb-package :cepl '~~- name))

;;--------------------------------------------------

(let ((current-key 0))
  (defun %gen-pass-key () (incf current-key)))

;;--------------------------------------------------

(defmethod free ((function function))
  (warn "CEPL: Free has not yet been implemented for pipelines.
Please bug me to work on this issue: https://github.com/cbaggers/cepl/issues/130"))

;;--------------------------------------------------
