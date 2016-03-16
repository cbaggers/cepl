(in-package :jungl)
(in-readtable fn:fn-reader)

;;{TODO} Almost everything in here could really benefit from being optimized

(defvar *gpu-func-specs* (make-hash-table :test #'eq))
(defvar *dependent-gpu-functions* (make-hash-table :test #'eq))
(defvar *gpu-program-cache* (make-hash-table :test #'eq))
(defvar *gpu-pipeline-specs* (make-hash-table :test #'eq))


;;--------------------------------------------------

(defclass gpu-func-spec ()
  ((name :initarg :name)
   (in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (actual-uniforms :initarg :actual-uniforms)
   (uniform-transforms :initarg :uniform-transforms)
   (context :initarg :context)
   (body :initarg :body)
   (instancing :initarg :instancing)
   (equivalent-inargs :initarg :equivalent-inargs)
   (equivalent-uniforms :initarg :equivalent-uniforms)
   (doc-string :initarg :doc-string)
   (declarations :initarg :declarations)
   (missing-dependencies :initarg :missing-dependencies :initform nil)
   (cached-compile-results :initform nil)))

(defclass glsl-stage-spec ()
  ((name :initarg :name)
   (in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (outputs :initarg :outputs)
   (context :initarg :context)
   (body-string :initarg :body-string)
   (cached-compile-results :initarg :compiled :initform nil)))

(defun %make-gpu-func-spec (name in-args uniforms context body instancing
                            equivalent-inargs equivalent-uniforms
			    actual-uniforms uniform-transforms
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
		 :uniform-transforms uniform-transforms
                 :doc-string doc-string
                 :declarations declarations
		 :missing-dependencies missing-dependencies))

(defun %make-glsl-stage-spec (name in-args uniforms outputs context body-string
			      compiled)
  (make-instance 'glsl-stage-spec
                 :name name
                 :in-args (mapcar #'listify in-args)
                 :uniforms (mapcar #'listify uniforms)
		 :outputs (mapcar #'listify outputs)
                 :context context
                 :body-string body-string
		 :compiled compiled))

(defmacro with-gpu-func-spec (func-spec &body body)
  `(with-slots (name in-args uniforms actual-uniforms context body instancing
                     equivalent-inargs equivalent-uniforms uniform-transforms
                     doc-string declarations missing-dependencies) ,func-spec
     (declare (ignorable name in-args uniforms actual-uniforms context body
			 instancing equivalent-inargs equivalent-uniforms
                         doc-string declarations missing-dependencies
			 uniform-transforms))
     ,@body))

(defmacro with-glsl-stage-spec (glsl-stage-spec &body body)
  `(with-slots (name in-args uniforms outputs context body-string)
       ,glsl-stage-spec
     (declare (ignorable name in-args uniforms outputs context body-string))
     ,@body))

(defun %serialize-gpu-func-spec (spec)
  (with-gpu-func-spec spec
    `(%make-gpu-func-spec ',name ',in-args ',uniforms ',context ',body
                          ',instancing ',equivalent-inargs ',equivalent-uniforms
			  ',actual-uniforms ',uniform-transforms
                          ,doc-string ',declarations ',missing-dependencies)))

(defun %serialize-glsl-stage-spec (spec)
  (with-glsl-stage-spec spec
    `(%make-glsl-stage-spec
      ',name ',in-args ',uniforms ',outputs ',context ',body-string)))

(defun gpu-func-spec (name &optional error-if-missing)
  (or (gethash name *gpu-func-specs*)
      (when error-if-missing
        (error 'gpu-func-spec-not-found :spec-name name))))

(defun (setf gpu-func-spec) (value name &optional error-if-missing)
  (when (and error-if-missing (null (gethash name *gpu-func-specs*)))
    (error "gpu-func-spec: gpu function ~a not found" name))
  (setf (gethash name *gpu-func-specs*) value))

(defun funcs-that-use-this-func (name)
  (gethash name *dependent-gpu-functions*))

(defun (setf funcs-that-use-this-func) (value name)
  (setf (gethash name *dependent-gpu-functions*) value))

(defun funcs-these-funcs-use (names &optional (include-names t))
  (remove-duplicates
   (append (apply #'concatenate 'list
		  (mapcar #'funcs-this-func-uses names))
	   (when include-names names))
   :from-end t
   :test #'eq))

(defun funcs-this-func-uses (name)
  "Recursivly searches for functions by this function.
Sorts the list of function names by dependency so the earlier
names are depended on by the functions named later in the list"
  (mapcar #'car
          (remove-duplicates
           (sort (%funcs-this-func-uses name) #'> :key #'cdr)
	   :from-end t
           :key #'car)))

(defun %funcs-this-func-uses (name &optional (depth 0))
  (assert (and (symbolp name) (not (keywordp name))))
  (let ((this-func-calls
         (remove nil (map-hash
                      (lambda (k v)
                        (when (member name v)
                          (cons k depth)))
                      *dependent-gpu-functions*))))
    (append this-func-calls
            (apply #'append
                   (mapcar (lambda (x)
                             (%funcs-this-func-uses (car x) (1+ depth)))
                           this-func-calls)))))

(defun pipelines-that-use-this-as-a-stage (name)
  (remove nil
          (map-hash
           (lambda (k v)
             (when (and (typep v 'shader-pipeline-spec)
                        (member name (slot-value v 'stages)))
               k))
           *gpu-pipeline-specs*)))

(defun update-specs-with-missing-dependencies ()
  (map-hash λ(with-gpu-func-spec _1
	       (when missing-dependencies
		 (%test-&-update-spec _1)
		 _))
	    *gpu-func-specs*))

(defun recompile-pipelines-that-use-this-as-a-stage (name)
  "Recompile all pipelines that depend on the named gpu function or any other
   gpu function that depends on the named gpu function. It does this by
   triggering a recompile on all pipelines that depend on this glsl-stage"
  (mapcar (lambda (_)
            (let ((recompile-pipeline-name (recompile-name _)))
	      (when (fboundp recompile-pipeline-name)
		(funcall (symbol-function recompile-pipeline-name)))))
          (pipelines-that-use-this-as-a-stage name)))

;;--------------------------------------------------

(defconstant +cache-last-compile-result+ t)

(defclass shader-pipeline-spec ()
  ((name :initarg :name)
   (stages :initarg :stages)
   (change-spec :initarg :change-spec)
   (context :initarg :context)
   (cached-compile-results :initform nil)))

(defclass compose-pipeline-spec ()
  ((name :initarg :name)
   (pipelines :initarg :pipelines)
   (in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (context :initarg :context)))

(defun make-shader-pipeline-spec (name stages change-spec context)
  (make-instance 'shader-pipeline-spec :name name :stages stages
                 :change-spec change-spec :context context))
(defun make-compose-pipeline-spec (name pipelines in-args uniforms context)
  (make-instance 'compose-pipeline-spec :name name :pipelines pipelines
                 :in-args in-args :uniforms uniforms :context context))

(defun pipeline-spec (name)
  (gethash name *gpu-pipeline-specs*))

(defun (setf pipeline-spec) (value name)
  (setf (gethash name *gpu-pipeline-specs*) value))

(defun update-pipeline-spec (spec)
  (setf (pipeline-spec (slot-value spec 'name)) spec))

(defun add-compile-results-to-pipeline (name compiled-results)
  (setf (slot-value (pipeline-spec name) 'cached-compile-results)
        compiled-results))

(defvar +pull*-g-not-enabled-message+
  "CEPL has been set to not cache the results of pipeline compilation.
See the +cache-last-compile-result+ constant for more details")

(defvar +pull-g-not-cached-template+
  "Either ~s is not a pipeline/gpu-function or the code for this asset
has not been cached yet")

(defmethod pull1-g ((asset-name symbol))
  (if +cache-last-compile-result+
      (or (slot-value (or (pipeline-spec asset-name)
			  (gpu-func-spec asset-name))
		      'cached-compile-results)
	  (%pull-g-soft-message asset-name))
      +pull*-g-not-enabled-message+))

(defmethod pull-g ((asset-name symbol))
  (if +cache-last-compile-result+
      (cond
	((pipeline-spec asset-name)
	 (let ((p (slot-value (pipeline-spec asset-name)
			      'cached-compile-results)))
	   (if p
	       (mapcar #'varjo:glsl-code p)
	       (%pull-g-soft-message asset-name))))
	((gpu-func-spec asset-name)
	 (let ((ast (slot-value (gpu-func-spec asset-name)
				'cached-compile-results)))
	   (if ast
	       (ast->code ast)
	       (%pull-g-soft-message asset-name))))
	(t (%pull-g-soft-message asset-name)))
      +pull*-g-not-enabled-message+))

(defun %pull-g-soft-message (asset-name)
  (format nil +pull-g-not-cached-template+ asset-name))

;;--------------------------------------------------

(defun request-program-id-for (name)
  (or (gethash name *gpu-program-cache*)
      (setf (gethash name *gpu-program-cache*)
            (gl:create-program))))

;;;--------------------------------------------------------------
;;; PIPELINE ;;;
;;;----------;;;

(defmacro defpipeline (name args gpu-pipe-form &body options)
  (assert (eq (first gpu-pipe-form) 'G->))
  (let* ((gpipe-args (rest gpu-pipe-form)))
    (assert (not (null gpipe-args)))
    (cond ((and (listp (first gpipe-args)) (eq (caar gpipe-args) 'function))
           (%defpipeline-gfuncs name args gpipe-args options))
          ((listp (first gpipe-args))
           (%defpipeline-compose name args options gpipe-args))
          (t (error "Invalid defpipeline syntax")))))

(defun ensure-no-name-collision ()
  )

;;--------------------------------------------------

(defun parse-options (pipeline-name options context)
  (labels ((tokenp (x)
             (and (symbolp x)
                  (or (keywordp x)
                      (char= (aref (symbol-name x) 0)
                             #\&)))))
    (let* ((result
            (mapcar #'cons
                    (cons nil (remove-if-not #'tokenp options))
                    (split-sequence-if #'tokenp options)))
           (result (if (equal (first result) '(nil))
                       (rest result)
                       result)))
      (assert-valid-options pipeline-name result context)
      result)))

;;--------------------------------------------------

(let ((stage-names '((:vertex . :vertex-shader)
                     (:fragment . :fragment-shader)
                     (:geometry . :geometry-shader)
                     (:compute . :compute-shader)
                     (:tesselation-evaluation . :tess-evaluation-shader)
                     (:tesselation-control . :tess-control-shader))))
  (defun varjo->gl-stage-names (stage-name)
    (or (cdr (assoc stage-name stage-names))
        (error "JUNGL: ~a is not a known type of shader stage" stage-name))))

;;--------------------------------------------------

(defvar |*instance-count*| 0)
(defmacro with-instances (count &body body)
  `(let ((|*instance-count*| ,count))
     (unless (> |*instance-count*| 0)
       (error "Instance count must be greater than 0"))
     ,@body))

;;--------------------------------------------------

(defun recompile-name (name) (symb-package :jungl '~~- name))

;;--------------------------------------------------

(let ((current-key 0))
  (defun %gen-pass-key () (incf current-key)))

;;--------------------------------------------------

(defmacro g-> (&rest forms)
  (declare (ignore forms))
  (error "Sorry, g-> can currently only be used inside defpipeline"))

;;--------------------------------------------------
