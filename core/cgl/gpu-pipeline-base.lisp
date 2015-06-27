(in-package :cgl)

;;{TODO} Almost everything in here could really benefit from being optimized

(defvar *gl-window* nil)
(defvar *gpu-func-specs* (make-hash-table :test #'eq))
(defvar *dependent-gpu-functions* (make-hash-table :test #'eq))
(defvar *gpu-program-cache* (make-hash-table :test #'eq))
(defvar *gpu-pipeline-specs* (make-hash-table :test #'eq))


;;--------------------------------------------------

(defclass gpu-func-spec ()
  ((name :initarg :name)
   (in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (context :initarg :context)
   (body :initarg :body)
   (instancing :initarg :instancing)
   (equivalent-inargs :initarg :equivalent-inargs)
   (equivalent-uniforms :initarg :equivalent-uniforms)
   (doc-string :initarg :doc-string)
   (declarations :initarg :declarations)))

(defun %make-gpu-func-spec (name in-args uniforms context body instancing
                            equivalent-inargs equivalent-uniforms
                            doc-string declarations)
  (make-instance 'gpu-func-spec
                 :name name
                 :in-args (mapcar #'listify in-args)
                 :uniforms (mapcar #'listify uniforms)
                 :context context
                 :body body
                 :instancing instancing
                 :equivalent-inargs equivalent-inargs
                 :equivalent-uniforms equivalent-uniforms
                 :doc-string doc-string
                 :declarations declarations))

(defmacro with-gpu-func-spec (func-spec &body body)
  `(with-slots (name in-args uniforms context body instancing
                     equivalent-inargs equivalent-uniforms
                     doc-string declarations) ,func-spec
     (declare (ignorable name in-args uniforms context body instancing
                         equivalent-inargs equivalent-uniforms
                         doc-string declarations))
     ,@body))

(defun %serialize-gpu-func-spec (spec)
  (with-gpu-func-spec spec
    `(%make-gpu-func-spec ',name ',in-args ',uniforms ',context ',body
                          ',instancing ',equivalent-inargs ',equivalent-uniforms
                          ,doc-string ',declarations)))

(defun gpu-func-spec (name &optional error-if-missing)
  (or (gethash name *gpu-func-specs*)
      (when error-if-missing
        (error "gpu-func-spec: gpu function ~a not found" name))))

(defun (setf gpu-func-spec) (value name &optional error-if-missing)
  (when (and error-if-missing (null (gethash name *gpu-func-specs*)))
    (error "gpu-func-spec: gpu function ~a not found" name))
  (setf (gethash name *gpu-func-specs*) value))

(defun funcs-that-use-this-func (name)
  (gethash name *dependent-gpu-functions*))

(defun (setf funcs-that-use-this-func) (value name)
  (setf (gethash name *dependent-gpu-functions*) value))

(defun funcs-this-func-uses (name)
  "Recursivly searches for functions by this function.
Sorts the list of function names by dependency so the earlier
names are depended on by the functions named later in the list"
  (mapcar #'car
          (remove-duplicates
           (sort (%funcs-this-func-uses name) #'> :key #'cdr)
           :key #'car :from-end t)))

(defun %funcs-this-func-uses (name &optional (depth 0))
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

(defun pipelines-that-use-this-func (name)
  (remove nil
          (map-hash
           (lambda (k v)
             (when (and (typep v 'shader-pipeline-spec)
                        (member name (slot-value v 'stages)))
               k))
           *gpu-pipeline-specs*)))

;;--------------------------------------------------

(defconstant +cache-last-pipeline-compile-result+ t)

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

(defmethod pull1-g ((asset-name symbol))
  (if +cache-last-pipeline-compile-result+
      (slot-value (pipeline-spec asset-name) 'cached-compile-results)
      "CEPL has been set to not cache the results of pipeline compilation.
See the +cache-last-pipeline-compile-result+ constant for more details"))

(defmethod pull-g ((asset-name symbol))
  (if +cache-last-pipeline-compile-result+
      (mapcar #'varjo:glsl-code
              (slot-value (pipeline-spec asset-name) 'cached-compile-results))
      "CEPL has been set to not cache the results of pipeline compilation.
See the +cache-last-pipeline-compile-result+ constant for more details"))

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

(defun parse-options (options)
  (mapcar #'cons
          (cons nil (remove-if-not #'keywordp options))
          (split-sequence-if #'keywordp options)))

;;--------------------------------------------------

(let ((stage-names '((:vertex . :vertex-shader)
                     (:fragment . :fragment-shader)
                     (:geometry . :geometry-shader)
                     (:compute . :compute-shader)
                     (:tesselation-evaluation . :tess-evaluation-shader)
                     (:tesselation-control . :tess-control-shader))))
  (defun varjo->gl-stage-names (stage-name)
    (or (cdr (assoc stage-name stage-names))
        (error "CGL: ~a is not a known type of shader stage" stage-name))))

;;--------------------------------------------------

(defvar |*instance-count*| 0)
(defmacro with-instances (count &body body)
  `(let ((|*instance-count*| ,count))
     (unless (> |*instance-count*| 0)
       (error "Instance count must be greater than 0"))
     ,@body))

;;--------------------------------------------------

(defun recompile-name (name) (symb-package :cgl '~~- name))

;;--------------------------------------------------

(let ((current-key 0))
  (defun %gen-pass-key () (incf current-key)))

;;--------------------------------------------------

(defmacro g-> (&rest forms)
  (declare (ignore forms))
  (error "Sorry, g-> can currently only be used inside defpipeline"))
