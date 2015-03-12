(in-package :cgl)

;;{TODO} Almost everything in here could really benefit from being optimized

(defparameter *gl-window* nil)

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
(defmacro with-instances ((count) &body body)
  `(let ((|*instance-count*| ,count))
     (unless (> |*instance-count*| 0)
       (error "Instance count must be greater than 0"))
     ,@body))

;;--------------------------------------------------

(defclass pipeline-spec ()
  ((name :initarg :name)
   (stages :initarg :stages)
   (context :initarg :context)))

(defun make-pipeline-spec (name stages context)
  (make-instance 'pipeline-spec :name name :stages stages :context context))

(defun pipeline-spec (name)
  (gethash name *gpu-pipeline-specs*))

(defun (setf pipeline-spec) (value name)
  (setf (gethash name *gpu-pipeline-specs*) value))

(defun update-pipeline-spec (spec)
  (setf (pipeline-spec (slot-value spec 'name)) spec))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gpu-program-cache* (make-hash-table :test #'eq))
  (defvar *gpu-pipeline-specs* (make-hash-table :test #'eq)))

(defun request-program-id-for (name)
  (or (gethash name *gpu-program-cache*)
      (setf (gethash name *gpu-program-cache*)
            (gl:create-program))))

;; (defmethod gl-pull ((asset-name symbol))
;;   (get-glsl-code asset-name))

;;;--------------------------------------------------------------
;;; PIPELINE ;;;
;;;----------;;;

(defmacro defpipeline (name args gpu-pipe-form &body options)
  (assert (eq (first gpu-pipe-form) 'G->))
  (let* ((gpipe-args (rest gpu-pipe-form)))
    (if (and (listp (first gpipe-args)) (eq (first gpipe-args) 'function))
        (%defpipeline-gfuncs name args gpipe-args options)
        (%defpipeline-compose name args gpipe-args options))))

(defun ensure-no-name-collision ()
  )

;;--------------------------------------------------

(defun parse-options (options)
  (mapcar #'cons
          (remove-if-not #'keywordp options)
          (split-sequence-if #'keywordp options)))

(defun get-stage-name (stage)
  (assert (listp stage))
  (if (eq (first stage) 'function)
      (second stage)
      (first stage)))
