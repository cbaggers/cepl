(in-package :cgl)

(defun %defpipeline-compose (name args options gpipe-args)
  (assert (and (every #'symbolp args) (not (some #'keywordp args))))
  (assoc-bind ((fbos :fbos) (context :context) (post :post))
      (parse-options options)
    (destructuring-bind (pass-forms gpipe-context)
        (parse-gpipe-args gpipe-args)
      (assert (not (and gpipe-context context)))
      (let ((pipeline-names (mapcar #'get-stage-name pass-forms))
            (uniforms (collate-uniforms pass-forms)))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (update-pipeline-spec
              (make-pipeline-spec ,name ,pipeline-names
                                  (or gpipe-context context))))
           (let (,(mapcar #'car fbos)
                 (initd nil))
             (def-compose-init ,name ,pipeline-names ,fbos ,post)
             (def-compose-dispatch ,name ,args ,uniforms ,pass-forms ,context)
             (def-compose-dummy ,name ,pass-forms)))))))

(defun fbo-comp-form (form)
  (destructuring-bind (name . make-fbo-args) form
    `(setf ,name (make-fbo ,@make-fbo-args))))

(defun collate-uniforms (pipeline-names)
  (let* ((stages (mapcat #'(lambda (x) (slot-value x 'stages))
                        (get-pipeline-specs pipeline-names)))
         (uniforms (mapcar #'(lambda (x) (slot-value x 'uniforms))
                           (mapcar #'gpu-func-spec stages)))
         (aggregated-uniforms (reduce #'aggregate-uniforms uniforms)))
    aggregated-uniforms))

(defun get-pipeline-specs (pipeline-names)
  (mapcar #'pipeline-spec pipeline-names))

;;--------------------------------------------------

(defmacro def-compose-init (name pipeline-names fbos post)
  `(defun ,(init-func-name name) ()
     (unless initd
       ,(mapcar #'fbo-comp-form fbos)
       (setf initd t)
       (funcall ,post))
     (assert (collate-uniforms ',pipeline-names))
     ;; {TODO} need to recompile the pipeline..this
     ;;        goes for shader-pipelines too...damnit
     ))


;;--------------------------------------------------

(defmacro def-compose-dispatch (name args uniforms pass-forms context)
  (declare (ignore context))
  `(defun ,(dispatch-func-name name) (,@args &key ,@(mapcar #'first uniforms))
     ,@(mapcar #'make-gmap-pass pass-forms)))

(defun make-gmap-pass (pass-form)
  (destructuring-bind (fbo call-form &optional options) pass-form
    (declare (ignore options))
    (let ((gmap-form `(gmap #',(first call-form) ,@(rest call-form))))
      (if fbo
          `(with-bind-fbo (,@(listify fbo)) ,gmap-form)
          gmap-form))))

;;--------------------------------------------------

(defmacro def-compose-dummy (name args uniforms)
  (let ((uniform-names (mapcar #'first uniforms)))
    `(defun ,name (,@args &key ,@uniform-names)
       (declare (ignore ,@uniform-names))
       (error "Pipelines do not take a stream directly, the stream must be gmap'd over the pipeline"))))
