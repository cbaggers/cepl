(in-package :cgl)

(defun %defpipeline-compose (name args options gpipe-args)
  (assert (and (every #'symbolp args) (not (some #'keywordp args))))
  (assoc-bind ((fbos :fbos) (context :context) (post :post))
      (parse-options options)
    (destructuring-bind (pass-forms gpipe-context)
        (parse-gpipe-args gpipe-args)
      (assert (not (and gpipe-context context)))
      (let* ((pipeline-names (mapcar #'second pass-forms))
             (uniforms (collate-uniforms pipeline-names))
             (stream-args (%number-args (collate-args '(test test2))))
             (args (append (apply #'append stream-args) args)))
        `(progn
           (let (,@(when fbos (mapcar #'car fbos))
                 (initd nil))
             (def-compose-dispatch ,name ,args ,uniforms ,context
                                   ,gpipe-args ,pipeline-names ,fbos ,post))
           (def-compose-dummy ,name ,args ,uniforms))))))


(defun fbo-comp-form (form)
  (destructuring-bind (name . make-fbo-args) form
    `(setf ,name (make-fbo ,@make-fbo-args))))

(defun collate-args (pipeline-names)
  (let* ((stages (mapcar λ(slot-value % 'stages)
                         (get-pipeline-specs pipeline-names))))
    (mapcar λ(slot-value (gpu-func-spec (first %)) 'in-args)
            stages)))

(defun %number-args (collated-args)
  (mapcar #'second
          (reverse
           (reduce (lambda (c x)
                     (let ((len (+ (apply #'+ (mapcar #'first c)) (length x))))
                       (cons (list len (loop for i from (or (caar c) 0)
                                          below len collect (symb 'stream i)))
                             c)))
                   collated-args
                   :initial-value nil))))

(defun collate-uniforms (pipeline-names)
  (let* ((stages (mapcat #'(lambda (x) (slot-value x 'stages))
                        (get-pipeline-specs pipeline-names)))
         (uniforms (mapcar #'(lambda (x) (slot-value x 'uniforms))
                           (mapcar #'gpu-func-spec stages)))
         (aggregated-uniforms (when uniforms
                                (reduce #'aggregate-uniforms uniforms))))
    aggregated-uniforms))

(defun get-pipeline-specs (pipeline-names)
  (mapcar #'pipeline-spec pipeline-names))


;;--------------------------------------------------

(defmacro def-compose-dispatch (name args uniforms context
                                gpipe-args pipeline-names fbos post)
  (declare (ignore context))
  `(defun ,(dispatch-func-name name)
       (,@args ,@(when uniforms `(,(cons '&key (mapcar #'first uniforms)))))
     (unless initd
       ,@(mapcar #'fbo-comp-form fbos)
       (setf initd t)
       ,(when post `(funcall ,post))
       ,(when pipeline-names `(assert (collate-uniforms ',pipeline-names))))
     ,@(mapcar #'make-gmap-pass gpipe-args
               (%number-args (collate-args '(test test2))))))

(defun make-gmap-pass (pass-form stream-args)
  (destructuring-bind (fbo call-form &optional options) pass-form
    (declare (ignore options))
    (let* ((func-name (first call-form))           
           (gmap-form `(gmap #',func-name ,@stream-args ,@(rest call-form))))
      (if fbo
          `(with-bind-fbo (,@(listify fbo)) ,gmap-form)
          gmap-form))))

;;--------------------------------------------------

(defmacro def-compose-dummy (name args uniforms)
  (let ((uniform-names (mapcar #'first uniforms)))
    `(defun ,name (,@args ,@(when uniforms `(&key ,@uniform-names)))
       (declare (ignorable ,@uniform-names ,@args))
       (error "Pipelines do not take a stream directly, the stream must be gmap'd over the pipeline"))))
