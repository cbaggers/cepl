(in-package :cgl)
(named-readtables:in-readtable fn_::fn_lambda)

(defun %defpipeline-compose (name args options gpipe-args)
  (assert (and (every #'symbolp args) (not (some #'keywordp args))))
  (destructuring-bind (args &optional user-uniforms)
      (split-sequence :&uniform args :test #'string-equal)
    (assoc-bind ((fbos :fbos) (context :context) (post :post))
        (parse-options options)
      (destructuring-bind (pass-forms gpipe-context)
          (parse-gpipe-args gpipe-args)
        (assert (not (and gpipe-context context)))
        (let* ((pipeline-names (mapcar #'second pass-forms))
               (uniform-args (make-pipeline-uniform-args
                              pipeline-names
                              (get-overidden-uniforms gpipe-args)))
               (uniforms
                (append (mapcar #'first uniform-args)
                        user-uniforms))
               (stream-args (%number-args (collate-args pipeline-names)))
               (args (append (apply #'append stream-args) args)))
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
                 (update-pipeline-spec
                  (make-compose-pipeline-spec
                   ',name ',pipeline-names ',args ',uniform-args
                   ',(or gpipe-context context))))
             (let (,@(when fbos (mapcar #'car fbos))
                   (initd nil))
               (def-compose-dispatch ,name ,args ,uniforms ,context
                                     ,gpipe-args ,pipeline-names ,fbos ,post))
             (def-compose-dummy ,name ,args ,uniforms)))))))

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

;;--------------------------------------------------

(defmacro def-compose-dispatch (name args uniforms context
                                gpipe-args pipeline-names fbos post)
  (declare (ignore context))
  `(defun ,(dispatch-func-name name)
       (,@args ,@(when uniforms `(&key ,@uniforms)))
     (unless initd
       ,@(mapcar #'fbo-comp-form fbos)
       (setf initd t)
       ,(when post `(funcall ,post)))
     ,@(mapcar #'make-gmap-pass gpipe-args
               (%number-args (collate-args pipeline-names)))))

(defun fbo-comp-form (form)
  (destructuring-bind (name . make-fbo-args) form
    `(setf ,name (make-fbo ,@make-fbo-args))))

(defun make-gmap-pass (pass-form stream-args)
  (destructuring-bind (fbo call-form &optional options) pass-form
    (declare (ignore options))
    (let* ((func-name (first call-form))
           (gmap-form `(gmap #',func-name ,@stream-args ,@(rest call-form)
                             ,@(mapcat #'%uniform-arg-to-call
                                       (get-pipeline-uniforms func-name
                                                              call-form)))))
      (if fbo
          `(with-bind-fbo (,@(listify fbo)) ,gmap-form)
          gmap-form))))

(defun %uniform-arg-to-call (uniform-arg)
  `(,(kwd (first uniform-arg)) ,(first uniform-arg)))

;;--------------------------------------------------

(defmacro def-compose-dummy (name args uniforms)
  `(defun ,name (,@args ,@(when uniforms `(&key ,@uniforms)))
     (declare (ignorable ,@uniforms ,@args))
     (error "Pipelines do not take a stream directly, the stream must be gmap'd over the pipeline")))

;;--------------------------------------------------

(defun collate-args (pipeline-names)
  (mapcar #'%collate-args (get-pipeline-specs pipeline-names)))
(defmethod %collate-args ((spec shader-pipeline-spec))
  (slot-value (gpu-func-spec (first (slot-value spec 'stages))) 'in-args))
(defmethod %collate-args ((spec compose-pipeline-spec))
  (slot-value spec 'in-args))

(defun get-pipeline-specs (pipeline-names)
  (mapcar #'pipeline-spec pipeline-names))

(defun get-overidden-uniforms (pass-forms)
  (let* ((forms (mapcar #'second pass-forms)))
    (mapcar λ(remove-if-not #'keywordp %) forms)))

;;{TODO} handle equivalent types
(defun make-pipeline-uniform-args (pipeline-names overriden-uniforms)
  (let ((all-uniforms
         (mapcat (lambda (uniforms overriden)
                   (loop :for uniform :in uniforms
                      :if (not (member (first uniform) overriden
                                       :test #'string-equal))
                      :collect uniform))
                 (mapcar #'get-pipeline-uniforms pipeline-names)
                 overriden-uniforms)))
    (%aggregate-uniforms all-uniforms)))

(defun get-pipeline-uniforms (pipeline-name &optional call-form)
  (%get-pipeline-uniforms (pipeline-spec pipeline-name) call-form))

(defmethod %get-pipeline-uniforms
    ((pipeline-spec shader-pipeline-spec) call-form)
  (let ((result (aggregate-uniforms (slot-value pipeline-spec 'stages)))
        (overriden-uniforms (remove-if-not #'keywordp call-form)))
    (remove-if λ(member % overriden-uniforms
                        :test (lambda (x y) (string-equal (car x) y)))
               result)))

(defmethod %get-pipeline-uniforms
    ((pipeline-spec compose-pipeline-spec) call-form)
  (let ((result (slot-value pipeline-spec 'uniforms))
        (overriden-uniforms (remove-if-not #'keywordp call-form)))
    (remove-if λ(member % overriden-uniforms
                        :test (lambda (x y) (string-equal (car x) y)))
               result)))
