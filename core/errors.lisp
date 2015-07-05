(in-package :cepl)

(defmacro deferror (name parent-condition (&rest args) error-string &body body)
  (unless (every #'symbolp args) (error "can only take simple args"))
  (labels ((kwd (x) (intern (format nil "~a" x) :keyword)))
    (let ((parent-condition (or parent-condition 'error)))
      (loop :for arg :in args :do
         (setf body (subst `(,arg condition) arg body :test #'eq)))
      `(define-condition ,name (,parent-condition)
         (,@(loop :for arg :in args :collect
               `(,arg :initarg ,(kwd arg) :reader ,arg)))
         (:report (lambda (condition stream)
                    (declare (ignorable condition))
                    (format stream ,error-string ,@body)))))))
