(in-package :base-time)

(defclass tcompile-obj ()
  ((code :initarg :code :accessor t-code :initform nil)
   (run-test :initarg :run-test :accessor t-run-test :initform nil)
   (expired-test :initarg :expired-test :accessor t-expired-test :initform nil)
   (local-vars :initarg :local-vars :accessor t-local-vars :initform nil)
   (closed-vars :initarg :closed-vars :accessor t-closed-vars :initform nil)
   (override :initarg :override :accessor t-override :initform nil)))

(defmacro with-t-obj ((&optional postfix) t-obj &body body)
  (unless (if postfix (and (listp postfix) (eq (first postfix) 'quote)) t)
    (error "postfix must be a quoted symbol"))
  (let ((postfix (second postfix))
        (slots '(code run-test expired-test local-vars closed-vars override)))
    `(with-slots ,(loop :for s :in slots :collect
                     `(,(if postfix (symb s '- postfix) s) ,s))
         ,t-obj
       ,@body)))

(defmethod merge-tcompile-obj ((a tcompile-obj) (b tcompile-obj))
  (with-t-obj ('a) a
    (with-t-obj ('b) b
      (make-instance
       'tcompile-obj
       :code (append (copy-tree code-a) (copy-tree code-b))
       :run-test `(and ,(copy-tree run-test-a) ,(copy-tree run-test-b))
       :expired-test `(and ,(copy-tree expired-test-a) ,(copy-tree expired-test-b))
       :local-vars (append (copy-tree local-vars-a) (copy-tree local-vars-b))
       :closed-vars (append (copy-tree closed-vars-a) (copy-tree closed-vars-b))
       :override (append (copy-tree override-a) (copy-tree override-b))))))

(defmacro def-time-condition (name args has-time-override &body body)  
  (let ((args (if has-time-override
                  (subst 'override "OVERRIDE" args
                         :key #'(lambda (x) (when (symbolp x) (symbol-name x)))
                         :test #'equal)
                  args))
        (body (if has-time-override
                  (subst 'override "OVERRIDE" body
                         :key #'(lambda (x) (when (symbolp x) (symbol-name x)))
                         :test #'equal)
                  body)))
    `(defun ,(symbolicate-package :time-syntax name) ,args
       (append (progn ,@body)
               (list ,has-time-override)))))
