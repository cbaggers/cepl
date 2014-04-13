(in-package :base-time)

(defconstant overflow-sym '|overflow|)
(defconstant current-time-sym '|current-time|)
(defparameter overflow-form `(- ,current-time-sym ,overflow-sym))
(defparameter end-of-time most-positive-fixnum)

(defclass t-compile-obj ()
  ((initialize :initarg :initialize :accessor t-initialize :initform nil)
   (code :initarg :code :accessor t-code :initform nil)
   (run-test :initarg :run-test :accessor t-run-test :initform nil)
   (expired-test :initarg :expired-test :accessor t-expired-test :initform nil)
   (end-time :initarg :end-time :accessor t-end-time :initform end-of-time)
   (local-vars :initarg :local-vars :accessor t-local-vars :initform nil)
   (closed-vars :initarg :closed-vars :accessor t-closed-vars :initform nil)))

(defmacro with-t-obj ((&optional postfix) t-obj &body body)
  (unless (if postfix (and (listp postfix) (eq (first postfix) 'quote)) t)
    (error "postfix must be a quoted symbol"))
  (let ((postfix (second postfix))
        (slots '(code run-test expired-test local-vars closed-vars initialize end-time)))
    `(with-slots ,(loop :for s :in slots :collect
                     `(,(if postfix (symb s '- postfix) s) ,s))
         ,t-obj
       ,@body)))

(defmacro def-time-condition (name args &body body)
  `(defun ,(symbolicate-package :time-syntax name) ,args
     (progn ,@body)))

(defun relaxed-max (&rest values)
  (let ((max 0))
    (loop :for v :in values :if (> v max) :do (setf max v))
    max))

(defun time-syntaxp (name)
  (symbol-function (symbolicate-package :time-syntax name)))

(defun compile-time-syntax (form)
  (let ((name (first form))
        (rest (rest form)))
    (apply (symbol-function (symbolicate-package :time-syntax name)) rest)))

