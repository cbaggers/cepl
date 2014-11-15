(in-package :cgl)

;; needed by varjo
(defclass l-type (v-type) ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gl-equivalent-type-data* (make-hash-table)))

(defmacro def-gl-equivalent (lisp-type &body body)
  (unless lisp-type (error 'null-lisp-type))
  (let ((bridge-name (symbol-to-bridge-symbol lisp-type)))
    (multiple-value-bind (spec shader-forms)
       (if (symbolp (first body))
           (apply #'process-direct lisp-type body)
           (process-compound lisp-type bridge-name body))
     `(progn (eval-when (:compile-toplevel :load-toplevel :execute)     
               (setf (gethash ',lisp-type *gl-equivalent-type-data*) ',spec)
               (defclass ,bridge-name (l-type) ())
               ,@shader-forms)))))

(defun symbol-to-bridge-symbol (x)
  (symb (package-name (symbol-package x)) '- (symbol-name x)))

(defun process-direct (lisp-type &key type converter)
  (unless type (error 'direct-type-nil))
  (values `(:direct ,lisp-type ,type ,converter)
          nil))

(defun process-compound (lisp-type bridge-name fields)
  (values
   `(:compound
     ,lisp-type
     ,(loop :for f :in fields :collect
         (destructuring-bind (name &key type converter) f
           `(,name 
             ,type
             ,(if converter
                  (if (valid-convertorp converter)
                      converter
                      (error 'invalid-converter-form :form converter))
                  `(function ,name))))))
   (loop :for (name &key type converter) :in fields :collect
      (if type
          `(v-defun ,name (x) ,(format nil "~~a-~a" name) (,bridge-name)
                    ,type :glsl-spec-matching nil)          
          (if converter
              (error 'converter-for-nil-field)
              `(v-defun ,name (x) 
                 ,(format nil "<invalid lisp field access>~~a-~a" name)
                 (,bridge-name) ,type :glsl-spec-matching nil))))))


(defun resolve-var-name (x &optional top)
  (cond ((symbolp x) x)
        ((and (= 2 (length x)) (first x) (symbolp (first x)) (listp x))
         (symb (resolve-var-name (cadr x) (if (null top) x top)) '- (first x)))
        (t (error 'invalid-accessor-for-arg :code (or top x)))))


(defun valid-type (x)
  (handler-case (progn (typep t x) t) (error () nil)))

(defun valid-convertorp (x)
  (and (listp x)
       (let ((y (first x)))
         (or (eq y 'function)
             (eq y 'lambda)))))

(deferror null-lisp-type (:prefix "CEPL: Equivalent Types") ()
    "lisp-type may not be null")

(deferror direct-type-nil (:prefix "CEPL: Equivalent Types") ()
    "Field type may be nil in the fields of a compound~%equivalent type but not in this case as it is a direct equivalentB type~%")

(deferror invalid-converter-form (:prefix "CEPL: Equivalent Types") (form)
    "Invalid form for converter, must be sharp-quoted function or lambda:~%~s" 
  form)

(deferror converter-for-nil-field (:prefix "CEPL: Equivalent Types") ()
    "Invalid form for converter, must be sharp-quoted function or lambda")

(deferror invalid-accessor-for-arg (:prefix "Varjo") (code)
    "Varjo: Accessor is not valid for this argument:~%~a" code)
