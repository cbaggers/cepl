(in-package #:cepl.events)

;;--------------------------------------------
;; Root event cell type

(cells:defmodel event-cell () 
  ((event :cell :ephemeral :initarg :event :reader event)))

;;--------------------------------------------
;; Helper macros 
(defmacro def-event-node 
    (name (&key parent) predicate-form &body other-slot-specs)
  (unless (or parent (eq predicate-form :in))
    (error "def-event-node: Parent is mandatory for non root nodes"))
  (let ((parent (symb '* parent '*)))
    `(progn 
       (cells:defmodel ,name (event-cell)
         ((cepl.events:event :cell :ephemeral 
                 :initform ,(cond 
                             ((eq :in predicate-form) `(cells:c-in nil))
                             ((null predicate-form) t)
                             (t `(cells:c? 
                                   (when ,(subst parent :parent predicate-form)
                                     (cepl.events:event ,parent))))))
          ,@(subst parent :parent other-slot-specs)))
       (defparameter ,(symb '* name '*) (make-instance ',name)))))

(defmacro undefobserver (slotname &rest args &aux (aroundp (eq :around (first args))))
  (destructuring-bind ((&optional (self-arg 'self) (new-varg 'new-value)
                                  (oldvarg 'old-value)
                                  (oldvargboundp 'old-value-boundp)
                                  (cell-arg 'c))
                       &rest ignore) args
    (declare (ignore ignore))
    `(remove-method
      #'cells:slot-value-observe                    
      (find-method #'cells:slot-value-observe
                   '(#-(or cormanlisp) ,(if aroundp :around 'progn))
                   '((eql ,slotname) 
                     ,(if (listp self-arg) (second self-arg) t)
                     ,(if (listp new-varg) (second new-varg) t)
                     ,(if (listp oldvarg) (second oldvarg) t)
                     ,(if (listp oldvargboundp) (second oldvargboundp) t)
                     ,(if (listp cell-arg) (second cell-arg) t))))))

(defmacro observe ((source &optional (event-var-name (symb 'e))) &body body)
  `(defobserver evt:event ((%node (eql ,source)))
     (let ((,event-var-name (event %node)))
       ,@body)))
