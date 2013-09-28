;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; Functions and macros for handling space

(in-package declarative-values)

;; [TODO] Need a constructor that sets the type

(defun make-dval (&optional value) (make-instance 'declarative-value :value value))

(defclass declarative-value ()
  ((dval :initform nil :initarg :value :reader dval)
   (bound :initform (make-hash-table) :accessor bound)
   (bcount :initform 0 :accessor bcount)))

(defmethod print-object ((object declarative-value) stream)
  (format stream "#<DVal: ~a>" (dval object)))

(defmethod (setf dval) (new-val (dval declarative-value))
  (setf (slot-value dval 'dval) new-val)
  (loop :for callback :being :each hash-value :of (bound dval) :do
     (funcall callback))
  new-val)

(defun transform-bind (place dvals expr)
  (print dvals)
  (let* ((dvals (loop :for v :in dvals :if (utils:find-in-tree v expr)
                   :collect v))
         (expr (if (consp expr)
                   (loop :for dval in dvals :do
                      (setf expr (subst `(dval ,dval) dval expr))
                      :finally (return expr))
                   `(dval ,expr))))
    `(if (not (loop :for d :in (list ,@dvals) :always (typep d 'declarative-value)))
         (error "The second argument must always be a dval or a list of dvals")
         (let ((setter (let (,@(loop for dval in dvals collect
                                    (list dval dval)))                       
                         (lambda () (setf ,place ,expr)))))
           (setf ,place ,expr)
           (list ,@(loop :for dval :in dvals :collect 
                      `(let ((key (incf (bcount ,dval))))
                         (setf (gethash key (bound ,dval)) setter)
                         key)))))))

(defmacro bind (place dvals &optional expr)
  (let ((dvals (if (listp dvals) 
                   (if (eq (first dvals) 'quote) 
                       (error "No need to quote the dvals") 
                       dvals)
                   (list dvals))))    
    (if expr
        (transform-bind place dvals expr)
        (if (> (length dvals) 1)
            (error "You cannot bind to multiple declarative-values without and expression to explain how they interact to make a value")
            (transform-bind place dvals (first dvals))))))
