;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; Functions and macros for handling space

(in-package declarative-values)

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

;; (set-dispatch-macro-character #\# #\~
;;    (lambda (stream char1 char2)
;;      (declare (ignorable char1 char2))
;;      (let ((target (read stream nil (values) t)))
;;        (list 'dval target))))

(defun transform-bind (place dvals expr release-on-change)
  (let* ((dvals (or (loop :for v :in dvals :if (utils:find-in-tree v expr)
                       :collect v) 
                    dvals))
         (expr (if (consp expr)
                   (loop :for dval in dvals :do
                      (setf expr (subst `(dval ,dval) dval expr))
                      :finally (return expr))
                   `(dval ,expr)))
         (key (gensym "DVAL-KEY-")))
    `(if (not (loop :for d :in (list ,@dvals) :always (typep d 'declarative-value)))
         (error "The second argument must always be a dval or a list of dvals")
         (let ((setter (let (,@(loop for dval in dvals collect
                                      (list dval dval)))
                           (lambda () 
                             ,(when release-on-change 
                                    `(progn ,@(loop :for dval :in dvals :collect
                                                 `(remhash ',key (bound ,dval))))) 
                             (setf ,place ,expr)))))
           ,@(loop :for dval :in dvals :collect 
                `(setf (gethash ',key (bound ,dval)) setter))
           ',key))))

;; [TODO] should be able to tag part to be eval'd now and never again

(defmacro brittle-bind (place dvals &optional expr)
  `(bind ,place ,dvals ,expr t))

(defmacro bind (place dvals &optional expr release-on-change)
  (let ((dvals (if (listp dvals) 
                   (if (eq (first dvals) 'quote) 
                       (error "No need to quote the dvals") 
                       dvals)
                   (list dvals))))    
    (if expr
        (transform-bind place dvals expr release-on-change)
        (if (> (length dvals) 1)
            (error "You cannot bind to multiple declarative-values without and expression to explain how they interact to make a value")
            (transform-bind place dvals (first dvals) release-on-change)))))

(defun unbind (key dval)
  (remhash key (bound dval)))

(defun unbind-all (dval)
  (let ((table (bound dval)))
    (loop :for key :being :the :hash-key :of table :do (remhash key table))))

;; [TODO] The following breaks as 'i' gets lexically bound and at the end of the
;;        loop it is 4. We need to bind the i at the time of the binding, not
;;        the initialisation. We can walk places for non-first, non-quoted 
;;        symbols and cache them with a 'let'.
;;        note: the non-quote is essential
;; (loop for i below (length the-list) 
;;    :do (print i) (bind (nth i the-list) d))
