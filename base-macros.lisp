;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; a load of handy macros to use around the place
;; the base-* packages are meant to be 'used' so that 
;; there is no need to write the package name.

(in-package :base-macros)

;;;--------------------------------------------------------------

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;;----------------------------------------------------------------
;;{TODO} Remove this crappy thing
(defmacro apply-across-elements (call array-forms 
			     num-of-elms &body body)
  "This is a helper macro to limit the amount of ugly code 
   in some of the maths libraries. See the following for 2 
   examples of how it is used:
   ;; (apply-across-elements make-vector3 ((vc-a vector-a)) 3
   ;;   (* vc-a b)))

   ;; (MAKE-VECTOR3 (* (AREF VECTOR-A 0) B)
   ;;               (* (AREF VECTOR-A 1) B)
   ;; 	         (* (AREF VECTOR-A 2) B))


   ;; (apply-to-elements make-matrix3 ((vc-a mat-a) 
   ;;                                  (vc-b mat-b)) 9
   ;;   (+ vc-a vc-b))

   ;; (MAKE-MATRIX3 (+ (AREF MAT-A 0) (AREF MAT-B 0))
   ;; 	         (+ (AREF MAT-A 1) (AREF MAT-B 1)) 
   ;; 	         (+ (AREF MAT-A 2) (AREF MAT-B 2))
   ;; 	         (+ (AREF MAT-A 3) (AREF MAT-B 3))
   ;; 	         (+ (AREF MAT-A 4) (AREF MAT-B 4))
   ;; 	         (+ (AREF MAT-A 5) (AREF MAT-B 5))
   ;; 	         (+ (AREF MAT-A 6) (AREF MAT-B 6))
   ;; 	         (+ (AREF MAT-A 7) (AREF MAT-B 7))
   ;; 	         (+ (AREF MAT-A 8) (AREF MAT-B 8)))"
  (labels ((subst-many (el-num tree swap-list) 
	     (if (null swap-list)
		 tree
		 (subst-many el-num 
			     (let* ((form (car swap-list))
				    (old (car form))
				    (new `(aref ,(cadr form) 
						,el-num)))
			       (subst new old tree)) 
			     (cdr swap-list))))
	   (gen-line (&optional (el-num 0))
	     (if (< el-num num-of-elms)
		 (cons (subst-many el-num (car body) array-forms) 
		       (gen-line (+ el-num 1)))
		 nil)))
    `(,call ,@(gen-line))))

;----------------------------------------------------------------
;; Relative values
;;-----------------

;;{TODO} should also support other source types....not sure what those are
(defgeneric update-relative (val))
(defmacro make-relative-type (type &key (dif '-))
  (let* ((type (if (and (listp type) (eq (first type) 'quote))
                   (second type) type))
         (rtype (symb 'relative- type)))
    `(progn 
       (defstruct ,rtype
         (value 0 :type ,type)
         (last-value 0 :type ,type)
         (source (lambda () 0) :type function))

       (defmethod update-relative ((val ,rtype))
         (let ((cval (funcall (,(symb rtype '-source) val)))
               (lval (,(symb rtype '-last-value) val)))
           (setf (,(symb rtype '-value) val) (,dif cval lval))
           (setf (,(symb rtype '-last-value) val) cval)
           (,(symb rtype '-value) val))))))

(make-relative-type 'integer)
