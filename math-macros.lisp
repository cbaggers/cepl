;; This package hopes to deal with two issues:

;; 1. Many of the functions used by vectors and matrices of 
;; different dimensions are idential except for the number of 
;; componants they are operating over. 
;; 2. We are having to write some ugly functions because we need
;; them to be fast, this is risky and it would be cleaner to 
;; define them as loops

;; The solution is to use macros to generate the ugly versions of
;; the functions at read-time. This will also abstract away the 
;; differences in dimensions wherever possible.

;; THESE SHOULD NOT BE USED OUTSIDE OF THE MATHS PACKAGES
;; THEY ARE NOT NECESSARILY GOOD LISP AND THEIR USE SHOULD 
;; BE AS MEASURED AS POSSIBLE

(in-package :math-macros)

;----------------------------------------------------------------

;; make vector should proably be left as it isnt much work and
;; does things a little differently as its the basic creation
;; function (also dont swap out aref for v-x as it messes with
;; the image of the function

;; i've left the creation of the parameters also..I'm not sure
;; i'm going to keep them as they are...also it makes more 
;; sense for the reader I think.

;----------------------------------------------------------------

;; These have been defined as macros as it want to guarantee they
;; are 'inlined' as it were and also to avoid any possible cost 
;; of the implied let. THe 'implied let' thing is an assumption 
;; on my part from something I read in "ansi common lisp" 
;; (declaim (inline single?))
;; (defun single? (lst)
;;   (and (consp lst) (null (cdr lst))))
;; (defun foo (x)
;;   (single? (bar x)))
;; is equivilent to 
;; (defun foo (x)
;;   (let ((lst (bar x)))
;;     (and (consp lst) (bull (cdr lst)))))
;; Final justification is that it's purely for syntatic clarity
;; and not for any computational reason.
(defmacro v-x (vec)
  "Returns the x component of the vector"
  `(aref ,vec 0))

(defmacro v-y (vec)
  "Returns the y component of the vector"
  `(aref ,vec 1))

(defmacro v-z (vec)
  "Returns the z component of the vector"
  `(aref ,vec 2))

(defmacro v-w (vec)
  "Returns the w component of the vector"  
  `(aref ,vec 3))

;----------------------------------------------------------------

;; 
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

