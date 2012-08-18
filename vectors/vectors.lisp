;; This package provides a generic interface to all the vector
;; functions provided in vector2,3 & 4.
;; It is here to make programming more pleasent.
;; Also see the reader macro for creating vectors in base-math.

;; DO NOT ':USE' THIS PACKAGE IN YOUR PROGRAMS AS IT REDEFINES
;; VARIOUS MATHEMATICAL SYMBOLS

(in-package :vectors)

;----------------------------------------------------------------

;; Reader macro used to make vectors of any size
(set-dispatch-macro-character #\# #\v
   #'(lambda (stream char-a char-b)
       (declare (ignore char-a)
		(ignore char-b))
       (let* ((attrs (loop for i in (read stream t nil t)
			 collect 
			  (if (numberp i)
			      (coerce i 'single-float)
			      `(coerce ,i 'single-float))))
	      (size (cl:length attrs))
	      (command (cepl-utils:symbolicate-package
			(format nil "VECTOR~s" size)
			"MAKE-VECTOR"
			(cepl-utils:mkstr size))))
	 (cons command attrs))))

;----------------------------------------------------------------

(defun make-vector (x y &optional z w)
  "Creates a new vector"
  (cond (w (vector4:make-vector4 x y z w))
	(z (vector3:make-vector3 x y z))
	(t (vector2:make-vector2 x y))))

;----------------------------------------------------------------

(defgeneric veczerop (size vec-a)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod veczerop ((size (eql 2)) vec-a)
  (v2:vzerop vec-a))

(defmethod veczerop ((size (eql 3)) vec-a)
  (v3:vzerop vec-a))

(defmethod veczerop ((size (eql 4)) vec-a)
  (v4:vzerop vec-a))

(defmacro zerop (vec-a)
  (base-macros:once-only (vec-a)
    `(veczerop (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric vecunitp (size vec-a)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecunitp ((size (eql 2)) vec-a)
  (v2:unitp vec-a))

(defmethod vecunitp ((size (eql 3)) vec-a)
  (v3:unitp vec-a))

(defmethod vecunitp ((size (eql 4)) vec-a)
  (v4:unitp vec-a))

(defmacro unitp (vec-a)
  (base-macros:once-only (vec-a)
    `(vecunitp (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric vec= (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec= ((size (eql 2)) vec-a vec-b)
  (v2:v= vec-a vec-b))

(defmethod vec= ((size (eql 3)) vec-a vec-b)
  (v3:v= vec-a vec-b))

(defmethod vec= ((size (eql 4)) vec-a vec-b)
  (v4:v= vec-a vec-b))

(defmacro = (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(vec= (cl:length ,vec-a) ,vec-a ,vec-b)))

;----------------------------------------------------------------

(defgeneric vec/= (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec/= ((size (eql 2)) vec-a vec-b)
  (v2:v/= vec-a vec-b))

(defmethod vec/= ((size (eql 3)) vec-a vec-b)

  (v3:v/= vec-a vec-b))

(defmethod vec/= ((size (eql 4)) vec-a vec-b)
  (v4:v/= vec-a vec-b))

(defmacro /= (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(vec/= (cl:length ,vec-a) ,vec-a ,vec-b)))

;----------------------------------------------------------------

(defgeneric vec+1 (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec+1 ((size (eql 2)) vec-a vec-b)
  (v2:v+1 vec-a vec-b))

(defmethod vec+1 ((size (eql 3)) vec-a vec-b)
  (v3:v+1 vec-a vec-b))

(defmethod vec+1 ((size (eql 4)) vec-a vec-b)
  (v4:v+1 vec-a vec-b))

(defmacro 1+ (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vec+1 (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vec-1 (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec-1 ((size (eql 2)) vec-a vec-b)
  (v2:v-1 vec-a vec-b))

(defmethod vec-1 ((size (eql 3)) vec-a vec-b)
  (v3:v-1 vec-a vec-b))

(defmethod vec-1 ((size (eql 4)) vec-a vec-b)
  (v4:v-1 vec-a vec-b))

(defmacro 1- (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vec-1 (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vec+ (size &rest vecs)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec+ ((size (eql 2)) &rest vecs)
  (apply #'v2:v+ vecs))

(defmethod vec+ ((size (eql 3)) &rest vecs)
  (apply #'v3:v+ vecs))

(defmethod vec+ ((size (eql 4)) &rest vecs)
  (apply #'v4:v+ vecs))

(defmacro + (vec-a &rest vecs)
  (base-macros:once-only (vec-a)
    `(vec+ (cl:length ,vec-a) ,vec-a ,@vecs)))

;----------------------------------------------------------------

(defgeneric vec- (size &rest vecs)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec- ((size (eql 2)) &rest vecs)
  (apply #'v2:v- vecs))

(defmethod vec- ((size (eql 3)) &rest vecs)
  (apply #'v3:v- vecs))

(defmethod vec- ((size (eql 4)) &rest vecs)
  (apply #'v4:v- vecs))

(defmacro - (vec-a &rest vecs)
  (base-macros:once-only (vec-a)
    `(vec- (cl:length ,vec-a) ,vec-a ,@vecs)))

;----------------------------------------------------------------

(defgeneric vec* (size vec-a multiple)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec* ((size (eql 2)) vec-a (multiple number))
  (v2:v* vec-a (coerce multiple 'single-float)))

(defmethod vec* ((size (eql 3)) vec-a (multiple number))
  (v3:v* vec-a (coerce multiple 'single-float)))

(defmethod vec* ((size (eql 4)) vec-a (multiple number))
  (v4:v* vec-a (coerce multiple 'single-float)))

(defmethod vec* ((size (eql 2))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v2:v*vec vec-a multiple))

(defmethod vec* ((size (eql 3))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v3:v*vec vec-a multiple))

(defmethod vec* ((size (eql 4))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v4:v*vec vec-a multiple))

(defmacro * (vec-a scalar-or-vec)
  (base-macros:once-only (vec-a)
    `(vec* (cl:length ,vec-a) ,vec-a ,scalar-or-vec)))

;----------------------------------------------------------------

(defgeneric vec/ (size vec-a multiple)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec/ ((size (eql 2)) vec-a (multiple number))
  (v2:v/ vec-a (coerce multiple 'single-float)))

(defmethod vec/ ((size (eql 3)) vec-a (multiple number))
  (v3:v/ vec-a (coerce multiple 'single-float)))

(defmethod vec/ ((size (eql 4)) vec-a (multiple number))
  (v4:v/ vec-a (coerce multiple 'single-float)))

(defmethod vec/ ((size (eql 2))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v2:v/vec vec-a multiple))

(defmethod vec/ ((size (eql 3))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v3:v/vec vec-a multiple))

(defmethod vec/ ((size (eql 4))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v4:v/vec vec-a multiple))

(defmacro / (vec-a scalar-or-vec)
  (base-macros:once-only (vec-a)
    `(vec/ (cl:length ,vec-a) ,vec-a ,scalar-or-vec)))

;----------------------------------------------------------------

(defgeneric veclength (size vec-a)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod veclength ((size (eql 2)) vec-a)
  (v2:vlength vec-a))

(defmethod veclength ((size (eql 3)) vec-a)
  (v3:vlength vec-a))

(defmethod veclength ((size (eql 4)) vec-a)
  (v4:vlength vec-a))

(defmacro length (vec-a)
  (base-macros:once-only (vec-a)
    `(veclength (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric veclength-squared (size vec-a)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod veclength-squared ((size (eql 2)) vec-a)
  (v2:vlength-squared vec-a))

(defmethod veclength-squared ((size (eql 3)) vec-a)
  (v3:vlength-squared vec-a))

(defmethod veclength-squared ((size (eql 4)) vec-a)
  (v4:vlength-squared vec-a))

(defmacro length-squared (vec-a)
  (base-macros:once-only (vec-a)
    `(veclength-squared (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric vecdistance (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecdistance ((size (eql 2)) vec-a vec-b)
  (v2:distance vec-a vec-b))

(defmethod vecdistance ((size (eql 3)) vec-a vec-b)
  (v3:distance vec-a vec-b))

(defmethod vecdistance ((size (eql 4)) vec-a vec-b)
  (v4:distance vec-a vec-b))

(defmacro distance (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecdistance (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecdistance-squared (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecdistance-squared ((size (eql 2)) vec-a vec-b)
  (v2:distance-squared vec-a vec-b))

(defmethod vecdistance-squared ((size (eql 3)) vec-a vec-b)
  (v3:distance-squared vec-a vec-b))

(defmethod vecdistance-squared ((size (eql 4)) vec-a vec-b)
  (v4:distance-squared vec-a vec-b))

(defmacro distance-squared (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecdistance-squared (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecdot (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecdot ((size (eql 2)) vec-a vec-b)
  (v2:dot vec-a vec-b))

(defmethod vecdot ((size (eql 3)) vec-a vec-b)
  (v3:dot vec-a vec-b))

(defmethod vecdot ((size (eql 4)) vec-a vec-b)
  (v4:dot vec-a vec-b))

(defmacro dot (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecdot (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecabsolute-dot (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecabsolute-dot ((size (eql 2)) vec-a vec-b)
  (v2:absolute-dot vec-a vec-b))

(defmethod vecabsolute-dot ((size (eql 3)) vec-a vec-b)
  (v3:absolute-dot vec-a vec-b))

(defmethod vecabsolute-dot ((size (eql 4)) vec-a vec-b)
  (v4:absolute-dot vec-a vec-b))

(defmacro absolute-dot (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecabsolute-dot (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecperp-dot (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecperp-dot ((size (eql 2)) vec-a vec-b)
  (v2:perp-dot vec-a vec-b))

(defmacro perp-dot (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecperp-dot (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecnormalize (size vec-a)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecnormalize ((size (eql 2)) vec-a)
  (v2:normalize vec-a))

(defmethod vecnormalize ((size (eql 3)) vec-a)
  (v3:normalize vec-a))

(defmethod vecnormalize ((size (eql 4)) vec-a)
  (v4:normalize vec-a))

(defmacro normalize (vec-a)
  (base-macros:once-only (vec-a)
    `(vecnormalize (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric veccross (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod veccross ((size (eql 2)) vec-a vec-b)
  (v2:cross vec-a vec-b))

(defmethod veccross ((size (eql 3)) vec-a vec-b)
  (v3:cross vec-a vec-b))

(defmacro cross (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (eq (cl:length ,vec-a) (cl:length ,vec-b))
         (veccross (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))
