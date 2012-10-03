;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This package provides a generic interface to all the vector
;; functions provided in vector2,3 & 4.
;; It is here to make programming more pleasent.
;; Also see the reader macro for creating vectors 

;; DO NOT ':USE' THIS PACKAGE IN YOUR PROGRAMS AS IT REDEFINES
;; VARIOUS MATHEMATICAL SYMBOLS

(in-package :vectors)

;----------------------------------------------------------------

(defun swizzle (&rest vectors)
    "Takes a list of vectors and combines them into a new vector"
    (labels ((seqify (x) 
	       (if (or (listp x) (arrayp x))
		   x
		   (list x))))
      (let ((combined (mapcar #'(lambda (x) 
				  (coerce x 'single-float))
		       (apply #'concatenate 'list
			      (mapcar #'seqify vectors)))))
	(apply #'make-vector combined))))

;; doesnt restrict length and needs correctly typed vectors
(defun strict-swizzle (&rest vectors)
  (apply #'concatenate 
         `(SIMPLE-ARRAY SINGLE-FLOAT 
                        (,(apply #'cl:+ (mapcar #'cl:length vectors)))) 
         vectors))

;----------------------------------------------------------------

(defun make-vector (x y &optional z w)
  "This takes floats and give back a vector, this is just an
   array but it specifies the array type and populates it. "
  (cond (w (vector4:make-vector4 x y z w))
	(z (vector3:make-vector3 x y z))
	(t (vector2:make-vector2 x y))))

;----------------------------------------------------------------

(defgeneric veczerop (size vec-a)
  (:documentation "Returns t if the vector is of zero length"))

(defmethod veczerop ((size (cl:eql 2)) vec-a)
  (v2:vzerop vec-a))

(defmethod veczerop ((size (cl:eql 3)) vec-a)
  (v3:vzerop vec-a))

(defmethod veczerop ((size (cl:eql 4)) vec-a)
  (v4:vzerop vec-a))

(defmacro zerop (vec-a)
  (base-macros:once-only (vec-a)
    `(veczerop (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric vecunitp (size vec-a)
  (:documentation "Returns t if the vector is of unit length"))

(defmethod vecunitp ((size (cl:eql 2)) vec-a)
  (v2:unitp vec-a))

(defmethod vecunitp ((size (cl:eql 3)) vec-a)
  (v3:unitp vec-a))

(defmethod vecunitp ((size (cl:eql 4)) vec-a)
  (v4:unitp vec-a))

(defmacro unitp (vec-a)
  (base-macros:once-only (vec-a)
    `(vecunitp (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric veceq (size vec-a vec-b)
  (:documentation "Returns t if the two vectors are equal"))

(defmethod veceq ((size (cl:eql 2)) vec-a vec-b)
  (v2:v-eq vec-a vec-b))

(defmethod veceq ((size (cl:eql 3)) vec-a vec-b)
  (v3:v-eq vec-a vec-b))

(defmethod veceq ((size (cl:eql 4)) vec-a vec-b)
  (v4:v-eq vec-a vec-b))

(defmacro eq (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(veceq (cl:length ,vec-a) ,vec-a ,vec-b)))

;----------------------------------------------------------------

(defun = (&rest vecs)
  "Returns either t if the two vectors are equal. 
   Otherwise it returns nil."
  (let ((vec-a (first vecs)))
    (loop for vec in (cdr vecs)
       when (not (eq vec-a vec)) do (return nil)
	 finally (return t))))

;----------------------------------------------------------------

(defun v/= (&rest vecs)
  "Returns either t if the two vectors are equal. 
   Otherwise it returns nil."
  (let ((vec-a (first vecs)))
    (loop for vec in (cdr vecs)
       when (eq vec-a vec) do (return nil)
	 finally (return t))))

;----------------------------------------------------------------

(defgeneric vec+1 (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec+1 ((size (cl:eql 2)) vec-a vec-b)
  (v2:v+1 vec-a vec-b))

(defmethod vec+1 ((size (cl:eql 3)) vec-a vec-b)
  (v3:v+1 vec-a vec-b))

(defmethod vec+1 ((size (cl:eql 4)) vec-a vec-b)
  (v4:v+1 vec-a vec-b))

(defmacro 1+ (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vec+1 (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vec-1 (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec-1 ((size (cl:eql 2)) vec-a vec-b)
  (v2:v-1 vec-a vec-b))

(defmethod vec-1 ((size (cl:eql 3)) vec-a vec-b)
  (v3:v-1 vec-a vec-b))

(defmethod vec-1 ((size (cl:eql 4)) vec-a vec-b)
  (v4:v-1 vec-a vec-b))

(defmacro 1- (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vec-1 (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vec+ (size &rest vecs)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec+ ((size (cl:eql 2)) &rest vecs)
  (apply #'v2:v+ vecs))

(defmethod vec+ ((size (cl:eql 3)) &rest vecs)
  (apply #'v3:v+ vecs))

(defmethod vec+ ((size (cl:eql 4)) &rest vecs)
  (apply #'v4:v+ vecs))

(defmacro + (vec-a &rest vecs)
  (base-macros:once-only (vec-a)
    `(vec+ (cl:length ,vec-a) ,vec-a ,@vecs)))

;----------------------------------------------------------------

(defgeneric vec- (size &rest vecs)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec- ((size (cl:eql 2)) &rest vecs)
  (apply #'v2:v- vecs))

(defmethod vec- ((size (cl:eql 3)) &rest vecs)
  (apply #'v3:v- vecs))

(defmethod vec- ((size (cl:eql 4)) &rest vecs)
  (apply #'v4:v- vecs))

(defmacro - (vec-a &rest vecs)
  (base-macros:once-only (vec-a)
    `(vec- (cl:length ,vec-a) ,vec-a ,@vecs)))

;----------------------------------------------------------------

(defgeneric vec* (size vec-a multiple)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vec* ((size (cl:eql 2)) vec-a (multiple number))
  (v2:v* vec-a (coerce multiple 'single-float)))

(defmethod vec* ((size (cl:eql 3)) vec-a (multiple number))
  (v3:v* vec-a (coerce multiple 'single-float)))

(defmethod vec* ((size (cl:eql 4)) vec-a (multiple number))
  (v4:v* vec-a (coerce multiple 'single-float)))

(defmethod vec* ((size (cl:eql 2))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v2:v*vec vec-a multiple))

(defmethod vec* ((size (cl:eql 3))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v3:v*vec vec-a multiple))

(defmethod vec* ((size (cl:eql 4))
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

(defmethod vec/ ((size (cl:eql 2)) vec-a (multiple number))
  (v2:v/ vec-a (coerce multiple 'single-float)))

(defmethod vec/ ((size (cl:eql 3)) vec-a (multiple number))
  (v3:v/ vec-a (coerce multiple 'single-float)))

(defmethod vec/ ((size (cl:eql 4)) vec-a (multiple number))
  (v4:v/ vec-a (coerce multiple 'single-float)))

(defmethod vec/ ((size (cl:eql 2))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v2:v/vec vec-a multiple))

(defmethod vec/ ((size (cl:eql 3))
		 vec-a 
		 (multiple #.(class-of 
			     (make-array 0 :element-type 
					 'single-float))))
  (v3:v/vec vec-a multiple))

(defmethod vec/ ((size (cl:eql 4))
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

(defmethod veclength ((size (cl:eql 2)) vec-a)
  (v2:vlength vec-a))

(defmethod veclength ((size (cl:eql 3)) vec-a)
  (v3:vlength vec-a))

(defmethod veclength ((size (cl:eql 4)) vec-a)
  (v4:vlength vec-a))

(defmacro length (vec-a)
  (base-macros:once-only (vec-a)
    `(veclength (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric veclength-squared (size vec-a)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod veclength-squared ((size (cl:eql 2)) vec-a)
  (v2:vlength-squared vec-a))

(defmethod veclength-squared ((size (cl:eql 3)) vec-a)
  (v3:vlength-squared vec-a))

(defmethod veclength-squared ((size (cl:eql 4)) vec-a)
  (v4:vlength-squared vec-a))

(defmacro length-squared (vec-a)
  (base-macros:once-only (vec-a)
    `(veclength-squared (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric vecdistance (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecdistance ((size (cl:eql 2)) vec-a vec-b)
  (v2:distance vec-a vec-b))

(defmethod vecdistance ((size (cl:eql 3)) vec-a vec-b)
  (v3:distance vec-a vec-b))

(defmethod vecdistance ((size (cl:eql 4)) vec-a vec-b)
  (v4:distance vec-a vec-b))

(defmacro distance (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecdistance (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecdistance-squared (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecdistance-squared ((size (cl:eql 2)) vec-a vec-b)
  (v2:distance-squared vec-a vec-b))

(defmethod vecdistance-squared ((size (cl:eql 3)) vec-a vec-b)
  (v3:distance-squared vec-a vec-b))

(defmethod vecdistance-squared ((size (cl:eql 4)) vec-a vec-b)
  (v4:distance-squared vec-a vec-b))

(defmacro distance-squared (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecdistance-squared (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecdot (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecdot ((size (cl:eql 2)) vec-a vec-b)
  (v2:dot vec-a vec-b))

(defmethod vecdot ((size (cl:eql 3)) vec-a vec-b)
  (v3:dot vec-a vec-b))

(defmethod vecdot ((size (cl:eql 4)) vec-a vec-b)
  (v4:dot vec-a vec-b))

(defmacro dot (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecdot (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecabsolute-dot (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecabsolute-dot ((size (cl:eql 2)) vec-a vec-b)
  (v2:absolute-dot vec-a vec-b))

(defmethod vecabsolute-dot ((size (cl:eql 3)) vec-a vec-b)
  (v3:absolute-dot vec-a vec-b))

(defmethod vecabsolute-dot ((size (cl:eql 4)) vec-a vec-b)
  (v4:absolute-dot vec-a vec-b))

(defmacro absolute-dot (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecabsolute-dot (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecperp-dot (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecperp-dot ((size (cl:eql 2)) vec-a vec-b)
  (v2:perp-dot vec-a vec-b))

(defmacro perp-dot (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (vecperp-dot (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))

;----------------------------------------------------------------

(defgeneric vecnormalize (size vec-a)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod vecnormalize ((size (cl:eql 2)) vec-a)
  (v2:normalize vec-a))

(defmethod vecnormalize ((size (cl:eql 3)) vec-a)
  (v3:normalize vec-a))

(defmethod vecnormalize ((size (cl:eql 4)) vec-a)
  (v4:normalize vec-a))

(defmacro normalize (vec-a)
  (base-macros:once-only (vec-a)
    `(vecnormalize (cl:length ,vec-a) ,vec-a)))

;----------------------------------------------------------------

(defgeneric veccross (size vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))

(defmethod veccross ((size (cl:eql 2)) vec-a vec-b)
  (v2:cross vec-a vec-b))

(defmethod veccross ((size (cl:eql 3)) vec-a vec-b)
  (v3:cross vec-a vec-b))

(defmacro cross (vec-a vec-b)
  (base-macros:once-only (vec-a vec-b)
    `(if (cl:eq (cl:length ,vec-a) (cl:length ,vec-b))
         (veccross (cl:length ,vec-a) ,vec-a ,vec-b)
         (error "Vector size mismatch"))))
