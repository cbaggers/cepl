;; This package provides a generic interface to all the vector
;; functions provided in vector2,3 & 4.
;; It is here to make programming more pleasent.
;; Also see the reader macro for creating vectors in base-math.

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
	      (size (length attrs))
	      (command (cepl-utils:symbolicate-package
			(format nil "VECTOR~s" size)
			"MAKE-VECTOR"
			(cepl-utils:mkstr size))))
	 (cons command attrs))))

;----------------------------------------------------------------

(defgeneric v+1 (vec-a vec-b)
  (:documentation "Adds two vectors together and returns the result as a new vector of the same type"))


(defmethod v+1 ((vec-a #.(class-of (make-array 2
					       :element-type 
					       `single-float)))
		(vec-b #.(class-of (make-array 2 
					       :element-type 
					       `single-float))))
  (v2:v+1 vec-a vec-b))


(defmethod v+1 ((vec-a #.(class-of (make-array 3 
					       :element-type 
					       `single-float)))
		(vec-b #.(class-of (make-array 3 
					       :element-type 
					       `single-float))))
  (v3:v+1 vec-a vec-b))



(defmethod v+1 ((vec-a #.(class-of (make-array 4
					       :element-type 
					       `single-float)))
		(vec-b #.(class-of (make-array 4 
					       :element-type 
					       `single-float))))
  (v4:v+1 vec-a vec-b))
