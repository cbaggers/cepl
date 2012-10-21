;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; This package is for all the quaternion math functions

;;; [TODO] NEED TO GO THROUGH ALL OF THIS AND DOUBLE CHECK 
;;;        THE SQRT v INV-SQRT AS I HAVE A FEW WRONG

(in-package #:quaternions)

;----------------------------------------------------------------

(defmacro w (quat)
  "Returns the w component of the quaternion"  
  `(aref ,quat 0))

(defmacro x (quat)
  "Returns the x component of the quaternion"
  `(aref ,quat 1))

(defmacro y (quat)
  "Returns the y component of the quaternion"
  `(aref ,quat 2))

(defmacro z (quat)
  "Returns the z component of the quaternion"
  `(aref ,quat 3))

;;----------------------------------------------------------------;;

(defun make-quat (w x y z)
  "This creates a quaternion. As a quaternion is just a vector4
   we just call that function."
  (v4:make-vector4 w x y z))

(defun zero-quat ()
  (v4:make-vector4 0.0 0.0 0.0 0.0))

(defun identity-quat ()
  (v4:make-vector4 1.0 0.0 0.0 0.0))

(defun unit-p (quat)
  (float-zero (- 1.0 
		 (* (q:w quat) (q:w quat))
		 (* (q:x quat) (q:x quat))
		 (* (q:y quat) (q:y quat))
		 (* (q:z quat) (q:z quat)))))

(defun identity-p (quat)
  (and (float-zero (- 1.0 (q:w quat)))
       (float-zero (q:x quat))
       (float-zero (q:y quat))
       (float-zero (q:z quat))))

;; setters 
(defun make-quat-axis-angle (axis angle)
  ;; if axis of rotation is zero vector then make an identity
  ;; quaternion.
  (let ((length (v4:vlength-squared axis)))
    (if (float-zero length)
	(identity-quat)
	(let* 
	    ((half-angle (/ angle 2.0))
	     (sin-half-angle (sin half-angle))
	     (cos-half-angle (cos half-angle))
	     (scale-factor (/ sin-half-angle (c-inv-sqrt length))))
	  (v4:make-vector4 (cos-half-angle)
			   (* scale-factor (v-x axis))
			   (* scale-factor (v-y axis))
			   (* scale-factor (v-z axis)))))))

(defun make-quat-from-vec (vec3)
  (make-quat 0 (v-x vec3) (v-y vec3) (v-z vec3)))

;; [TODO] too much array creation. could be faster
;;        should have the functions that do the normalisation
;;        take and return floats rather than quats and use them
;;        to limit the ammount of array creation and access
;;        required.
(defun make-quat-from-vecs (start end)
  ;; the axis of rotation
  (let ((axis (v3:cross start end))
	(initial-quat (normalize (make-quat (v3.dot from to) 
					    (v-x axis) 
					    (v-y axis) 
					    (v-z axis)))))
    (setf (q:w initial-quat) (+ 1.0 (q:w initial-quat)))
    (normalize (if (float-less-than-zero (q:w initial-quat))
		   (if (> (* (v-z start) (v-z start))
			  (* (v-x start) (v-x start)))
		       (make-quat 0.0 
				  0.0 
				  (v-z start) 
				  (- (v-y start)))
		       (make-quat 0.0
				  (v-y start)
				  (- (v-x start))
				  0.0))
		   initial-quat))))

(defun make-quat-from-rotation-matrix (mat3)
  (let ((mtrace (m3:mtrace mat3)))
    (if (> mtrace 0.0)
	(let ((s (c-sqrt (+ 1.0 trace) )))))))

(defun make-quat-from-fixed-angles (x-rot y-rot z-rot)
  (let ((x-rot (/ x-rot 2.0))
	(y-rot (/ y-rot 2.0))
	(z-rot (/ z-rot 2.0)))
    (let (((cos-x (cos x-rot)) (sin-x (sin x-rot)))
	  ((cos-y (cos y-rot)) (sin-y (sin y-rot)))
	  ((cos-z (cos z-rot)) (sin-z (sin z-rot))))
      (make-quat (- (* cos-x cos-y cos-z) (* sin-x sin-y sin-z))
		 (- (* sin-x cos-y cos-z) (* cos-x sin-y sin-z))
		 (- (* cos-x sin-y cos-z) (* sin-x cos-y sin-z))
		 (- (* cos-x cos-y sin-z) (* sin-x sin-y cos-x))))))


(defun get-axis-angle (quat)
  (list 
   (let ((length (c-inv-sqrt (- 1.0 (* (q:w quat) (q:w quat))))))
     (if (float-zero length)
	 (zero-quat)
	 (let ((length (/ 1.0 length)))
	   (make-quat (* length (q:x quat) (q:y quat) (q:z quat))))))
   (* 2.0 (acos (q:w quat)))))

(defun normalize (quat)
  (let ((length-squared (v4:dot quat quat)))
    (if (float-zero length-squared)
	(zero-quat)
	(let ((factor (c-inv-sqrt length-squared)))
	  (make-quat (* (q:w quat) factor)
		     (* (q:x quat) factor)
		     (* (q:y quat) factor)
		     (* (q:z quat) factor))))))
(defun conjugate (quat)
  (make-quat (q:w quat) (- (q:x quat)) (- (q:y quat)) (- (q:z quat))))

;; [TODO] Identity-quat is a pretty crap name...all the 'quats' are
;;        fairly redundant
(defun inverse (quat)
  (let ((norm (v4:dot quat quat)))
    (if (float-zero norm)
	(identity-quat)
	(let ((norm-recip (/ 1.0 norm)))
	  (make-quat (* norm-recip (q:w quat))
		     (- (* norm-recip (q:x quat)))
		     (- (* norm-recip (q:y quat)))
		     (- (* norm-recip (q:z quat))))))))

(defun q+1 (quat-a quat-b)
  (v4:v+1 quat-a quat-b))

(defun q+ (&rest quats)
  (apply v4:v+ quats))

(defun q-1 (quat-a quat-b)
  (v4:v-1 quat-a quat-b))

(defun q- (&rest quats)
  (apply v4:v- quats))

(defun q* (quat-a scalar)
  (v4:v* quat-a scalar))

(defun q*quat (quat-a quat-b)
  (make-quat (- (* (q:w quat-a) (q:w quat-b))
		(* (q:x quat-a) (q:x quat-b))
		(* (q:y quat-a) (q:y quat-b))
		(* (q:z quat-a) (q:z quat-b)))
	     (- (+ (* (q:w quat-a) (q:x quat-b))
		   (* (q:x quat-a) (q:w quat-b))
		   (* (q:y quat-a) (q:z quat-b)))
		(* (q:z quat-a) (q:y quat-b)))
	     (- (+ (* (q:w quat-a) (q:y quat-b))
		   (* (q:y quat-a) (q:w quat-b))
		   (* (q:z quat-a) (q:x quat-b)))
		(* (q:x quat-a) (q:z quat-b)))
	     (- (+ (* (q:w quat-a) (q:z quat-b))
		   (* (q:z quat-a) (q:w quat-b))
		   (* (q:x quat-a) (q:y quat-b)))
		(* (q:y quat-a) (q:x quat-b)))))

(defun dot (quat-a quat-b)
  (v4:dot quat-a quat-b))

;; [TODO] Look into assets (this should be a unit quaternion
(defun rotate (vector quat)
  "Rotate vector by quaternion. Assumes quaternion is normalized."
  (let* ((v-mult (* 2.0 (+ (* (q:x quat) (v-x vector))
			    (* (q:y quat) (v-y vector))
			    (* (q:z quat) (v-z vector)))))
	 (cross-mult (* 2.0 (q:w quat)))
	 (p-mult (- (* cross-mult (q:w quat)) 1.0)))
    (v3:make-vector3 (+ (* p-mult (v-x vector))
			(* v-mult (q:x quat))
			(* cross-mult 
			   (- (* (q:y quat) (v-z vector))
			      (* (q:z quat) (v-y vector)))))
		     (+ (* p-mult (v-y vector))
			(* v-mult (q:y quat))
			(* cross-mult 
			   (- (* (q:z quat) (v-x vector))
			      (* (q:x quat) (v-z vector)))))
		     (+ (* p-mult (v-z vector))
			(* v-mult (q:z quat))
			(* cross-mult 
			   (- (* (q:x quat) (v-y vector))
			      (* (q:y quat) (v-x vector))))))))

;; [TODO] Could be faster (see q+1 area)
(defun lerp (start-quat end-quat pos)
  "Linearaly interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (let ((cos-angle (dot start-quat end-quat)))
    (if (float-greater-than-zero cos-angle)
	(q+1 (q* end-quat pos)
	     (q* start-quat (- 1.0 pos)))
	(q+1 (q* end-quat pos)
	     (q* start-quat (- pos 1.0))))))

(defun slerp (start-quat end-quat pos)
  "Spherically interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (destructuring-bind (start-mult end-mult)
      (let ((cos-angle (dot start-quat end-quat)))
	;; if angle between quaternions is less than 90 degrees
	(if (float-greater-than-zero cos-angle)
	    ;; if angle is greater than zero
	    (if (float-greater-than-zero (- 1.0 cos-angle))
		(let ((angle (acos cos-angle))
		      (recip-sin-angle (/ 1.0 (sin angle))))
		  (list (* (sin (* (- 1.0 pos) angle)) 
			   recip-sin-angle)
			(* (sin (* pos angle))
			   recip-sin-angle)))
		;; angle is close to zero
		(list (- 1.0 pos) pos))
	    ;; we take the shorter route
	    ;; if angle is less that 180 degrees
	    (if (float-greater-than-zero (+ 1.0 cos-angle))
		(let ((angle (acos (- cos-angle)))
		      (recip-sin-angle (/ 1.0 (sin angle))))
		  (list (* (sin (* (- pos 1.0) angle)) 
			   recip-sin-angle)
			(* (sin (* pos angle))
			   recip-sin-angle)))
		;; angle is close to 180 degrees
		(list (- pos 1.0) pos))))
    (q+1 (q* start-quat start-mult)
	 (q* end-quat end-mult))))

(defun approx-slerp (start-quat end-quat pos)
  (let* ((cos-angle (dot start-quat end-quat))
	 (factor (expt (- 1.0 (* 0.7878088 cos-angle)) 2.0))
	 (k (* 0.5069269 factor))
	 (b (* 2.0 k))
	 (c (* -3 k))
	 (d (+ 1 k))
	 (pos (+ (* pos (+ c (* b pos))) d)))
    ;; if angle is less than 90 degrees
    (if (float-greater-than-zero cos-angle)
	;; use standard interp
	(q+1 (q* end-quat pos)
	     (q* start-quat (- 1.0 pos)))
	;; take shorter path
	(q+1 (q* end-quat pos)
	     (q* start-quat (- pos 1.0))))))


