(in-package #:cepl-matrix3)
(import `base:float-zero)

;; Code adapted from Ogre which in turn was adapted 
;; from Wild Magic 0.2 Matrix math (free source code 
;; http://www.geometrictools.com/) and also from Nklien's
;; excelent few posts on optimizing common lisp
;; http://nklein.com/tags/optimization/

;; The coordinate system is assumed to be right-handed.
;; Coordinate axis rotation matrices are of the form
;;   RX =    1       0       0
;;           0     cos(t) -sin(t)
;;           0     sin(t)  cos(t)
;; where t > 0 indicates a counterclockwise rotation in the 
;; yz-plane
;;   RY =  cos(t)    0     sin(t)
;;           0       1       0
;;        -sin(t)    0     cos(t)
;; where t > 0 indicates a counterclockwise rotation in the 
;; zx-plane
;;   RZ =  cos(t) -sin(t)    0
;;         sin(t)  cos(t)    0
;;           0       0       1
;; where t > 0 indicates a counterclockwise rotation in the 
;; xy-plane.

;; remember that matrix co-ords go row then column, unlike 
;; how we are used to. The axies (as it were) are generally
;; named m & n where m is rows and n is columns

;; matrices will be stored as 1D arrays with 9 elements, 
;; this is for speed reasons

;; taken from Ogre, I'm assuming this is some kind of lower 
;; float boundary
;; const Real Matrix3::ms_fSvdEpsilon = 1e-04;

;; used for single value decomposition...need to read up on 
;; this again
;; const unsigned int Matrix3::ms_iSvdMaxIterations = 32;

;; [TODO] Ultimately I want to have all math function contents
;; generated at compile time by macros so we can set a single
;; flag for left or right handed co-ord systems and have it 
;; properly handled

;----------------------------------------------------------------

;; OK, so when you look at this is may seem incorrect at first,
;; however we are storing the matrix in Column Major Order.
;; This is due to the following passage from "Essential Mathema
;; tics for Games and Interactive Applications":
;; in Direct3D matrices are expected to be used with row vectors.
;; And even in OpenGL, despite the fact that the documentation
;; is written using column vectors, the internal representation 
;; premultiplies the vectors; that is, it expects row vectors as
;; well..<some more>..The solution is to pretranspose the matrix
;; in the storage representation.
;; This ONLY needs attention in the code that accesses the 1D 
;; version of the matrix. The rest of the code does not need to 
;; 'know'

(defmacro melm (mat-a row col)
  (cond ((and (numberp row) (numberp col)) 
	 `(aref ,mat-a ,(+ row (* col 3))))
	((numberp col)
	 `(aref ,mat-a (+ ,row ,(* col 3))))
	(t `(aref ,mat-a (+ ,row (* ,col 3))))))

;----------------------------------------------------------------

(defun identity-matrix3 ()
  #(1 0 0 0 1 0 0 0 1))

(defun zero-matrix3 ()
  #(0 0 0 0 0 0 0 0 0))

;----------------------------------------------------------------

(defun make-matrix3 ( a b c d e f g h i )
  (let ((result (zero-matrix3)))
    (setf (melm result 0 0) a)
    (setf (melm result 0 1) b)
    (setf (melm result 0 2) c)
    (setf (melm result 1 0) d)
    (setf (melm result 1 1) e)
    (setf (melm result 1 2) f)
    (setf (melm result 2 0) g)
    (setf (melm result 2 1) h)
    (setf (melm result 2 2) i)
    result))

;----------------------------------------------------------------

(defun make-from-rows (row-1 row-2 row-3)
  (make-matrix3 (cepl-vec3:v-x row-1)
		(cepl-vec3:v-y row-1)
		(cepl-vec3:v-z row-1) 
		(cepl-vec3:v-x row-2)
		(cepl-vec3:v-y row-2)
		(cepl-vec3:v-z row-2)
		(cepl-vec3:v-x row-3)
		(cepl-vec3:v-y row-3)
		(cepl-vec3:v-z row-3)))

;----------------------------------------------------------------

(defun get-rows (mat-a)
   (list (cepl-vec3:make-vector3 (melm mat-a 0 0)
				 (melm mat-a 1 0)
				 (melm mat-a 2 0))
	 (cepl-vec3:make-vector3 (melm mat-a 0 1)
				 (melm mat-a 1 1)
				 (melm mat-a 2 1))
	 (cepl-vec3:make-vector3 (melm mat-a 0 2)
				 (melm mat-a 1 2)
				 (melm mat-a 2 2))))

;----------------------------------------------------------------

(defun get-row (mat-a row-num)
  (cepl-vec3:make-vector3 (melm mat-a 0 row-num)
			  (melm mat-a 1 row-num)
			  (melm mat-a 2 row-num)))

;----------------------------------------------------------------

(defun make-from-columns (col-1 col-2 col-3)
  (make-matrix3 (cepl-vec3:v-x col-1)
		(cepl-vec3:v-x col-2)
		(cepl-vec3:v-x col-3) 
		(cepl-vec3:v-y col-1)
		(cepl-vec3:v-y col-2)
		(cepl-vec3:v-y col-3)
		(cepl-vec3:v-z col-1)
		(cepl-vec3:v-z col-2)
		(cepl-vec3:v-z col-3)))

;----------------------------------------------------------------

(defun get-columns (mat-a)
   (list (cepl-vec3:make-vector3 (melm mat-a 0 0)
				 (melm mat-a 0 1)
				 (melm mat-a 0 2))
	 (cepl-vec3:make-vector3 (melm mat-a 1 0)
				 (melm mat-a 1 1)
				 (melm mat-a 1 2))
	 (cepl-vec3:make-vector3 (melm mat-a 2 0)
				 (melm mat-a 2 1)
				 (melm mat-a 2 2))))

;----------------------------------------------------------------

(defun get-column (mat-a col-num)
  (cepl-vec3:make-vector3 (melm mat-a col-num 0)
			  (melm mat-a col-num 1)
			  (melm mat-a col-num 2)))

;----------------------------------------------------------------

(defun mzerop (mat-a)
  (loop for i 
     below 9
     if (not (float-zero (aref mat-a i)))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------

(defun identityp (mat-a)
  (and (= (aref mat-a 0) 1.0)
       (= (aref mat-a 4) 1.0)
       (= (aref mat-a 8) 1.0)
       (float-zero (aref mat-a 1))
       (float-zero (aref mat-a 2))
       (float-zero (aref mat-a 3))
       (float-zero (aref mat-a 5))
       (float-zero (aref mat-a 6))
       (float-zero (aref mat-a 7))))

;----------------------------------------------------------------

(defun meql (mat-a mat-b)
  (loop for i 
     below 9
     if (/= (aref mat-a i) (aref mat-b i))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------

;;[TODO] should definately inline this 
;;[TODO] Would it be faster not to have to cofactors too?
(defun determinate-cramer (mat-a)
  (let ((cofactor-0 (- (* (melm mat-a 1 1)
			  (melm mat-a 2 2))
		       (* (melm mat-a 1 2)
			  (melm mat-a 2 1))))
	(cofactor-3 (- (* (melm mat-a 0 2)
			  (melm mat-a 2 1))
		       (* (melm mat-a 0 1)
			  (melm mat-a 2 2))))
	(cofactor-6 (- (* (melm mat-a 0 1)
			  (melm mat-a 1 2))
		       (* (melm mat-a 0 2)
			  (melm mat-a 1 1)))))
    (values (+ (* (melm mat-a 0 0) cofactor-0)
	       (* (melm mat-a 1 0) cofactor-3)
	       (* (melm mat-a 2 0) cofactor-6))
	    cofactor-0 cofactor-3 cofactor-6)))

;----------------------------------------------------------------

;;[TODO] Look more into errors
(defun inverve (mat-a)
  (multiple-value-bind (det cofactor-0 cofactor-3 cofactor-6)
      (determinate-cramer mat-a)
    (if (float-zero det)
	(error "Matrix Inverse: Singular Matrix (determinate is 0)"))
    (let ((inv-det (/ 1.0 det)))
      (make-matrix3 (* inv-det cofactor-0)
		    (* inv-det cofactor-3)
		    (* inv-det cofactor-6)
		    (* inv-det (- (* (melm mat-a 1 2)
				     (melm mat-a 2 0))
				  (* (melm mat-a 1 0)
				     (melm mat-a 2 2))))
		    (* inv-det (- (* (melm mat-a 0 0)
				     (melm mat-a 2 2))
				  (* (melm mat-a 0 2)
				     (melm mat-a 2 0))))
		    (* inv-det (- (* (melm mat-a 0 2)
				     (melm mat-a 1 0))
				  (* (melm mat-a 0 0 )
				     (melm mat-a 1 2))))
		    (* inv-det (- (* (melm mat-a 1 0)
				     (melm mat-a 2 1))
				  (* (melm mat-a 1 1)
				     (melm mat-a 2 0))))
		    (* inv-det (- (* (melm mat-a 0 1)
				     (melm mat-a 2 0))
				  (* (melm mat-a 0 0)
				     (melm mat-a 2 1))))
		    (* inv-det (- (* (melm mat-a 0 0)
				     (melm mat-a 1 1))
				  (* (melm mat-a 0 1)
				     (melm mat-a 1 0))))))))

;----------------------------------------------------------------

(defun transpose (mat-a)
  (make-matrix3 (melm mat-a 0 0)
		(melm mat-a 1 0)
		(melm mat-a 2 0)
		(melm mat-a 0 1)
		(melm mat-a 1 1)
		(melm mat-a 2 1)
		(melm mat-a 0 2)
		(melm mat-a 1 2)
		(melm mat-a 2 2)))

;----------------------------------------------------------------

;;This is taken straight from 'Essential Mathematics for Game..'
;; Must be a more efficient way :)
(defun adjoint (mat-a)
  (make-matrix3  (- (* (melm mat-a 1 1)
		       (melm mat-a 2 2))
		    (* (melm mat-a 1 2)
		       (melm mat-a 2 1)))
		 (- (* (melm mat-a 0 2)
		       (melm mat-a 2 1))
		    (* (melm mat-a 0 1)
		       (melm mat-a 2 2)))
		 (- (* (melm mat-a 0 1)
		       (melm mat-a 1 2))
		    (* (melm mat-a 0 2)
		       (melm mat-a 1 1)))
		 (- (* (melm mat-a 1 2)
		       (melm mat-a 2 0))
		    (* (melm mat-a 1 0)
		       (melm mat-a 2 2)))
		 (- (* (melm mat-a 0 0)
		       (melm mat-a 2 2))
		    (* (melm mat-a 0 2)
		       (melm mat-a 2 0)))
		 (- (* (melm mat-a 0 2)
		       (melm mat-a 1 0))
		    (* (melm mat-a 0 0)
		       (melm mat-a 1 2)))
		 (- (* (melm mat-a 1 0)
		       (melm mat-a 2 1))
		    (* (melm mat-a 1 1)
		       (melm mat-a 2 0)))
		 (- (* (melm mat-a 0 1)
		       (melm mat-a 2 0))
		    (* (melm mat-a 0 0)
		       (melm mat-a 2 1)))
		 (- (* (melm mat-a 0 0)
		       (melm mat-a 1 1))
		    (* (melm mat-a 0 1)
		       (melm mat-a 1 0)))))

;----------------------------------------------------------------

(defun mtrace (mat-a)
  (+ (melm mat-a 0 0)
     (melm mat-a 1 1)
     (melm mat-a 2 2)))

;----------------------------------------------------------------

;;Rotation goes here, requires quaternion

;----------------------------------------------------------------

(defun make-rotation-matrix-euler (x y z)
  (let ((cx (cos x))
	(cy (cos y))
	(cz (cos z))
	(sx (sin x))
	(sy (sin y))
	(sz (sin z)))
    (make-matrix3 (* cy cz)
		  (+ (* sx sy cz) (* cx sz))
		  (+ (- (* cx sy cz)) (* sx sz))
		  (- (* cy sz))
		  (+ (- (* sx sy sz)) (* cx sz))
		  (+ (* cx sy sz) (* sx cz))
		  sy
		  (- (* sx cy))
		  (* cx cy))))

;----------------------------------------------------------------

;;not sure if there was a mistake in the book calculating
;;tyz
(defun make-rotation-mat-aa (axis angle)
  (let* ((c-a (cos angle))
	 (s-a (sin angle))
	 (tt (- 1.0 c-a))
	 (norm-axis (cepl-vec3:normalize axis))
	 (tx (* tt (cepl-vec3:v-x norm-axis)))
	 (ty (* tt (cepl-vec3:v-y norm-axis)))
	 (tz (* tt (cepl-vec3:v-z norm-axis)))
	 (sx (* s-a (cepl-vec3:v-x norm-axis)))
	 (sy (* s-a (cepl-vec3:v-y norm-axis)))
	 (sz (* s-a (cepl-vec3:v-z norm-axis)))
	 (txy (* tx (cepl-vec3:v-y norm-axis)))
	 (tyz (* tx (cepl-vec3:v-z norm-axis)))
	 (txz (* tx (cepl-vec3:v-z norm-axis))))
    (make-matrix3 (+ c-a (* tx (cepl-vec3:v-x norm-axis)))
		  (+ txy xz)
		  (- txz sy)
		  (- txy sz)
		  (+ c (* ty (cepl-vec3:v-y norm-axis)))
		  (+ tyz sx)
		  (+ txz sy)
		  (- tyz sx)
		  (+ c (* tz (cepl-vec3:v-z norm-axis))))))

;----------------------------------------------------------------

(defun make-scale-matrix-vec (vec)
  (make-matrix3 (cepl-vec3:v-x vec)
		0.0
		0.0
		0.0
		(cepl-vec3:v-y vec)
		0.0
		0.0
		0.0
		(cepl-vec3:v-z vec)))

;----------------------------------------------------------------

(defun make-scale-matrix (x y z)
  (make-matrix3 (cepl-vec3:v-x x)
		0.0
		0.0
		0.0
		(cepl-vec3:v-y y)
		0.0
		0.0
		0.0
		(cepl-vec3:v-z z)))

;----------------------------------------------------------------

(defun make-xrotation-matrix (angle)
  (let ((c-a (cos angle))
	(s-a (sin angle)))
    (make-matrix3 1.0
		  0.0
		  0.0
		  0.0
		  c-a
		  s-a
		  0.0
		  (- s-a)
		  c-a)))

;----------------------------------------------------------------

(defun make-yrotation-matrix (angle)
  (let ((c-a (cos angle))
	(s-a (sin angle)))
    (make-matrix3 c-a
		  0.0
		  (- s-a)
		  0.0
		  1.0
		  0.0
		  s-a
		  0.0
		  c-a)))

;----------------------------------------------------------------

(defun make-zrotation-matrix (angle)
  (let ((c-a (cos angle))
	(s-a (sin angle)))
    (make-matrix3 c-a
		  s-a
		  0.0
		  (- s-a)
		  c-a
		  0.0
		  0.0
		  0.0 
		  1.0)))

;----------------------------------------------------------------

;; Gets one set of possible z-y-x fixed angles that will generate
;; this matrix. Assumes that this is a rotation matrix
;; [TODO] returned as vector x-y-z
(defun get-fixed-angles (mat-a)
  (let* ((sy (melm mat-a 0 2))
	 (cy (base:c-sqrt (- 1.0 (* cy cy)))))
    (if (not (float-zero cy))
	(let* ((factor (/ 1.0 cy))
	       (sx (* factor (- (melm mat-a 2 1))))
	       (cx (* factor (melm mat-a 2 2)))
	       (sz (* factor (- (melm mat-a 1 0))))
	       (cz (* factor (melm mat-a 0 0))))
	  (cepl-vec3:make-vector3 (atan sx cx)
				  (atan sy cy)
				  (atan sz cz)))
	(let* ((sz 0.0)
	       (cx 1.0)
	       (sz (melm mat-a 1 2))
	       (cz (melm mat-a 1 1)))
	  (cepl-vec3:make-vector3 (atan sx cx)
				  (atan sy cy)
				  (atan sz cz))))))

;----------------------------------------------------------------

;; Gets one possible axis-angle pair that will generate this 
;; matrix. Assumes that this is a rotation matrix
;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works

(defun get-axis-angle (mat-a)
  (let* ((c-a (* 0.5 (- (mtrace mat-a) 1.0)))
	 (angle (acos c-a)))
    (cond ((float-zero angle) 
	   ;angle is zero so axis can be anything
	   (cepl-vec3:make-vector3 1.0 0.0 0.0))
	  ((< angle (- base:+pi+ base:+float-threshold+))
					;its not 180 degrees
	   (let ((axis (cepl-vec3:make-vector3 
			(- (melm mat-a 1 2) (melm mat-a 2 1))
			(- (melm mat-a 2 0) (melm mat-a 0 2))
			(- (melm mat-a 0 1) (melm mat-a 1 0)))))
	     (cepl-vec3:normalize axis)))
	  (t (let* ((i (if (> (melm mat-a 1 1)
			      (melm mat-a 0 0))
			   1
			   (if (> (melm mat-a 2 2)
				  (melm mat-a 0 0))
			       2
			       0)))
		    (j (mod (+ i 1) 3))
		    (k (mod (+ j 1) 3))
		    (s (base:c-sqrt (+ 1.0 
				       (- 
					(melm mat-a i i)
					(melm mat-a j j)
					(melm mat-a k k)))))
		    (recip (/ 1.0 s))
		    (result (cepl-vec3:make-vector3 0.0 
						    0.0 
						    0.0)))
	       (setf (aref result i) (* 0.5 s))
	       (setf (aref result j) (* recip
					(melm mat-a i j)))
	       (setf (aref result j) (* recip
					(melm mat-a k i)))
	       result)))))

;----------------------------------------------------------------

(defun m+ (mat-a mat-b)
  (let ((r (zero-matrix3)))
    (loop for i below 9
	 do (setf (aref r i) (+ (aref mat-a i) 
				 (aref mat-b i))))
    r))

;----------------------------------------------------------------

(defun m- (mat-a mat-b)
  (let ((r (zero-matrix3)))
    (loop for i below 9
	 do (setf (aref r i) (- (aref mat-a i) 
				 (aref mat-b i))))
    r))

;----------------------------------------------------------------

(defun negate (mat-a)
  (let ((result (zero-matrix3)))
    (loop for i below 9
	 do (setf (aref result i) (- (aref mat-a i))))
    result))

;----------------------------------------------------------------

(defun m* (mat-a mat-b))

;----------------------------------------------------------------

;;(defun m*vec ())

;----------------------------------------------------------------

;; 4x4 matrix vec multiply
(declaim (ftype (function ((simple-array single-float (12))
                           (simple-array single-float (3)))
                          (simple-array single-float (3))) mvl*))
(defun mvl* (matrix vec)
  (declare (optimize (speed 3) (safety 0)))
  (let ((ret (make-array 3 :initial-element 0.0f0
                           :element-type 'single-float)))
    (loop for jj from 0 below 3
       do (let ((offset (* jj 4)))
            (setf (aref ret jj)
                  (+ (aref matrix (+ offset 3))
                     (loop for ii from 0 below 3
                        for kk from offset below (+ offset 3)
                        summing (* (aref vec ii)
                                   (aref matrix kk))
                        single-float)))))
    ret))
