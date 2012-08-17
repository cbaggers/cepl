;; This is the 4x4 matrix functionality. 
;; There will be a generic function-set to make this as easy
;; as possible for people writing the games but this will 
;; be in a seperate package (prehaps the base-maths one)

(in-package :matrix4)

;----------------------------------------------------------------

(defmacro melm (mat-a row col)
  "A helper macro to provide access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have 
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (cond ((and (numberp row) (numberp col)) 
	 `(aref ,mat-a ,(+ row (* col 4))))
	((numberp col)
	 `(aref ,mat-a (+ ,row ,(* col 4))))
	(t `(aref ,mat-a (+ ,row (* ,col 4))))))

;----------------------------------------------------------------

(defun identity-matrix4 ()
  "Return a 4x4 identity matrix"
  (make-array 16 :element-type `single-float :initial-contents
	      #(1.0 0.0 0.0 0.0
		0.0 1.0 0.0 0.0
		0.0 0.0 1.0 0.0
		0.0 0.0 0.0 1.0)))

(defun zero-matrix4 ()
  "Return a 4x4 zero matrix"
  (make-array 16 :element-type `single-float))

(defun 2dclipspace-to-imagespace-matrix4 ()
  (make-array 16 :element-type `single-float :initial-contents
	      #(0.5  0.0  0.0  0.5
		0.0 -0.0  0.0  0.5
		0.0  0.0  1.0  0.0
		0.0  0.0  0.0  1.0)))

;----------------------------------------------------------------

(defun make-matrix4 ( a b c d e f g h i j k l m n o p )
  "Make a 4x4 matrix. Data must be provided in row major order"
  (let ((result (zero-matrix4)))
    (setf (melm result 0 0) a)
    (setf (melm result 0 1) b)
    (setf (melm result 0 2) c)
    (setf (melm result 0 3) d)
    (setf (melm result 1 0) e)
    (setf (melm result 1 1) f)
    (setf (melm result 1 2) g)
    (setf (melm result 1 3) h)
    (setf (melm result 2 0) i)
    (setf (melm result 2 1) j)
    (setf (melm result 2 2) k)
    (setf (melm result 2 3) l)
    (setf (melm result 3 0) m)
    (setf (melm result 3 1) n)
    (setf (melm result 3 2) o)
    (setf (melm result 3 3) p)
    result))


;----------------------------------------------------------------

(defun make-from-rows (row-1 row-2 row-3 row-4)
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the rows"
  (make-matrix4 (v-x row-1) (v-y row-1) (v-z row-1) (v-w row-1) 
		(v-x row-2) (v-y row-2)	(v-z row-2) (v-w row-2)
		(v-x row-3) (v-y row-3) (v-z row-3) (v-w row-3)
		(v-x row-4) (v-y row-4) (v-z row-4) (v-w row-4)))

;----------------------------------------------------------------

(defun get-rows (mat-a)
  "Return the rows of the matrix as 4 vector4s"
   (list (make-vector4 (melm mat-a 0 0)
		       (melm mat-a 0 1)
		       (melm mat-a 0 2)
		       (melm mat-a 0 3))
	 (make-vector4 (melm mat-a 1 0)
		       (melm mat-a 1 1)
		       (melm mat-a 1 2)
		       (melm mat-a 1 3))
	 (make-vector4 (melm mat-a 2 0)
		       (melm mat-a 2 1)
		       (melm mat-a 2 2)
		       (melm mat-a 2 3))
	 (make-vector4 (melm mat-a 3 0)
		       (melm mat-a 3 1)
		       (melm mat-a 3 2)
		       (melm mat-a 3 3))))

;----------------------------------------------------------------

(defun get-row (mat-a row-num)
  "Return the specified row of the matrix a vector4"
  (make-vector4 (melm mat-a row-num 0)
		(melm mat-a row-num 1)
		(melm mat-a row-num 2)
		(melm mat-a row-num 3)))


;----------------------------------------------------------------

(defun make-from-columns (col-1 col-2 col-3 col-4)
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the columns"
  (make-matrix4 (v-x col-1)
		(v-x col-2)
		(v-x col-3)
		(v-x col-4)
		(v-y col-1)
		(v-y col-2)
		(v-y col-3)
		(v-y col-4)
		(v-z col-1)
		(v-z col-2)
		(v-z col-3)
		(v-z col-4)
		(v-w col-1)
		(v-w col-2)
		(v-w col-3)
		(v-w col-4)))

;----------------------------------------------------------------

(defun get-columns (mat-a)
  "Return the columns of the matrix as 4 vector4s"
   (list (make-vector4 (melm mat-a 0 0)
		       (melm mat-a 1 0)
		       (melm mat-a 2 0)
		       (melm mat-a 3 0))
	 (make-vector4 (melm mat-a 0 1)
		       (melm mat-a 1 1)
		       (melm mat-a 2 1)
		       (melm mat-a 3 1))
	 (make-vector4 (melm mat-a 0 2)
		       (melm mat-a 1 2)
		       (melm mat-a 2 2)
		       (melm mat-a 3 2))
	 (make-vector4 (melm mat-a 0 3)
		       (melm mat-a 1 3)
		       (melm mat-a 2 3)
		       (melm mat-a 3 3))))

;----------------------------------------------------------------

(defun get-column (mat-a col-num)
  "Return the specified column of the matrix a vector4"
  (make-vector4 (melm mat-a 0 col-num)
		(melm mat-a 1 col-num)
		(melm mat-a 2 col-num)
		(melm mat-a 3 col-num)))

;----------------------------------------------------------------

(defun mzerop (mat-a)
  "Returns 't' if this is a zero matrix (as contents of the 
   matrix are floats the values have an error bound as defined
   in base-maths"
  (loop for i below 16
     if (not (float-zero (aref mat-a i)))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------

;[TODO] should the checks for '1.0' also have the error bounds?
(defun identityp (mat-a)
  "Returns 't' if this is an identity matrix (as contents of the 
   matrix are floats the values have an error bound as defined
   in base-maths"
  (and (float-zero (- (melm mat-a 0 0) 1.0))
       (float-zero (- (melm mat-a 1 1) 1.0))
       (float-zero (- (melm mat-a 2 2) 1.0))
       (float-zero (- (melm mat-a 3 3) 1.0))
       (float-zero (melm mat-a 0 1))
       (float-zero (melm mat-a 0 2))
       (float-zero (melm mat-a 0 3))
       (float-zero (melm mat-a 1 0))
       (float-zero (melm mat-a 1 2))
       (float-zero (melm mat-a 1 3))
       (float-zero (melm mat-a 2 0))
       (float-zero (melm mat-a 2 1))
       (float-zero (melm mat-a 2 3))
       (float-zero (melm mat-a 3 0))
       (float-zero (melm mat-a 3 1))
       (float-zero (melm mat-a 3 2))))

;----------------------------------------------------------------

(defun meql (mat-a mat-b)
  "Returns t if all elements of both matrices provided are 
   equal"
  (loop for i 
     below 16
     if (/= (aref mat-a i) (aref mat-b i))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------

;; Ok so the determinant is of an element in the matrix is the 
;; value of that element multiplied by the determinant of the 
;; submatrix formed when you remove the row and column the element
;; lie in.
;; The minor is the 'determinant of the submatrix' mentioned above
;; If you create a matrix B from Matrix A where evey element in
;; B is the minor of the corrosponding element in A then
;; Matrix B is the matrix of cofactors. 
;; If Matrix B is tranposed then the new Matrix (we will call 
;; Matrix C) is known as the adjoint matrix
;; The Inverse of a matrix can be calculated as:
;; (* (adjoint matrix-a) (/ 1.0 (determinant matrix-a)))
;; This method is known as cramer's method and is fast enough
;; for 3x3 and 4x4 matrices. Thus we can use it for games.

(defun minor (mat-a row-0 row-1 row-2 col-0 col-1 col-2)
  (+ (* (melm mat-a row-0 col-0) 
	(- (* (melm mat-a row-1 col-1) (melm mat-a row-2 col-2))
	   (* (melm mat-a row-2 col-1) (melm mat-a row-1 col-2))))

     (* (melm mat-a row-0 col-2) 
	(- (* (melm mat-a row-1 col-0) (melm mat-a row-2 col-1))
	   (* (melm mat-a row-2 col-0) (melm mat-a row-1 col-1))))

     (- (* (melm mat-a row-0 col-1) 
	   (- (* (melm mat-a row-1 col-0) (melm mat-a row-2 col-2))
	      (* (melm mat-a row-2 col-0) (melm mat-a row-1 col-2)))))))

;----------------------------------------------------------------

(defun adjoint (mat-a)
  "Returns the adjoint of the matrix"
  (make-matrix4 (minor mat-a 1 2 3 1 2 3)
		(- (minor mat-a 0 2 3 1 2 3))
		(minor mat-a 0 1 3 1 2 3)
		(- (minor mat-a 0 1 2 1 2 3))

		(- (minor mat-a 1 2 3 0 2 3))
		(minor mat-a 0 2 3 0 2 3)
		(- (minor mat-a 0 1 3 0 2 3))
		(minor mat-a 0 1 2 0 2 3)

		(minor mat-a 1 2 3 0 1 3)
		(- (minor mat-a 0 2 3 0 1 3))
		(minor mat-a 0 1 3 0 1 3)
		(- (minor mat-a 0 1 2 0 1 3))

		(- (minor mat-a 1 2 3 0 1 2))
		(minor mat-a 0 2 3 0 1 2)
		(- (minor mat-a 0 1 3 0 1 2))
		(minor mat-a 0 1 2 0 1 2)))

;----------------------------------------------------------------

(defun determinant (mat-a)
  "Returns the determinant of the matrix"
  (+ (* (melm mat-a 0 0) (minor mat-a 1 2 3 1 2 3))
     (- (* (melm mat-a 0 1) (minor mat-a 1 2 3 0 2 3)))
     (* (melm mat-a 0 2) (minor mat-a 1 2 3 0 1 3))
     (- (* (melm mat-a 0 3) (minor mat-a 1 2 3 0 1 2)))))

;----------------------------------------------------------------

;;this one is from 'Essential Maths'
(defun affine-inverse (mat-a)
  "Returns the affine inverse of the matrix"
  ;;calculate upper left 3x3 matrix determinant
  (let* ((cofac-0 (- (* (melm mat-a 1 1) (melm mat-a 2 2))
		     (* (melm mat-a 2 1) (melm mat-a 1 2))))

	 (cofac-4 (- (* (melm mat-a 2 0) (melm mat-a 1 2))
		     (* (melm mat-a 1 0) (melm mat-a 2 2))))

	 (cofac-8 (- (* (melm mat-a 1 0) (melm mat-a 2 1))
		     (* (melm mat-a 2 0) (melm mat-a 1 1))))
	 (det (+ (* (melm mat-a 0 0) cofac-0)
		 (* (melm mat-a 0 1) cofac-4)
		 (* (melm mat-a 0 2) cofac-8))))
    (if 
     (float-zero det)
     (error "Matrix4 Inverse: Singular Matrix")
     (let* 
	 ((inv-det (/ 1.0 det))
	  (r00 (* inv-det cofac-0)) 
	  (r10 (* inv-det cofac-0))
	  (r20 (* inv-det cofac-0))
	  (r01 (* inv-det (- (* (melm mat-a 2 1) (melm mat-a 0 2))
			     (* (melm mat-a 0 1) (melm mat-a 2 2)))))
	  (r11 (* inv-det (- (* (melm mat-a 0 0) (melm mat-a 2 2))
			     (* (melm mat-a 2 0) (melm mat-a 0 2)))))
	  (r21 (* inv-det (- (* (melm mat-a 2 0) (melm mat-a 0 1))
			     (* (melm mat-a 0 0) (melm mat-a 2 1)))))
	  (r02 (* inv-det (- (* (melm mat-a 0 1) (melm mat-a 1 2))
			     (* (melm mat-a 1 1) (melm mat-a 0 2)))))
	  (r12 (* inv-det (- (* (melm mat-a 1 0) (melm mat-a 0 2))
			     (* (melm mat-a 0 0) (melm mat-a 1 2)))))
	  (r22 (* inv-det (- (* (melm mat-a 0 0) (melm mat-a 1 1))
			     (* (melm mat-a 1 0) (melm mat-a 0 1)))))
	  )
       (make-matrix4 r00 r01 r02 
		     (- 0.0 
			(* (melm mat-a 0 0) (melm mat-a 0 3))
			(* (melm mat-a 0 1) (melm mat-a 1 3))
			(* (melm mat-a 0 2) (melm mat-a 2 3)))
		     r10 r11 r12 
		     (- 0.0 
			(* (melm mat-a 1 0) (melm mat-a 0 3))
			(* (melm mat-a 1 1) (melm mat-a 1 3))
			(* (melm mat-a 1 2) (melm mat-a 2 3)))
		      r20 r21 r22 
		     (- 0.0 
			(* (melm mat-a 2 0) (melm mat-a 0 3))
			(* (melm mat-a 2 1) (melm mat-a 1 3))
			(* (melm mat-a 2 2) (melm mat-a 2 3)))
		     0.0 0.0 0.0 0.0)))))

;----------------------------------------------------------------
;; could just feed straight from array into make
(defun transpose (m-a)
  "Returns the transpose of the provided matrix"
  (make-matrix4 
   (melm m-a 0 0) (melm m-a 1 0) (melm m-a 2 0) (melm m-a 3 0)
   (melm m-a 0 1) (melm m-a 1 1) (melm m-a 2 1) (melm m-a 3 1)
   (melm m-a 0 2) (melm m-a 1 2) (melm m-a 2 2) (melm m-a 3 2)
   (melm m-a 0 3) (melm m-a 1 3) (melm m-a 2 3) (melm m-a 3 3)))

;----------------------------------------------------------------

(defun translation (vec3-a)
  "Takes a vector3 and returns a matrix4 which will translate
   by the specified amount"
  (make-matrix4 
   1.0  0.0  0.0  (v-x vec3-a)
   0.0  1.0  0.0  (v-y vec3-a)
   0.0  0.0  1.0  (v-z vec3-a)
   0.0  0.0  0.0  1.0))

;----------------------------------------------------------------

;;need quaternion
;;(defun make-rotation)

;----------------------------------------------------------------

(defun rotation-from-matrix3 (m-a)
  "Takes a 3x3 rotation matrix and returns a 4x4 rotation matrix
   with the same values. The 4th component is filled as an 
   identity matrix would be."
  (make-matrix4 
   (m3:melm m-a 0 0)  (m3:melm m-a 0 1)  (m3:melm m-a 0 2)  0.0
   (m3:melm m-a 1 0)  (m3:melm m-a 1 1)  (m3:melm m-a 1 2)  0.0
   (m3:melm m-a 2 0)  (m3:melm m-a 2 1)  (m3:melm m-a 2 2)  0.0
   0.0                0.0                0.0                1.0))

;----------------------------------------------------------------

(defun rotation-from-euler (vec3-a)
  "This is an unrolled contatenation of rotation matrices x
   y & z. The arguments in the originl were in reverse order"
  (let ((x (v-x vec3-a)) (y (v-y vec3-a)) (z (v-z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
	  (sy (sin y)) (cy (cos y))
	  (sz (sin z)) (cz (cos z)))
      (make-matrix4 (* cy cz)
		    (+ (* sx sy cz) (* cx sz))
		    (- (* sx sz) (* cx sy cz))
		    0.0
		    
		    (- (* cy sz))
		    (- (* cx cz) (* sx sy sz)) ;is this right?
		    (+ (* cx sy sz) (* sx cz))
		    0.0
		    
		    sy
		    (- (* sx cy))
		    (* cx cy)
		    0.0
		    
		    0.0 0.0 0.0 1.0))))

;----------------------------------------------------------------

(defun rotation-from-axis-angle (axis3 angle)
  "Returns a matrix which will rotate a point about the axis
   specified by the angle provided"
  (let* ((c-a (cos angle)) 
	 (s-a (sin angle)) 
	 (tt (- 1.0 c-a))
	 (norm-axis (vector3:normalize axis3))
	 (tx (* tt (v-x norm-axis)))
	 (ty (* tt (v-x norm-axis)))
	 (tz (* tt (v-x norm-axis)))
	 (sx (* s-a (v-x norm-axis)))
	 (sy (* s-a (v-x norm-axis)))
	 (sz (* s-a (v-x norm-axis)))
	 (txy (* tx (v-y norm-axis)))
	 (tyz (* ty (v-z norm-axis)))
	 (txz (* tx (v-z norm-axis))))
    (make-matrix4
     (+ c-a (* tx (v-x norm-axis))) (- txy sz) (+ txz sy) 0.0
     (+ txy xz) (+ c (* ty (v-y norm-axis))) (- tyz sx) 0.0
     (- txz sy) (+ tyz sx) (+ c (* tz (v-z norm-axis))) 0.0
     0.0   0.0   0.0   1.0)))

;----------------------------------------------------------------

(defun scale (scale-vec3)
  "Returns a matrix which will scale by the amounts specified"
  (make-matrix4
   (v-x scale-vec3)  0.0               0.0               0.0
   0.0               (v-y scale-vec3)  0.0               0.0
   0.0               0.0               (v-z scale-vec3)  0.0
   0.0               0.0               0.0               1.0))

;----------------------------------------------------------------

(defun rotation-x (angle)
  "Returns a matrix which would rotate a point around the x axis
   by the specified amount"
  (let ((s-a (sin angle))
	(c-a (cos angle)))
    (make-matrix4 1.0  0.0  0.0     0.0
		  0.0  c-a  (- s-a) 0.0
		  0.0  s-a  c-a     0.0
		  0.0  0.0  0.0     1.0)))

;----------------------------------------------------------------

(defun rotation-y (angle)
  "Returns a matrix which would rotate a point around the y axis
   by the specified amount"
  (let ((s-a (sin angle))
	(c-a (cos angle)))
    (make-matrix4 c-a      0.0  s-a  0.0
		  0.0      1.0  0.0  0.0
		  (- s-a)  0.0  c-a  0.0
		  0.0      0.0  0.0  1.0)))

;----------------------------------------------------------------

(defun rotation-z (angle)
  "Returns a matrix which would rotate a point around the z axis
   by the specified amount"
  (let ((s-a (sin angle))
	(c-a (cos angle)))
    (make-matrix4 c-a  (- s-a)  0.0  0.0
		  s-a  c-a      0.0  0.0
		  0.0  0.0      1.0  0.0
		  0.0  0.0      0.0  1.0)))

;----------------------------------------------------------------

;; [TODO] returned as vector x-y-z

(defun get-fixed-angles (mat-a)
  "Gets one set of possible z-y-x fixed angles that will generate
   this matrix. Assumes that this is a rotation matrix. Result
   is returned as vector3"
  (let* ((sy (melm mat-a 0 2))
	 (cy (base-maths:c-sqrt (- 1.0 (* sy sy)))))
    (if (float-zero cy)
	(let ((sz 0.0)
	      (cz 1.0)
	      (sx (melm mat-a 2 1))
	      (cx (melm mat-a 1 1)))
	  (make-vector3 (atan sx cx) (atan sy cy) (atan sz cz)))
	(let* ((factor (/ 1.0 cy)) ; normal case
	       (sx (- (* factor (melm mat-a 1 2))))
	       (cx (* factor (melm mat-a 2 2)))
	       (sz (- (* factor (melm mat-a 0 1))))
	       (cz (* factor (melm mat-a 0 0))))
	  (make-vector3 (atan sx cx) (atan sy cy) (atan sz cz))))))

;----------------------------------------------------------------

(defun mtrace (mat-a)
  "Returns the trace of the matrix (That is the diagonal values)"
  (+ (melm mat-a 0 0) (melm mat-a 1 1) 
     (melm mat-a 2 2) (melm mat-a 3 3)))

;----------------------------------------------------------------

;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works

(defun get-axis-angle (mat-a)
  "Gets one possible axis-angle pair that will generate this 
   matrix. Assumes that this is a rotation matrix"
  (let* ((trace-a (+ (melm mat-a 0 0) (melm mat-a 1 1) 
			    (melm mat-a 2 2)))
	 (cos-theta (* 0.5 (- trace-a 1.0)))
	 (angle (acos cos-theta)))
    (cond ((float-zero angle) (values vector3:*unit-x* angle))
	  ((float-zero (- base-maths:+pi+ angle))
	   (values 
	    (vector3:normalize
	     (make-vector3 (- (melm mat-a 2 1) (melm mat-a 1 2))
			   (- (melm mat-a 0 2) (melm mat-a 2 0))
			   (- (melm mat-a 1 0) (melm mat-a 0 1))))
	    angle))
	  (t (labels ((biggest-trace (matr)
			(let ((x 0))
			  (if (> (melm matr 1 1) (melm matr 0 0))
			      (setf x 1))
			  (if (> (melm matr 2 2) (melm matr x x))
			      (setf x 2))
			  x)))
	       (let* ((i (biggest-trace mat-a))
		      (j (mod (+ i 1) 3))
		      (k (mod (+ i 1) 3))
		      (s (c-sqrt (+ 1.0 (- (melm mat-a i i)
					   (melm mat-a j j)
					   (melm mat-a k k)))))
		      (recip (/ 1.0 s)))
		 (values (make-vector3 
			  (* 0.5 s)
			  (* recip (aref mat-a (+ i (* 4 j))))
			  (* recip (aref mat-a (+ k (* 4 i)))))
			 angle)))))))


;----------------------------------------------------------------

(defun m+ (mat-a mat-b)
  "Adds the 2 matrices component wise and returns the result as
   a new matrix"
  (let ((r (zero-matrix4)))
    (loop for i below 16
	 do (setf (aref r i) (+ (aref mat-a i) (aref mat-b i))))
    r))

;----------------------------------------------------------------

(defun m- (mat-a mat-b)
  "Subtracts the 2 matrices component wise and returns the result
   as a new matrix"
  (let ((r (zero-matrix4)))
    (loop for i below 16
       do (setf (aref r i) (- (aref mat-a i) (aref mat-b i))))
    r))

;----------------------------------------------------------------

(defun negate (mat-a)
  "Negates the components of the matrix"
  (let ((r (zero-matrix4)))
    (loop for i below 16
       do (setf (aref r i) (- (aref mat-a i))))
    r))

;----------------------------------------------------------------

(defun m*scalar (mat-a scalar)
  "Multiplies the components of the matrix by the scalar 
   provided"
  (let ((result (zero-matrix4)))
    (loop for i below 16
	 do (setf (aref result i) (* scalar (aref mat-a i))))
    result))

;----------------------------------------------------------------

(defun mcol*vec4 (mat-a vec)
  (make-vector4 
   (+ (* (v-x vec) (melm mat-a 0 0)) (* (v-y vec) (melm mat-a 0 1))
      (* (v-z vec) (melm mat-a 0 2)) (* (v-w vec) (melm mat-a 0 3)))

   (+ (* (v-x vec) (melm mat-a 1 0)) (* (v-y vec) (melm mat-a 1 1))
      (* (v-z vec) (melm mat-a 1 2)) (* (v-w vec) (melm mat-a 1 3)))

   (+ (* (v-x vec) (melm mat-a 2 0)) (* (v-y vec) (melm mat-a 2 1))
      (* (v-z vec) (melm mat-a 2 2)) (* (v-w vec) (melm mat-a 2 3)))

   (+ (* (v-x vec) (melm mat-a 3 0)) (* (v-y vec) (melm mat-a 3 1))
      (* (v-z vec) (melm mat-a 3 2)) (* (v-w vec) (melm mat-a 3 3)))
   ))

;----------------------------------------------------------------

(defun mrow*vec4 (vec mat-a)
  (make-vector4 
   (+ (* (v-x vec) (melm mat-a 0 0)) (* (v-y vec) (melm mat-a 1 0))
      (* (v-z vec) (melm mat-a 2 0)) (* (v-w vec) (melm mat-a 3 0)))

   (+ (* (v-x vec) (melm mat-a 0 1)) (* (v-y vec) (melm mat-a 1 1))
      (* (v-z vec) (melm mat-a 2 1)) (* (v-w vec) (melm mat-a 3 1)))

   (+ (* (v-x vec) (melm mat-a 0 2)) (* (v-y vec) (melm mat-a 1 2))
      (* (v-z vec) (melm mat-a 2 2)) (* (v-w vec) (melm mat-a 3 2)))

   (+ (* (v-x vec) (melm mat-a 0 3)) (* (v-y vec) (melm mat-a 1 3))
      (* (v-z vec) (melm mat-a 2 3)) (* (v-w vec) (melm mat-a 3 3)))
   ))

;----------------------------------------------------------------

(defun m* (mat-a mat-b)
  "Multiplies 2 matrices and returns the result as a new 
   matrix"
  (make-matrix4 (+ (* (melm mat-a 0 0) (melm mat-b 0 0)) 
		   (* (melm mat-a 0 1) (melm mat-b 1 0))
		   (* (melm mat-a 0 2) (melm mat-b 2 0)) 
		   (* (melm mat-a 0 3) (melm mat-b 3 0)))
		(+ (* (melm mat-a 0 0) (melm mat-b 0 1))
		   (* (melm mat-a 0 1) (melm mat-b 1 1)) 
		   (* (melm mat-a 0 2) (melm mat-b 2 1)) 
		   (* (melm mat-a 0 3) (melm mat-b 3 1)))
		(+ (* (melm mat-a 0 0) (melm mat-b 0 2)) 
		   (* (melm mat-a 0 1) (melm mat-b 1 2)) 
		   (* (melm mat-a 0 2) (melm mat-b 2 2)) 
		   (* (melm mat-a 0 3) (melm mat-b 3 2)))
		(+ (* (melm mat-a 0 0) (melm mat-b 0 3)) 
		   (* (melm mat-a 0 1) (melm mat-b 1 3)) 
		   (* (melm mat-a 0 2) (melm mat-b 2 3)) 
		   (* (melm mat-a 0 3) (melm mat-b 3 3)))
		(+ (* (melm mat-a 1 0) (melm mat-b 0 0)) 
		   (* (melm mat-a 1 1) (melm mat-b 1 0)) 
		   (* (melm mat-a 1 2) (melm mat-b 2 0)) 
		   (* (melm mat-a 1 3) (melm mat-b 3 0)))
		(+ (* (melm mat-a 1 0) (melm mat-b 0 1)) 
		   (* (melm mat-a 1 1) (melm mat-b 1 1)) 
		   (* (melm mat-a 1 2) (melm mat-b 2 1)) 
		   (* (melm mat-a 1 3) (melm mat-b 3 1)))
		(+ (* (melm mat-a 1 0) (melm mat-b 0 2)) 
		   (* (melm mat-a 1 1) (melm mat-b 1 2)) 
		   (* (melm mat-a 1 2) (melm mat-b 2 2)) 
		   (* (melm mat-a 1 3) (melm mat-b 3 2)))
		(+ (* (melm mat-a 1 0) (melm mat-b 0 3)) 
		   (* (melm mat-a 1 1) (melm mat-b 1 3)) 
		   (* (melm mat-a 1 2) (melm mat-b 2 3)) 
		   (* (melm mat-a 1 3) (melm mat-b 3 3)))
		(+ (* (melm mat-a 2 0) (melm mat-b 0 0)) 
		   (* (melm mat-a 2 1) (melm mat-b 1 0)) 
		   (* (melm mat-a 2 2) (melm mat-b 2 0)) 
		   (* (melm mat-a 2 3) (melm mat-b 3 0)))
		(+ (* (melm mat-a 2 0) (melm mat-b 0 1)) 
		   (* (melm mat-a 2 1) (melm mat-b 1 1)) 
		   (* (melm mat-a 2 2) (melm mat-b 2 1)) 
		   (* (melm mat-a 2 3) (melm mat-b 3 1)))
		(+ (* (melm mat-a 2 0) (melm mat-b 0 2)) 
		   (* (melm mat-a 2 1) (melm mat-b 1 2)) 
		   (* (melm mat-a 2 2) (melm mat-b 2 2)) 
		   (* (melm mat-a 2 3) (melm mat-b 3 2)))
		(+ (* (melm mat-a 2 0) (melm mat-b 0 3)) 
		   (* (melm mat-a 2 1) (melm mat-b 1 3)) 
		   (* (melm mat-a 2 2) (melm mat-b 2 3)) 
		   (* (melm mat-a 2 3) (melm mat-b 3 3)))
		(+ (* (melm mat-a 3 0) (melm mat-b 0 0)) 
		   (* (melm mat-a 3 1) (melm mat-b 1 0)) 
		   (* (melm mat-a 3 2) (melm mat-b 2 0)) 
		   (* (melm mat-a 3 3) (melm mat-b 3 0)))
		(+ (* (melm mat-a 3 0) (melm mat-b 0 1)) 
		   (* (melm mat-a 3 1) (melm mat-b 1 1)) 
		   (* (melm mat-a 3 2) (melm mat-b 2 1)) 
		   (* (melm mat-a 3 3) (melm mat-b 3 1)))
		(+ (* (melm mat-a 3 0) (melm mat-b 0 2)) 
		   (* (melm mat-a 3 1) (melm mat-b 1 2)) 
		   (* (melm mat-a 3 2) (melm mat-b 2 2)) 
		   (* (melm mat-a 3 3) (melm mat-b 3 2)))
		(+ (* (melm mat-a 3 0) (melm mat-b 0 3))
		   (* (melm mat-a 3 1) (melm mat-b 1 3)) 
		   (* (melm mat-a 3 2) (melm mat-b 2 3)) 
		   (* (melm mat-a 3 3) (melm mat-b 3 3)))))

;----------------------------------------------------------------

(defun transform (mat-a vec)
  "Returns the transform of a matrix"
  (make-vector3 (+ (* (melm mat-a 0 0) (v-x vec))
		   (* (melm mat-a 0 1) (v-y vec))
		   (* (melm mat-a 0 2) (v-z vec))
		   (melm mat-a 0 3))
		(+ (* (melm mat-a 1 0) (v-x vec))
		   (* (melm mat-a 1 1) (v-y vec))
		   (* (melm mat-a 1 2) (v-z vec))
		   (melm mat-a 1 3))
		(+ (* (melm mat-a 2 0) (v-x vec))
		   (* (melm mat-a 2 1) (v-y vec))
		   (* (melm mat-a 2 2) (v-z vec))
		   (melm mat-a 2 3))))

;----------------------------------------------------------------

