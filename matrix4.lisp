(in-package :matrix4)

;----------------------------------------------------------------

(defmacro melm (mat-a row col)
  (cond ((and (numberp row) (numberp col)) 
	 `(aref ,mat-a ,(+ row (* col 4))))
	((numberp col)
	 `(aref ,mat-a (+ ,row ,(* col 4))))
	(t `(aref ,mat-a (+ ,row (* ,col 4))))))

;----------------------------------------------------------------

;----------------------------------------------------------------

(defun identity-matrix4 ()
  #(1.0 0.0 0.0 0.0
    0.0 1.0 0.0 0.0
    0.0 0.0 1.0 0.0
    0.0 0.0 0.0 1.0))

(defun zero-matrix4 ()
  #(0.0 0.0 0.0 0.0
    0.0 0.0 0.0 0.0
    0.0 0.0 0.0 0.0
    0.0 0.0 0.0 0.0))

(defun 2dclipspace-to-imagespace-matrix4 ()
  #(0.5  0.0  0.0  0.5
    0.0 -0.0  0.0  0.5
    0.0  0.0  1.0  0.0
    0.0  0.0  0.0  1.0))

;----------------------------------------------------------------

(defun make-matrix4 ( a b c d e f g h i j k l m n o p )
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

(defun mzerop (mat-a)
  (loop for i below 16
     if (not (float-zero (aref mat-a i)))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------


(defun identityp (mat-a)
  (and (= (melm mat-a 0 0) 1.0)
       (= (melm mat-a 1 1) 1.0)
       (= (melm mat-a 2 2) 1.0)
       (= (melm mat-a 3 3) 1.0)
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
  (+ (* (melm mat-a 0 0) (minor mat-a 1 2 3 1 2 3))
     (- (* (melm mat-a 0 1) (minor mat-a 1 2 3 0 2 3)))
     (* (melm mat-a 0 2) (minor mat-a 1 2 3 0 1 3))
     (- (* (melm mat-a 0 3) (minor mat-a 1 2 3 0 1 2)))))

;----------------------------------------------------------------

;;this one is from 'Essential Maths'
(defun affine-inverse (mat-a)
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
  (make-matrix4 
   (melm m-a 0 0) (melm m-a 1 0) (melm m-a 2 0) (melm m-a 3 0)
   (melm m-a 0 1) (melm m-a 1 1) (melm m-a 2 1) (melm m-a 3 1)
   (melm m-a 0 2) (melm m-a 1 2) (melm m-a 2 2) (melm m-a 3 2)
   (melm m-a 0 3) (melm m-a 1 3) (melm m-a 2 3) (melm m-a 3 3)))

;----------------------------------------------------------------

(defun make-translation (vec-a)
  (make-matrix4 
   1.0  0.0  0.0  (v-x vec-a)
   0.0  1.0  0.0  (v-y vec-a)
   0.0  0.0  1.0  (v-z vec-a)
   0.0  0.0  0.0  1.0))

;----------------------------------------------------------------

;;need quaternion
;;(defun make-rotation)

;----------------------------------------------------------------

(defun rotation-from-matrix3 (m-a)
  (make-matrix4 
   (melm m-a 0 0)  (melm m-a 0 1)  (melm m-a 0 2)  0.0
   (melm m-a 1 0)  (melm m-a 1 1)  (melm m-a 1 2)  0.0
   (melm m-a 2 0)  (melm m-a 2 1)  (melm m-a 2 2)  0.0
   0.0             0.0             0.0             1.0))

;----------------------------------------------------------------

(defun rotation-from-euler (x y z)
  "This is an unrolled contatenation of rotation matrices x
   y & z. The arguments in the originl were in reverse order"
  (let ((sx (sin x)) (cx (cos x))
	(sy (sin y)) (cy (cos y))
	(sz (sin z)) (cz (cos z)))
    (make-matrix4 (* cy cz)  (- (* cy sz))  sy  0.0
		  
		  (+ (* sx sy sz) (* cx sz))
		  (- (+ (* sx sy sz) (* cx sz))) ;is this right?
		  (- (* sx cy))
		  0.0

		  (- (+ (* cx sy cz) (* sx sz)))
		  (+ (* cx sy sz) (* sx cz))
		  (* cx cy)
		  0.0

		  0.0  0.0  0.0  1.0)))

;----------------------------------------------------------------

(defun rotation-from-axis-angle (axis3 angle)
  (let* ((ca (cos angle)) (sa (sin angle)) (ta (- 1.0 ca))
	 (n-axis (vector3:normalize axis3))
	 (tx (* ta (v-x n-axis)))
	 (ty (* ta (v-x n-axis)))
	 (tz (* ta (v-x n-axis)))
	 (sx (* sa (v-x n-axis)))
	 (sy (* sa (v-x n-axis)))
	 (sz (* sa (v-x n-axis)))
	 (txy (* tx (v-y n-axis)))
	 (tyz (* tx (v-z n-axis))) ;this doesnt look right
	 (txz (* tx (v-z n-axis))))
    (make-matrix4
     (+ ca  (* tx (v-x n-axis)))  (- txy sz)  (+ txz sy)  0.0
     (+ txy sz)  (+ ca (* ty (v-y n-axis)))  (- tyz sx)  0.0
     (- txz sy)  (+ tyz sx)  (+ ca (* tz (v-z n-axis)))  0.0
     0.0  0.0  0.0  1.0)))

;----------------------------------------------------------------

(defun scaling (scale-vec3)
  (make-matrix4
   (v-x scale-vec3)  0.0               0.0               0.0
   0.0               (v-y scale-vec3)  0.0               0.0
   0.0               0.0               (v-z scale-vec3)  0.0
   0.0               0.0               0.0               1.0))

;----------------------------------------------------------------

(defun make-rotation-x (angle)
  (let ((s-a (sin angle))
	(c-a (cos angle)))
    (make-matrix4 1.0  0.0  0.0     0.0
		  0.0  c-a  (- s-a) 0.0
		  0.0  s-a  c-a     0.0
		  0.0  0.0  0.0     1.0)))

;----------------------------------------------------------------

(defun make-rotation-y (angle)
  (let ((s-a (sin angle))
	(c-a (cos angle)))
    (make-matrix4 c-a      0.0  s-a  0.0
		  0.0      1.0  0.0  0.0
		  (- s-a)  0.0  c-a  0.0
		  0.0      0.0  0.0  1.0)))

;----------------------------------------------------------------

(defun make-rotation-z (angle)
  (let ((s-a (sin angle))
	(c-a (cos angle)))
    (make-matrix4 c-a  (- s-a)  0.0  0.0
		  s-a  c-a      0.0  0.0
		  0.0  0.0      1.0  0.0
		  0.0  0.0      0.0  1.0)))

;----------------------------------------------------------------

(defun get-fixed-angles (mat-a)
  (let* ((sy (melm mat-a 0 2))
	 (cy (base:c-sqrt (- 1.0 (* sy sy)))))
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
  (+ (melm mat-a 0 0) (melm mat-a 1 1) 
     (melm mat-a 2 2) (melm mat-a 3 3)))

;----------------------------------------------------------------

(defun get-axis-angle (mat-a)
  (let* ((trace-a (+ (melm mat-a 0 0) (melm mat-a 1 1) 
			    (melm mat-a 2 2)))
	 (cos-theta (* 0.5 (- trace-a 1.0)))
	 (angle (acos cos-theta)))
    (cond ((float-zero angle) (values vector3:*unit-x* angle))
	  ((float-zero (- base:+pi+ angle))
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
  (let ((r (zero-matrix4)))
    (loop for i below 16
	 do (setf (aref r i) (+ (aref mat-a i) (aref mat-b i))))
    r))

;----------------------------------------------------------------

(defun m- (mat-a mat-b)
  (let ((r (zero-matrix4)))
    (loop for i below 16
       do (setf (aref r i) (- (aref mat-a i) (aref mat-b i))))
    r))

;----------------------------------------------------------------
;; [TODO] why cant this be named negate?
(defun negate4 (mat-a)
  (let ((r (zero-matrix4)))
    (loop for i below 16
       do (setf (aref r i) (- (aref mat-a i))))
    r))

;----------------------------------------------------------------

(defun m*scalar (mat-a scalar)
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

;----------------------------------------------------------------
;; 0  4  8 12
;; 1  5  9 13
;; 2  6 10 14
;; 3  7 11 15
;----------------------------------------------------------------
;----------------------------------------------------------------
;----------------------------------------------------------------
