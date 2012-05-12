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

(defmacro matrix-elm (matrix-a row col)
  (cond ((and (numberp row) (numberp col)) 
	 `(aref ,matrix-a ,(+ row (* col 3))))
	((numberp col)
	 `(aref ,matrix-a (+ ,row ,(* col 3))))
	(t `(aref ,matrix-a (+ ,row (* ,col 3))))))

;----------------------------------------------------------------

(defun identity-matrix3 ()
  #(1 0 0 0 1 0 0 0 1))

(defun zero-matrix3 ()
  #(0 0 0 0 0 0 0 0 0))

;----------------------------------------------------------------

(defun make-matrix3 ( a b c d e f g h i )
  (let ((result (zero-matrix3)))
    (setf (matrix-elm result 0 0) a)
    (setf (matrix-elm result 0 1) b)
    (setf (matrix-elm result 0 2) c)
    (setf (matrix-elm result 1 0) d)
    (setf (matrix-elm result 1 1) e)
    (setf (matrix-elm result 1 2) f)
    (setf (matrix-elm result 2 0) g)
    (setf (matrix-elm result 2 1) h)
    (setf (matrix-elm result 2 2) i)
    result))

;----------------------------------------------------------------

(defun make-from-rows (row-1 row-2 row-3)
  (make-matrix3 (cepl-vec3:c-x row-1)
		(cepl-vec3:c-y row-1)
		(cepl-vec3:c-z row-1) 
		(cepl-vec3:c-x row-2)
		(cepl-vec3:c-y row-2)
		(cepl-vec3:c-z row-2)
		(cepl-vec3:c-x row-3)
		(cepl-vec3:c-y row-3)
		(cepl-vec3:c-z row-3)))

;----------------------------------------------------------------

(defun get-rows (matrix-a)
   (list (cepl-vec3:make-vector3 (matrix-elm matrix-a 0 0)
				 (matrix-elm matrix-a 1 0)
				 (matrix-elm matrix-a 2 0))
	 (cepl-vec3:make-vector3 (matrix-elm matrix-a 0 1)
				 (matrix-elm matrix-a 1 1)
				 (matrix-elm matrix-a 2 1))
	 (cepl-vec3:make-vector3 (matrix-elm matrix-a 0 2)
				 (matrix-elm matrix-a 1 2)
				 (matrix-elm matrix-a 2 2))))

;----------------------------------------------------------------

(defun get-row (matrix-a row-num)
  (cepl-vec3:make-vector3 (matrix-elm matrix-a 0 row-num)
			  (matrix-elm matrix-a 1 row-num)
			  (matrix-elm matrix-a 2 row-num)))

;----------------------------------------------------------------

(defun make-from-columns (col-1 col-2 col-3)
  (make-matrix3 (cepl-vec3:c-x col-1)
		(cepl-vec3:c-x col-2)
		(cepl-vec3:c-x col-3) 
		(cepl-vec3:c-y col-1)
		(cepl-vec3:c-y col-2)
		(cepl-vec3:c-y col-3)
		(cepl-vec3:c-z col-1)
		(cepl-vec3:c-z col-2)
		(cepl-vec3:c-z col-3)))

;----------------------------------------------------------------

(defun get-columns (matrix-a)
   (list (cepl-vec3:make-vector3 (matrix-elm matrix-a 0 0)
				 (matrix-elm matrix-a 0 1)
				 (matrix-elm matrix-a 0 2))
	 (cepl-vec3:make-vector3 (matrix-elm matrix-a 1 0)
				 (matrix-elm matrix-a 1 1)
				 (matrix-elm matrix-a 1 2))
	 (cepl-vec3:make-vector3 (matrix-elm matrix-a 2 0)
				 (matrix-elm matrix-a 2 1)
				 (matrix-elm matrix-a 2 2))))

;----------------------------------------------------------------

(defun get-column (matrix-a col-num)
  (cepl-vec3:make-vector3 (matrix-elm matrix-a col-num 0)
			  (matrix-elm matrix-a col-num 1)
			  (matrix-elm matrix-a col-num 2)))

;----------------------------------------------------------------

(defun c-zerop (matrix-a)
  (loop for i 
     below 9
     if (not (float-zero (aref matrix-a i)))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------

(defun c-identityp (matrix-a)
  (and (= (aref matrix-a 0) 1.0)
       (= (aref matrix-a 4) 1.0)
       (= (aref matrix-a 8) 1.0)
       (float-zero (aref matrix-a 1))
       (float-zero (aref matrix-a 2))
       (float-zero (aref matrix-a 3))
       (float-zero (aref matrix-a 5))
       (float-zero (aref matrix-a 6))
       (float-zero (aref matrix-a 7))))

;----------------------------------------------------------------

(defun c-eql (matrix-a matrix-b)
  (loop for i 
     below 9
     if (/= (aref matrix-a i) (aref matrix-b i))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------

;;[TODO] should definately inline this 
;;[TODO] Would it be faster not to have to cofactors too?
(defun determinate-cramer (matrix-a)
  (let ((cofactor-0 (- (* (matrix-elm matrix-a 1 1)
			  (matrix-elm matrix-a 2 2))
		       (* (matrix-elm matrix-a 1 2)
			  (matrix-elm matrix-a 2 1))))
	(cofactor-3 (- (* (matrix-elm matrix-a 0 2)
			  (matrix-elm matrix-a 2 1))
		       (* (matrix-elm matrix-a 0 1)
			  (matrix-elm matrix-a 2 2))))
	(cofactor-6 (- (* (matrix-elm matrix-a 0 1)
			  (matrix-elm matrix-a 1 2))
		       (* (matrix-elm matrix-a 0 2)
			  (matrix-elm matrix-a 1 1)))))
    (values (+ (* (matrix-elm matrix-a 0 0) cofactor-0)
	       (* (matrix-elm matrix-a 1 0) cofactor-3)
	       (* (matrix-elm matrix-a 2 0) cofactor-6))
	    cofactor-0 cofactor-3 cofactor-6)))

;----------------------------------------------------------------

;;[TODO] Look more into errors
(defun inverve (matrix-a)
  (multiple-value-bind (det cofactor-0 cofactor-3 cofactor-6)
      (determinate-cramer matrix-a)
    (if (float-zero det)
	(error "Matrix Inverse: Singular Matrix (determinate is 0)"))
    (let ((inv-det (/ 1.0 det)))
      (make-matrix3 (* inv-det cofactor-0)
		    (* inv-det cofactor-3)
		    (* inv-det cofactor-6)
		    (* inv-det (- (* (matrix-elm matrix-a 1 2)
				     (matrix-elm matrix-a 2 0))
				  (* (matrix-elm matrix-a 1 0)
				     (matrix-elm matrix-a 2 2))))
		    (* inv-det (- (* (matrix-elm matrix-a 0 0)
				     (matrix-elm matrix-a 2 2))
				  (* (matrix-elm matrix-a 0 2)
				     (matrix-elm matrix-a 2 0))))
		    (* inv-det (- (* (matrix-elm matrix-a 0 2)
				     (matrix-elm matrix-a 1 0))
				  (* (matrix-elm matrix-a 0 0 )
				     (matrix-elm matrix-a 1 2))))
		    (* inv-det (- (* (matrix-elm matrix-a 1 0)
				     (matrix-elm matrix-a 2 1))
				  (* (matrix-elm matrix-a 1 1)
				     (matrix-elm matrix-a 2 0))))
		    (* inv-det (- (* (matrix-elm matrix-a 0 1)
				     (matrix-elm matrix-a 2 0))
				  (* (matrix-elm matrix-a 0 0)
				     (matrix-elm matrix-a 2 1))))
		    (* inv-det (- (* (matrix-elm matrix-a 0 0)
				     (matrix-elm matrix-a 1 1))
				  (* (matrix-elm matrix-a 0 1)
				     (matrix-elm matrix-a 1 0))))))))

;----------------------------------------------------------------

(defun transpose (matrix-a)
  (make-matrix3 (matrix-elm matrix-a 0 0)
		(matrix-elm matrix-a 1 0)
		(matrix-elm matrix-a 2 0)
		(matrix-elm matrix-a 0 1)
		(matrix-elm matrix-a 1 1)
		(matrix-elm matrix-a 2 1)
		(matrix-elm matrix-a 0 2)
		(matrix-elm matrix-a 1 2)
		(matrix-elm matrix-a 2 2)))

;----------------------------------------------------------------

;;This is taken straight from 'Essential Mathematics for Game..'
;; Must be a more efficient way :)
(defun adjoint (matrix-a)
  (make-matrix3  (- (* (matrix-elm matrix-a 1 1)
		       (matrix-elm matrix-a 2 2))
		    (* (matrix-elm matrix-a 1 2)
		       (matrix-elm matrix-a 2 1)))
		 (- (* (matrix-elm matrix-a 0 2)
		       (matrix-elm matrix-a 2 1))
		    (* (matrix-elm matrix-a 0 1)
		       (matrix-elm matrix-a 2 2)))
		 (- (* (matrix-elm matrix-a 0 1)
		       (matrix-elm matrix-a 1 2))
		    (* (matrix-elm matrix-a 0 2)
		       (matrix-elm matrix-a 1 1)))
		 (- (* (matrix-elm matrix-a 1 2)
		       (matrix-elm matrix-a 2 0))
		    (* (matrix-elm matrix-a 1 0)
		       (matrix-elm matrix-a 2 2)))
		 (- (* (matrix-elm matrix-a 0 0)
		       (matrix-elm matrix-a 2 2))
		    (* (matrix-elm matrix-a 0 2)
		       (matrix-elm matrix-a 2 0)))
		 (- (* (matrix-elm matrix-a 0 2)
		       (matrix-elm matrix-a 1 0))
		    (* (matrix-elm matrix-a 0 0)
		       (matrix-elm matrix-a 1 2)))
		 (- (* (matrix-elm matrix-a 1 0)
		       (matrix-elm matrix-a 2 1))
		    (* (matrix-elm matrix-a 1 1)
		       (matrix-elm matrix-a 2 0)))
		 (- (* (matrix-elm matrix-a 0 1)
		       (matrix-elm matrix-a 2 0))
		    (* (matrix-elm matrix-a 0 0)
		       (matrix-elm matrix-a 2 1)))
		 (- (* (matrix-elm matrix-a 0 0)
		       (matrix-elm matrix-a 1 1))
		    (* (matrix-elm matrix-a 0 1)
		       (matrix-elm matrix-a 1 0)))))

;----------------------------------------------------------------

(defun trace (matrix-a)
  (+ (matrix-elm matrix-a 0 0)
     (matrix-elm matrix-a 1 1)
     (matrix-elm matrix-a 2 2)))

;----------------------------------------------------------------

;;Rotation goes here, requires quaternion

;----------------------------------------------------------------

(defun rotation-euler (x y z)
  (let ((cx (cos x))
	(cy (cos y))
	(cz (cos z))
	(sx (sin x))
	(sy (sin y))
	(sz (sin z)))
    (make-matrix3 (* cy cz)
		  (- (* cy sz))
		  sy
		  (+ (* sx sy cz) (* cx sz))
		  (+ (- (* sx sy sz)) (* cx sz))
		  (- (* sx cy))
		  (+ (- (* cx sy cz)) (* sx sz))
		  (+ (* cx sy sz) (* sx cz))
		  (* cx cy))))

;----------------------------------------------------------------

(defun c-+ (matrix-a matrix-b)
  (let ((r (zero-matrix3)))
    (loop for i below 9
	 do (setf (aref r i) (+ (aref matrix-a i) 
				 (aref matrix-b i))))
    r))

;----------------------------------------------------------------

(defun c-- (matrix-a matrix-b)
  (let ((r (zero-matrix3)))
    (loop for i below 9
	 do (setf (aref r i) (- (aref matrix-a i) 
				 (aref matrix-b i))))
    r))

;----------------------------------------------------------------

(defun c-* (matrix-a matrix-b)
  (let ((result (zero-matrix3)))
    (loop for row-num below 3
	 (loop for col-num below 3
	    do (let ((row (get-row matrix-a row-num))
		     (col (get-column matrix-b col-num)))
		 (setf (matrix-elm result row-num col-num)
		       (+ (* (aref row 0) (aref col 0))
			  (* (aref row 1) (aref col 1))
			  (* (aref row 2) (aref col 2)))))))
    result))

;----------------------------------------------------------------

;;(defun c-*vec ())

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
