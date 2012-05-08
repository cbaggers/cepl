(in-package #:cepl-matrix3)

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

(defun make-matrix3 ()
  (make-array 9 :initial-element 0))

;----------------------------------------------------------------

(defun matrix-row (matrix row-num)
  (assert (and (>= row-num 0) (< row-num 3)))
  (let ((offset (* row-num 3)))
    (vector (aref matrix offset)
	    (aref matrix (+ 1 offset))
	    (aref matrix (+ 2 offset)))))

;----------------------------------------------------------------

(defun matrix-column (matrix col-num)
  (assert (and (>= col-num 0) (< col-num 3)))
  (vector (aref matrix col-num)
	  (aref matrix (+ 3 col-num))
	  (aref matrix (+ 6 col-num))))

;----------------------------------------------------------------

(defun matrix-element (matrix row col)
  (aref matrix (+ col (* row 3))))

;----------------------------------------------------------------

(defun c-identity ()
  #(1 0 0 0 1 0 0 0 1))

;----------------------------------------------------------------

(defun c-= (matrix-a matrix-b)
  (loop for i 
     below 9
     if (/= (aref matrix-a i) (aref matrix-b i))
     do (return nil)
     finally (return t)))

;----------------------------------------------------------------

(defun c-+ (matrix-a matrix-b)
  (let ((r (make-matrix3)))
    (loop for i below 9
	 do (setf (aref r i) (+ (aref matrix-a i) 
				 (aref matrix-b i))))
    r))

;----------------------------------------------------------------

(defun c-- (matrix-a matrix-b)
  (let ((r (make-matrix3)))
    (loop for i below 9
	 do (setf (aref r i) (- (aref matrix-a i) 
				 (aref matrix-b i))))
    r))

;----------------------------------------------------------------

(defun c-* (matrix-a matrix-b)
  (let ((result (make-matrix3)))
    (loop for row-num below 3
	 (loop for col-num below 3
	      do (let ((row (matrix-row matrix-a))
		       (col (matrix-column matrix b)))
		   (setf (aref result (+ col (* row 3)) 
				(+ ()))))))
    result))

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
