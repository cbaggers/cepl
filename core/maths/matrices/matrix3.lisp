;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is the 3x3 matrix functionality.
;; There will be a generic function-set to make this as easy
;; as possible for people writing the games but this will
;; be in a seperate package (prehaps the base-maths one)

(in-package :matrix3)

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

;; All matrices are stored in column-major format, but these functions
;; expect row major format. The reason is that row-major is easier to
;; read in code and matches with lots of examples in maths text books.


(declaim (inline melm)
	 (ftype (function ((simple-array single-float (9)) (integer 0 3) (integer 0 3))
                      single-float)
		melm))
(defun melm (mat-a row col)
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare ((simple-array single-float (9)) mat-a)
           ((integer 0 3) row col))
  (aref mat-a (+ row (* col 3))))

(defun (setf melm) (value mat-a row col)
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare ((simple-array single-float (9)) mat-a)
           ((integer 0 3) row col)
           (single-float value))
  (setf (aref mat-a (+ row (* col 3))) value))

(define-compiler-macro melm (mat-a row col)
  "Provide access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (cond ((and (numberp row) (numberp col))
         `(aref ,mat-a ,(+ row (* col 3))))
        ((numberp col)
         `(aref ,mat-a (+ ,row ,(* col 3))))
        (t `(aref ,mat-a (+ ,row (* ,col 3))))))

;;----------------------------------------------------------------

(declaim
 (inline identity-matrix3)
 (ftype (function () (simple-array single-float (9)))
        identity-matrix3))
(defun identity-matrix3 ()
  "Return a 3x3 identity matrix"
  (make-matrix3
   1.0 0.0 0.0
   0.0 1.0 0.0
   0.0 0.0 1.0))

(declaim
 (inline zero-matrix3)
 (ftype (function () (simple-array single-float (9)))
        zero-matrix3))
(defun zero-matrix3 ()
  "Return a 3x3 zero matrix"
  (make-array 9 :element-type `single-float :initial-element 0.0))

;;----------------------------------------------------------------

(declaim
 (inline make-matrix3)
 (ftype (function
         (single-float single-float single-float
                       single-float single-float single-float
                       single-float single-float single-float)
         (simple-array single-float (9)))
        make-matrix3))
(defun make-matrix3 ( a b c d e f g h i )
  "Make a 3x3 matrix. Data must be provided in row major order"
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

;;----------------------------------------------------------------

(declaim
 (inline make-from-rows)
 (ftype (function ((simple-array single-float (3))
                   (simple-array single-float (3))
                   (simple-array single-float (3)))
                  (simple-array single-float (9)))
        make-from-rows))
(defun make-from-rows (row-1 row-2 row-3)
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the rows"
  (declare ((simple-array single-float (3)) row-1 row-2 row-3))
  (make-matrix3 (v-x row-1) (v-y row-1) (v-z row-1)
                (v-x row-2) (v-y row-2)	(v-z row-2)
                (v-x row-3) (v-y row-3) (v-z row-3)))

;;----------------------------------------------------------------

(defun get-rows (mat-a)
  "Return the rows of the matrix a 3 vector3s"
  (declare ((simple-array single-float (9)) mat-a))
  (list (make-vector3 (melm mat-a 0 0)
                      (melm mat-a 0 1)
                      (melm mat-a 0 2))
        (make-vector3 (melm mat-a 1 0)
                      (melm mat-a 1 1)
                      (melm mat-a 1 2))
        (make-vector3 (melm mat-a 2 0)
                      (melm mat-a 2 1)
                      (melm mat-a 2 2))))

;;----------------------------------------------------------------

(declaim
 (inline get-rows)
 (ftype (function ((simple-array single-float (9)) (integer 0 3))
                  (simple-array single-float (3)))
        get-row))
(defun get-row (mat-a row-num)
  "Return the specified row of the matrix a vector3"
  (declare ((simple-array single-float (9)) mat-a)
           ((integer 0 3) row-num))
  (make-vector3 (melm mat-a row-num 0)
                (melm mat-a row-num 1)
                (melm mat-a row-num 2)))

;;----------------------------------------------------------------

(declaim
 (inline make-from-columns)
 (ftype (function ((simple-array single-float (3))
                   (simple-array single-float (3))
                   (simple-array single-float (3)))
                  (simple-array single-float (9)))
        make-from-columns))
(defun make-from-columns (col-1 col-2 col-3)
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the columns"
  (declare ((simple-array single-float (3)) col-1 col-2 col-3))
  (make-matrix3 (v-x col-1)
                (v-x col-2)
                (v-x col-3)
                (v-y col-1)
                (v-y col-2)
                (v-y col-3)
                (v-z col-1)
                (v-z col-2)
                (v-z col-3)))

;;----------------------------------------------------------------


(defun get-columns (mat-a)
  "Return the columns of the matrix as 3 vector3s"
  (declare ((simple-array single-float (9)) mat-a))
  (list (make-vector3 (melm mat-a 0 0)
                      (melm mat-a 1 0)
                      (melm mat-a 2 0))
        (make-vector3 (melm mat-a 0 1)
                      (melm mat-a 1 1)
                      (melm mat-a 2 1))
        (make-vector3 (melm mat-a 0 2)
                      (melm mat-a 1 2)
                      (melm mat-a 2 2))))

;;----------------------------------------------------------------

(declaim
 (inline get-column)
 (ftype (function ((simple-array single-float (9)) (integer 0 3))
                  (simple-array single-float (3)))
        get-column))
(defun get-column (mat-a col-num)
  "Return the specified column of the matrix a vector3"
  (declare ((simple-array single-float (9)) mat-a)
           ((integer 0 3) col-num))
  (make-vector3 (melm mat-a 0 col-num)
                (melm mat-a 1 col-num)
                (melm mat-a 2 col-num)))

;;----------------------------------------------------------------

(declaim
 (inline mzerop)
 (ftype (function ((simple-array single-float (9)))
                  boolean)
        mzerop))
(defun mzerop (mat-a)
  "Returns 't' if this is a zero matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare ((simple-array single-float (9)) mat-a))
  (loop for i below 9
     if (not (float-zero (aref mat-a i)))
     do (return nil)
     finally (return t)))

;;----------------------------------------------------------------

;;[TODO] should the checks for '1.0' also have the error bounds?
(declaim
 (inline identityp)
 (ftype (function ((simple-array single-float (9)))
                  boolean)
        identityp))
(defun identityp (mat-a)
  "Returns 't' if this is an identity matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare ((simple-array single-float (9)) mat-a))
  (and (float-zero (- (melm mat-a 0 0) 1.0))
       (float-zero (- (melm mat-a 1 1) 1.0))
       (float-zero (- (melm mat-a 2 2) 1.0))
       (float-zero (melm mat-a 0 1))
       (float-zero (melm mat-a 0 2))
       (float-zero (melm mat-a 1 0))
       (float-zero (melm mat-a 1 2))
       (float-zero (melm mat-a 2 0))
       (float-zero (melm mat-a 2 1))))

;;----------------------------------------------------------------

(declaim
 (inline eql)
 (ftype (function ((simple-array single-float (9)) (simple-array single-float (9)))
                  boolean)
        eql))
(defun eql (mat-a mat-b)
  "Returns t if all elements of both matrices provided are
   equal"
  (declare ((simple-array single-float (9)) mat-a mat-b))
  (loop :for i :below 9 :always (= (aref mat-a i) (aref mat-b i))))

;;----------------------------------------------------------------

;;[TODO] should definately inline this ....should we? why?
;;[TODO] Would it be faster not to have to cofactors too?
(defun determinate-cramer (mat-a)
  "Returns the determinate of the matrix
   (uses the cramer method)"
  (declare ((simple-array single-float (9)) mat-a))
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

;;----------------------------------------------------------------

;;[TODO] Look more into errors
(declaim
 (inline inverse)
 (ftype (function ((simple-array single-float (9)))
                  (simple-array single-float (9)))
        inverse))
(defun inverse (mat-a)
  "returns the inverse of the matrix"
  (declare ((simple-array single-float (9)) mat-a))
  (multiple-value-bind (det cofactor-0 cofactor-3 cofactor-6)
      (determinate-cramer mat-a)
    (if (float-zero det)
        (error "Matrix Inverse: Singular Matrix (determinate is 0)"))
    (let ((inv-det (/ 1.0 det)))
      (make-matrix3
       (* inv-det cofactor-0)
       (* inv-det cofactor-3)
       (* inv-det cofactor-6)
       (* inv-det (- (* (melm mat-a 1 2) (melm mat-a 2 0))
                     (* (melm mat-a 1 0) (melm mat-a 2 2))))
       (* inv-det (- (* (melm mat-a 0 0) (melm mat-a 2 2))
                     (* (melm mat-a 0 2) (melm mat-a 2 0))))
       (* inv-det (- (* (melm mat-a 0 2) (melm mat-a 1 0))
                     (* (melm mat-a 0 0 ) (melm mat-a 1 2))))
       (* inv-det (- (* (melm mat-a 1 0) (melm mat-a 2 1))
                     (* (melm mat-a 1 1) (melm mat-a 2 0))))
       (* inv-det (- (* (melm mat-a 0 1) (melm mat-a 2 0))
                     (* (melm mat-a 0 0) (melm mat-a 2 1))))
       (* inv-det (- (* (melm mat-a 0 0) (melm mat-a 1 1))
                     (* (melm mat-a 0 1) (melm mat-a 1 0))))))))

;;----------------------------------------------------------------

(declaim
 (inline transpose)
 (ftype (function ((simple-array single-float (9)))
                  (simple-array single-float (9)))
        transpose))
(defun transpose (mat-a)
  "Returns the transpose of the provided matrix"
  (declare ((simple-array single-float (9))))
  (make-matrix3
   (melm mat-a 0 0) (melm mat-a 1 0) (melm mat-a 2 0)
   (melm mat-a 0 1) (melm mat-a 1 1) (melm mat-a 2 1)
   (melm mat-a 0 2) (melm mat-a 1 2) (melm mat-a 2 2)))

;;----------------------------------------------------------------

;;This is taken straight from 'Essential Mathematics for Game..'
(declaim
 (inline adjoint)
 (ftype (function ((simple-array single-float (9)))
                  (simple-array single-float (9)))
        adjoint))
(defun adjoint (mat-a)
  "Returns the adjoint of the matrix"
  (make-matrix3  (- (* (melm mat-a 1 1) (melm mat-a 2 2))
                    (* (melm mat-a 1 2) (melm mat-a 2 1)))
                 (- (* (melm mat-a 0 2) (melm mat-a 2 1))
                    (* (melm mat-a 0 1) (melm mat-a 2 2)))
                 (- (* (melm mat-a 0 1) (melm mat-a 1 2))
                    (* (melm mat-a 0 2) (melm mat-a 1 1)))
                 (- (* (melm mat-a 1 2) (melm mat-a 2 0))
                    (* (melm mat-a 1 0) (melm mat-a 2 2)))
                 (- (* (melm mat-a 0 0) (melm mat-a 2 2))
                    (* (melm mat-a 0 2) (melm mat-a 2 0)))
                 (- (* (melm mat-a 0 2) (melm mat-a 1 0))
                    (* (melm mat-a 0 0) (melm mat-a 1 2)))
                 (- (* (melm mat-a 1 0) (melm mat-a 2 1))
                    (* (melm mat-a 1 1) (melm mat-a 2 0)))
                 (- (* (melm mat-a 0 1) (melm mat-a 2 0))
                    (* (melm mat-a 0 0) (melm mat-a 2 1)))
                 (- (* (melm mat-a 0 0) (melm mat-a 1 1))
                    (* (melm mat-a 0 1) (melm mat-a 1 0)))))

;;----------------------------------------------------------------


(defun mtrace (mat-a)
  "Returns the trace of the matrix (That is the diagonal values)"
  (declare ((simple-array single-float (9)) mat-a))
  (+ (melm mat-a 0 0) (melm mat-a 1 1) (melm mat-a 2 2)))

;;----------------------------------------------------------------

;;Rotation goes here, requires quaternion

;;----------------------------------------------------------------

(declaim
 (inline rotation-from-euler)
 (ftype (function ((simple-array single-float (3)))
                  (simple-array single-float (9)))
        rotation-from-euler))
(defun rotation-from-euler (vec3-a)
  (declare ((simple-array single-float (3)) vec3-a))
  (let ((x (v-x vec3-a)) (y (v-y vec3-a)) (z (v-z vec3-a)))
    (let ((cx (cos x)) (cy (cos y)) (cz (cos z))
          (sx (sin x)) (sy (sin y)) (sz (sin z)))
      (make-matrix3 (* cy cz)
                    (+ (* sx sy cz) (* cx sz))
                    (+ (- (* cx sy cz)) (* sx sz))
                    (- (* cy sz))
                    (+ (- (* sx sy sz)) (* cx sz))
                    (+ (* cx sy sz) (* sx cz))
                    sy
                    (- (* sx cy))
                    (* cx cy)))))

;;----------------------------------------------------------------

(declaim
 (inline rotation-from-axis-angle)
 (ftype (function ((simple-array single-float (3)) single-float)
                  (simple-array single-float (9)))
        rotation-from-axis-angle))
(defun rotation-from-axis-angle (axis3 angle)
  "Returns a matrix which will rotate a point about the axis
   specified by the angle provided"
  (let* ((c-a (cos angle))
         (s-a (sin angle))
         (tt (- 1.0 c-a))
         (norm-axis (vector3:normalize axis3))
         (tx (* tt (v-x norm-axis)))
         (ty (* tt (v-y norm-axis)))
         (tz (* tt (v-z norm-axis)))
         (sx (* s-a (v-x norm-axis)))
         (sy (* s-a (v-y norm-axis)))
         (sz (* s-a (v-z norm-axis)))
         (txy (* tx (v-y norm-axis)))
         (tyz (* ty (v-z norm-axis)))
         (txz (* tx (v-z norm-axis))))
    (make-matrix3
     (+ c-a (* tx (v-x norm-axis)))  (- txy sz)                      (+ txz sy)
     (+ txy sz)                      (+ c-a (* ty (v-y norm-axis)))  (- tyz sx)
     (- txz sy)                      (+ tyz sx)                      (+ c-a (* tz (v-z norm-axis))))))

;;----------------------------------------------------------------

(declaim
 (inline scale)
 (ftype (function ((simple-array single-float (3)))
                  (simple-array single-float (9)))
        scale))
(defun scale (scale-vec3)
  "Returns a matrix which will scale by the amounts specified"
  (declare ((simple-array single-float (3)) scale-vec3))
  (make-matrix3 (v-x scale-vec3)  0.0               0.0
                0.0               (v-y scale-vec3)  0.0
                0.0               0.0               (v-z scale-vec3)))

;;----------------------------------------------------------------

(declaim
 (inline rotation-x)
 (ftype (function (single-float)
                  (simple-array single-float (9)))
        rotation-x))
(defun rotation-x (angle)
  "Returns a matrix which would rotate a point around the x axis
   by the specified amount"
  (declare (single-float angle))
  (let ((c-a (cos angle))
        (s-a (sin angle)))
    (make-matrix3 1.0    0.0       0.0
                  0.0    c-a       s-a
                  0.0    (- s-a)   c-a)))

;;----------------------------------------------------------------

(declaim
 (inline rotation-y)
 (ftype (function (single-float)
                  (simple-array single-float (9)))
        rotation-y))
(defun rotation-y (angle)
  "Returns a matrix which would rotate a point around the y axis
   by the specified amount"
  (declare (single-float angle))
  (let ((c-a (cos angle))
        (s-a (sin angle)))
    (make-matrix3 c-a    0.0    (- s-a)
                  0.0    1.0    0.0
                  s-a    0.0    c-a)))

;;----------------------------------------------------------------

(declaim
 (inline rotation-z)
 (ftype (function (single-float)
                  (simple-array single-float (9)))
        rotation-z))
(defun rotation-z (angle)
  "Returns a matrix which would rotate a point around the z axis
   by the specified amount"
  (declare (single-float angle))
  (let ((c-a (cos angle))
        (s-a (sin angle)))
    (make-matrix3 c-a      s-a    0.0
                  (- s-a)  c-a    0.0
                  0.0      0.0    1.0)))

;;----------------------------------------------------------------

(declaim
 (inline get-fixed-angles)
 (ftype (function ((simple-array single-float (9)))
                  (simple-array single-float (3)))
        get-fixed-angles))
(defun get-fixed-angles (mat-a)
  "Gets one set of possible z-y-x fixed angles that will generate
   this matrix. Assumes that this is a rotation matrix"
  (declare ((simple-array single-float (9)) mat-a))
  (let* ((sy (melm mat-a 0 2))
         (cy (c-sqrt (- 1.0 (* sy sy)))))
    (if (not (float-zero cy)) ; [TODO: not correct PI-epsilon]
        (let* ((factor (/ 1.0 cy))
               (sx (* factor (- (melm mat-a 2 1))))
               (cx (* factor (melm mat-a 2 2)))
               (sz (* factor (- (melm mat-a 1 0))))
               (cz (* factor (melm mat-a 0 0))))
          (make-vector3 (atan sx cx) (atan sy cy) (atan sz cz)))
        (let* ((sz 0.0)
               (cx 1.0)
               (sx (melm mat-a 1 2))
               (cz (melm mat-a 1 1)))
          (make-vector3 (atan sx cx) (atan sy cy) (atan sz cz))))))

;;----------------------------------------------------------------

;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works
(declaim
 (inline get-axis-angle)
 (ftype (function ((simple-array single-float (9)))
                  (simple-array single-float (3)))
        get-axis-angle))
(defun get-axis-angle (mat-a)
  "Gets one possible axis-angle pair that will generate this
   matrix. Assumes that this is a rotation matrix"
  (declare ((simple-array single-float (9)) mat-a))
  (let* ((c-a (* 0.5 (- (mtrace mat-a) 1.0)))
         (angle (acos c-a)))
    (cond ((float-zero angle) ;; <-angle is zero so axis can be anything
           (make-vector3 1.0 0.0 0.0))
          ((< angle (- base-maths:+pi+ base-maths:+float-threshold+))
                                        ;its not 180 degrees
           (let ((axis (make-vector3
                        (- (melm mat-a 1 2) (melm mat-a 2 1))
                        (- (melm mat-a 2 0) (melm mat-a 0 2))
                        (- (melm mat-a 0 1) (melm mat-a 1 0)))))
             (vector3:normalize axis)))
          (t (let* ((i (if (> (melm mat-a 1 1) (melm mat-a 0 0))
                           1
                           (if (> (melm mat-a 2 2)
                                  (melm mat-a 0 0))
                               2
                               0)))
                    (j (mod (+ i 1) 3))
                    (k (mod (+ j 1) 3))
                    (s (c-sqrt (+ 1.0 (- (melm mat-a i i)
                                         (melm mat-a j j)
                                         (melm mat-a k k)))))
                    (recip (/ 1.0 s))
                    (result (make-vector3 0.0 0.0 0.0)))
               (setf (aref result i) (* 0.5 s))
               (setf (aref result j) (* recip (melm mat-a i j)))
               (setf (aref result j) (* recip (melm mat-a k i)))
               result)))))

;;----------------------------------------------------------------

(declaim
 (inline m+)
 (ftype (function ((simple-array single-float (9))
                   (simple-array single-float (9)))
                  (simple-array single-float (9)))
        m+))
(defun m+ (mat-a mat-b)
  "Adds the 2 matrices component wise and returns the result as
   a new matrix"
  (declare ((simple-array single-float (9)) mat-a mat-b))
  (let ((r (zero-matrix3)))
    (declare ((simple-array single-float (9)) r))
    (dotimes (i 9)
      (setf (aref r i) (+ (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(declaim
 (inline m-)
 (ftype (function ((simple-array single-float (9))
                   (simple-array single-float (9)))
                  (simple-array single-float (9)))
        m-))
(defun m- (mat-a mat-b)
  "Subtracts the 2 matrices component wise and returns the result
   as a new matrix"
  (declare ((simple-array single-float (9)) mat-a mat-b))
  (let ((r (zero-matrix3)))
    (declare ((simple-array single-float (9)) r))
    (loop :for i :below 9 :do
       (setf (aref r i) (- (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(declaim
 (inline negate)
 (ftype (function ((simple-array single-float (9)))
                  (simple-array single-float (9)))
        negate))
(defun negate (mat-a)
  "Negates the components of the matrix"
  (let ((result (zero-matrix3)))
    (loop for i below 9
       do (setf (aref result i) (- (aref mat-a i))))
    result))

;;----------------------------------------------------------------

(defun mcol*vec3 (mat-a vec)
  (make-vector3
   (+ (* (v-x vec) (melm mat-a 0 0)) (* (v-y vec) (melm mat-a 0 1))
      (* (v-z vec) (melm mat-a 0 2)))

   (+ (* (v-x vec) (melm mat-a 1 0)) (* (v-y vec) (melm mat-a 1 1))
      (* (v-z vec) (melm mat-a 1 2)))

   (+ (* (v-x vec) (melm mat-a 2 0)) (* (v-y vec) (melm mat-a 2 1))
      (* (v-z vec) (melm mat-a 2 2)))))

;----------------------------------------------------------------

(defun mrow*vec3 (vec mat-a)
  (make-vector3
   (+ (* (v-x vec) (melm mat-a 0 0)) (* (v-y vec) (melm mat-a 1 0))
      (* (v-z vec) (melm mat-a 2 0)))

   (+ (* (v-x vec) (melm mat-a 0 1)) (* (v-y vec) (melm mat-a 1 1))
      (* (v-z vec) (melm mat-a 2 1)))

   (+ (* (v-x vec) (melm mat-a 0 2)) (* (v-y vec) (melm mat-a 1 2))
      (* (v-z vec) (melm mat-a 2 2)))))

;----------------------------------------------------------------
(defun m* (mat-a mat-b)
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (make-matrix3 (+ (* (melm mat-a 0 0) (melm mat-b 0 0))
                   (* (melm mat-a 0 1) (melm mat-b 1 0))
                   (* (melm mat-a 0 2) (melm mat-b 2 0)))
                (+ (* (melm mat-a 1 0) (melm mat-b 0 0))
                   (* (melm mat-a 1 1) (melm mat-b 1 0))
                   (* (melm mat-a 1 2) (melm mat-b 2 0)))
                (+ (* (melm mat-a 2 0) (melm mat-b 0 0))
                   (* (melm mat-a 2 1) (melm mat-b 1 0))
                   (* (melm mat-a 2 2) (melm mat-b 2 0)))
                (+ (* (melm mat-a 0 0) (melm mat-b 0 1))
                   (* (melm mat-a 0 1) (melm mat-b 1 1))
                   (* (melm mat-a 0 2) (melm mat-b 2 1)))
                (+ (* (melm mat-a 1 0) (melm mat-b 0 1))
                   (* (melm mat-a 1 1) (melm mat-b 1 1))
                   (* (melm mat-a 1 2) (melm mat-b 2 1)))
                (+ (* (melm mat-a 2 0) (melm mat-b 0 1))
                   (* (melm mat-a 2 1) (melm mat-b 1 1))
                   (* (melm mat-a 2 2) (melm mat-b 2 1)))
                (+ (* (melm mat-a 0 0) (melm mat-b 0 2))
                   (* (melm mat-a 0 1) (melm mat-b 1 2))
                   (* (melm mat-a 0 2) (melm mat-b 2 2)))
                (+ (* (melm mat-a 0 1) (melm mat-b 0 2))
                   (* (melm mat-a 1 1) (melm mat-b 1 2))
                   (* (melm mat-a 1 2) (melm mat-b 2 2)))
                (+ (* (melm mat-a 2 0) (melm mat-b 0 2))
                   (* (melm mat-a 2 1) (melm mat-b 1 2))
                   (* (melm mat-a 2 2) (melm mat-b 2 2)))))

  ;;----------------------------------------------------------------


(defun m*vec (mat-a vec-a)
  "Multiplies the vector3 by the matrix and returns the result
   as a new vector3"
  (make-vector3 (+ (* (melm mat-a 0 0) (v-x vec-a))
                   (* (melm mat-a 0 1) (v-y vec-a))
                   (* (melm mat-a 0 2) (v-z vec-a)))
                (+ (* (melm mat-a 1 0) (v-x vec-a))
                   (* (melm mat-a 1 1) (v-y vec-a))
                   (* (melm mat-a 1 2) (v-z vec-a)))
                (+ (* (melm mat-a 2 0) (v-x vec-a))
                   (* (melm mat-a 2 1) (v-y vec-a))
                   (* (melm mat-a 2 2) (v-z vec-a)))))

;;----------------------------------------------------------------

(defun m*scalar (mat-a scalar)
  "Multiplies the components of the matrix by the scalar
   provided"
  (let ((result (zero-matrix3)))
    (loop for i below 9
       do (setf (aref result i) (* scalar (aref mat-a i))))
    result))
