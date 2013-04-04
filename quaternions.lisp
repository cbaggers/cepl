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

(defun w (quat)
  "Returns the w component of the quaternion"  
  (aref quat 0))

(defun x (quat)
  "Returns the x component of the quaternion"
  (aref quat 1))

(defun y (quat)
  "Returns the y component of the quaternion"
  (aref quat 2))

(defun z (quat)
  "Returns the z component of the quaternion"
  (aref quat 3))

;;----------------------------------------------------------------;;

(defun zero-quat ()
  (make-quat 0.0 0.0 0.0 0.0))

(defun zero-quatp (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (float-zero (+ (* w w) (* x x) (* y y) (* z z)))))

(defun unit-quatp (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (float-zero (- 1.0 (* w w) (* x x) (* y y) (* z z)))))

(defun identity-quat ()
  (make-quat 1.0 0.0 0.0 0.0))

(defun identity-p (quat)
  (and (float-zero (- 1.0 (w quat)))
       (float-zero (x quat))
       (float-zero (y quat))
       (float-zero (z quat))))

(declaim (inline make-quat)
         (ftype (function ((single-float) 
                           (single-float) 
                           (single-float)
                           (single-float)) 
                          (simple-array single-float (4))) 
                make-quat))
(defun make-quat (w x y z)
  "This takes 4 floats and give back a vector4, this is just an
   array but it specifies the array type and populates it. 
   For speed reasons it will not accept integers so make sure 
   you hand it floats."
  (declare (single-float x y z w))
  (let ((q (make-array 4 :element-type `single-float)))
    (setf (aref q 0) w
          (aref q 1) x
          (aref q 2) y
          (aref q 3) z)
    q))

(defun make-quat-from-vec3 (vec3)
  (make-quat 0.0 (aref vec3 0) (aref vec3 1) (aref vec3 2)))

(defun make-quat-from-rotation-matrix3 (mat3)
  (let ((trace (m3:mtrace mat3)))
    (if (> trace 0.0)
        (let* ((s (c-sqrt (+ 1.0 trace)))               
               (recip (/ 0.5 s)))
          (make-quat (* s 0.5) 
                     (* (- (m3:melm mat3 2 1) (m3:melm mat3 1 2)) recip)
                     (* (- (m3:melm mat3 0 2) (m3:melm mat3 2 0)) recip)
                     (* (- (m3:melm mat3 1 0) (m3:melm mat3 0 1)) recip)))
        (let* ((i (if (> (m3:melm mat3 1 1) (m3:melm mat3 0 0)) 1 0))
               (i (when (> (m3:melm mat3 2 2) (m3:melm mat3 i i)) 2))
               (j (mod (+ 1 i) 3))
               (k (mod (+ 1 j) 3))
               (s (c-sqrt (+ (- (m3:melm mat3 i i)
                                (m3:melm mat3 j j)
                                (m3:melm mat3 k k)) 
                             1.0)))
               (recip (/ 0.5 s))
               (quat (make-quat (* (- (m3:melm mat3 k j) 
                                      (m3:melm mat3 j k)) 
                                   recip) 0.0 0.0 0.0)))
          (setf (aref quat i) (* s 0.5)
                (aref quat j) (* (+ (m3:melm mat3 j i) (m3:melm mat3 i j))
                                 recip)
                (aref quat k) (* (+ (m3:melm mat3 k i) (m3:melm mat3 i k))
                                 recip))))))

(defun make-quat-from-axis-angle (axis-vec3 angle)
  (let ((length (v3:vlength-squared axis-vec3)))
    (if (float-zero length)
        (identity-quat)
        (let* ((half-angle (/ angle 2.0))
               (sin-half-angle (sin half-angle))
               (cos-half-angle (cos half-angle))
               (scale-factor (/ sin-half-angle (c-sqrt length))))
          (v4:make-vector4 cos-half-angle
                           (* scale-factor (aref axis-vec3 0))
                           (* scale-factor (aref axis-vec3 1))
                           (* scale-factor (aref axis-vec3 2)))))))

;;[TODO] Need to use destructive operations in here to stop multiple quats 
;;       being created
(defun make-quat-from-vectors (from3 to3)
  (let* ((axis (v3:cross from3 to3))
         (quat (normalize (make-quat (v3:dot from3 to3) (aref axis 0) (aref axis 1)
                                     (aref axis 2))))
         (w (+ 1.0 (aref quat 0))))    
    (if (<= w +float-threshold+)
        (if (> (* (aref from3 2) (aref from3 2))
               (* (aref from3 0) (aref from3 0)))
            (setf (aref quat 0) 0.0
                  (aref quat 1) 0.0
                  (aref quat 2) (aref from3 2)
                  (aref quat 3) (- (aref from3 1)))
            (setf (aref quat 0) 0.0
                  (aref quat 1) (aref from3 1)
                  (aref quat 2) (- (aref from3 0))
                  (aref quat 3) 0.0))
        (setf (aref quat 0) w))
    (normalize quat)))

(defun make-quat-from-fixed-angles (x-rot y-rot z-rot)
  (let ((x-rot (/ x-rot 2.0))
        (y-rot (/ y-rot 2.0))
        (z-rot (/ z-rot 2.0)))
    (let ((cos-x (cos x-rot)) (sin-x (sin x-rot))
          (cos-y (cos y-rot)) (sin-y (sin y-rot))
          (cos-z (cos z-rot)) (sin-z (sin z-rot)))
      (make-quat (- (* cos-x cos-y cos-z) (* sin-x sin-y sin-z))
                 (- (* sin-x cos-y cos-z) (* cos-x sin-y sin-z))
                 (- (* cos-x sin-y cos-z) (* sin-x cos-y sin-z))
                 (- (* cos-x cos-y sin-z) (* sin-x sin-y cos-x))))))

(defun magnitude (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (c-sqrt (+ (* w w) (* x x) (* y y) (* z z)))))

(defun norm (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (+ (* w w) (* x x) (* y y) (* z z))))

(defun quat-eql (q1 q2)
  (and (float-zero (- (w q2) (w q1))) (float-zero (- (x q2) (x q1)))
       (float-zero (- (y q2) (y q1))) (float-zero (- (z q2) (z q1)))))

;;[TODO] This seems wrong!...but book says it's right
(defun quat-!eql (q1 q2)
  (not (or (float-zero (- (w q2) (w q1))) (float-zero (- (x q2) (x q1)))
           (float-zero (- (y q2) (y q1))) (float-zero (- (z q2) (z q1))))))

(defun copy (quat)
  (make-quat (w quat) (x quat) (y quat) (z quat)))

(defun get-axis-angle (quat)
  (list 
   (let ((length (c-sqrt (- 1.0 (* (w quat) (w quat))))))
     (if (float-zero length)
         (v3:make-vector3 0.0 0.0 0.0)
         (let ((length (/ 1.0 length)))
           (v3:make-vector3 (* length (x quat)) 
                            (* length (y quat)) 
                            (* length (z quat))))))
   (* 2.0 (acos (w quat)))))

(defun normalize (quat)
  (let ((length-squared (v4:dot quat quat)))
    (if (float-zero length-squared)
        (zero-quat)
        (let ((factor (c-inv-sqrt length-squared)))
          (make-quat (* (w quat) factor)
                     (* (x quat) factor)
                     (* (y quat) factor)
                     (* (z quat) factor))))))

(defun qconjugate (quat)
  (make-quat (w quat) (- (x quat)) (- (y quat)) (- (z quat))))

(defun inverse (quat)
  (let ((norm (norm quat)))
    (if (float-zero norm)
        (identity-quat)
        (let ((norm-recip (/ 1.0 norm)))
          (make-quat (* norm-recip (w quat))
                     (- (* norm-recip (x quat)))
                     (- (* norm-recip (y quat)))
                     (- (* norm-recip (z quat))))))))

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
  (make-quat (- (* (w quat-a) (w quat-b))
                (* (x quat-a) (x quat-b))
                (* (y quat-a) (y quat-b))
                (* (z quat-a) (z quat-b)))
             (- (+ (* (w quat-a) (x quat-b))
                   (* (x quat-a) (w quat-b))
                   (* (y quat-a) (z quat-b)))
                (* (z quat-a) (y quat-b)))
             (- (+ (* (w quat-a) (y quat-b))
                   (* (y quat-a) (w quat-b))
                   (* (z quat-a) (x quat-b)))
                (* (x quat-a) (z quat-b)))
             (- (+ (* (w quat-a) (z quat-b))
                   (* (z quat-a) (w quat-b))
                   (* (x quat-a) (y quat-b)))
                (* (y quat-a) (x quat-b)))))

(defun to-matrix3 (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (let ((x2 (+ x x)) (y2 (+ y y)) (z2 (+ z z)))
      (let ((wx (* w x2))  (wy (* w y2))  (wz (* w z2))
            (xx (* x x2))  (xy (* x y2))  (xz (* x z2))
            (yy (* y y2))  (yz (* y z2))
            (zz (* z z2)))
        (m3:make-matrix3 
         (- 1.0 (+ yy zz)) (- xy wz)         (+ xz wy)
         (+ xy wz)         (- 1.0 (+ xx zz)) (- yz wx)
         (- xz wy)         (+ yz wx)         (- 1.0 (+ xx yy)))))))

(defun to-matrix4 (quat)
  (let ((w (w quat))  (x (x quat))  (y (y quat))  (z (z quat)))
    (let ((x2 (+ x x)) (y2 (+ y y)) (z2 (+ z z)))
      (let ((wx (* w x2))  (wy (* w y2))  (wz (* w z2))
            (xx (* x x2))  (xy (* x y2))  (xz (* x z2))
            (yy (* y y2))  (yz (* y z2))
            (zz (* z z2)))
        (m4:make-matrix4
         (- 1.0 (+ yy zz)) (- xy wz) (+ xz wy) 0.0
         (+ xy wz) (- 1.0 (+ xx zz)) (- yz wx) 0.0
         (- xz wy) (+ yz wx) (- 1.0 (+ xx yy)) 0.0
         0.0 0.0 0.0 1.0)))))

(defun dot (quat-a quat-b)
  (v4:dot quat-a quat-b))

;; [TODO] Look into assets (this should be a unit quaternion
(defun rotate (vec3 quat)
  "Rotate vec3 by quaternion. Assumes quaternion is normalized."
  (let* ((v-mult (* 2.0 (+ (* (x quat) (aref vec3 0))
                           (* (y quat) (aref vec3 1))
                           (* (z quat) (aref vec3 2)))))
         (cross-mult (* 2.0 (w quat)))
         (p-mult (- (* cross-mult (w quat)) 1.0)))
    (v3:make-vector3 (+ (* p-mult (aref vec3 0))
                        (* v-mult (x quat))
                        (* cross-mult 
                           (- (* (y quat) (aref vec3 2))
                              (* (z quat) (aref vec3 1)))))
                     (+ (* p-mult (aref vec3 1))
                        (* v-mult (y quat))
                        (* cross-mult 
                           (- (* (z quat) (aref vec3 0))
                              (* (x quat) (aref vec3 2)))))
                     (+ (* p-mult (aref vec3 2))
                        (* v-mult (z quat))
                        (* cross-mult 
                           (- (* (x quat) (aref vec3 1))
                              (* (y quat) (aref vec3 0))))))))

;; [TODO] Could be faster (see q+1 area)
(defun lerp (start-quat end-quat pos)
  "Linearaly interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (let ((cos-angle (v4:dot start-quat end-quat)))
    (if (float>=0 cos-angle)
        (q+1 (q* end-quat pos)
             (q* start-quat (- 1.0 pos)))
        (q+1 (q* end-quat pos)
             (q* start-quat (- pos 1.0))))))

(defun slerp (start-quat end-quat pos)
  "Spherically interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (destructuring-bind (start-mult end-mult)
      (let ((cos-angle (v4:dot start-quat end-quat)))
        ;; if angle between quaternions is less than 90 degrees
        (if (float-greater-than-zero cos-angle)
            ;; if angle is greater than zero
            (if (float-greater-than-zero (- 1.0 cos-angle))
                (let* ((angle (acos cos-angle))
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
                (let* ((angle (acos (- cos-angle)))
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
  (let* ((cos-angle (v4:dot start-quat end-quat))
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
