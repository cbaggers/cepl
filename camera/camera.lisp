;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is stub
;; I'm popping these function here as we will eventually need
;; some kind of abstraction aroudn the idea of 'cameras'

(in-package :cepl-camera)

;;;--------------------------------------------------------------

(defclass camera () 
  ((cam->clip :type (simple-array single-float (16)) :reader cam->clip)
   (cam->clip-func :initform nil :initarg :cam->clip-func )
   (frame-size :reader frame-size :initarg :frame-size 
               :initform (v! (nth 0 cgl::+default-resolution+)
                             (nth 1 cgl::+default-resolution+)))
   (near :type single-float :reader near :initarg :near)
   (far :type single-float :reader far :initarg :far)
   (fov :type single-float :reader fov :initarg :fov)))

(defmethod update-cam->clip ((camera camera))
  (setf (slot-value camera 'cam->clip)
        (funcall (slot-value camera 'cam->clip-func) camera)))

(defmethod (setf near) (distance (camera camera))
  (setf (slot-value camera 'near) distance)
  (update-cam->clip camera))

(defmethod (setf far) (distance (camera camera))
  (setf (slot-value camera 'far) distance)
  (update-cam->clip camera))

(defmethod (setf fov) (angle (camera camera))
  (setf (slot-value camera 'fov) angle)
  (update-cam->clip camera))

(defmethod (setf frame-size) (frame-size (camera camera))
  (let ((frame-size-vec2
         (if (typep frame-size '(simple-array single-float (2)))
             frame-size
             (make-array 2 :element-type 'single-float :initial-contents
                         (list (float (elt frame-size 0))
                               (float (elt frame-size 1)))))))
    (setf (slot-value camera 'frame-size) frame-size-vec2))
  (update-cam->clip camera))

(defgeneric world->cam (camera))

(defclass pos-dir-cam (camera)
  ((world-up :type (simple-array single-float (3))
             :initform (v3:make-vector3 0.0 1.0 0.0)
             :initarg :world-up
             :accessor world-up)
   (position :type (simple-array single-float (3))
             :initform (v3:make-vector3 0.0 0.0 0.0)
             :initarg :pos
             :accessor cepl-generics:pos)
   (direction :type (simple-array single-float (3))
              :initform (v3:make-vector3 0.0 0.0 -1.0)
              :initarg :dir
              :accessor cepl-generics:dir)))

(defmethod look-at ((camera pos-dir-cam) point-vec3)
  (with-slots (world-up position direction) camera
    (setf direction (v3:normalize (v3:v-1 point-vec3 position)))))

(defmethod world->cam ((camera pos-dir-cam))
  (with-slots (world-up position direction) camera
    (let* ((up (v3:normalize 
                (v3:v-1 world-up 
                        (v3:v* direction (v3:dot world-up direction)))))
           (side (v3:cross direction up))
           (rotate (m3:make-from-rows side up (v3:negate direction)))
           (eye-inv (v3:negate (m3:mcol*vec3 rotate position)))
           (result (m4:rotation-from-matrix3 rotate)))
      (setf (m4:melm result 0 3) (aref eye-inv 0)
            (m4:melm result 1 3) (aref eye-inv 1)
            (m4:melm result 2 3) (aref eye-inv 2))
      result)))

(defun perspective-projection (camera)
  (let* ((aspect-ratio (/ (aref (frame-size camera) 0) 
                          (aref (frame-size camera) 1)))
         (near (near camera))
         (far (far camera))
         (fov (fov camera))
         (range (tan (/ fov 2.0)))
         (left (- (* range aspect-ratio)))
         (right (* range aspect-ratio))
         (bottom (- range))
         (top range))
    (matrix4:make-matrix4 
     (/ (* near 2) (- right left)) 0.0 0.0 0.0
     0.0 (/ (* near 2) (- top bottom)) 0.0 0.0
     0.0 0.0 (- (/ (+ far near) (- far near))) -1.0
     0.0 0.0 (/ (* 2.0 far near) (- near far)) 0.0)))

(defun orthographic-projection (camera)
  (let ((left (- (/ (aref (frame-size camera) 0) 2.0)))
        (right (/ (aref (frame-size camera) 0) 2.0))
        (top (/ (aref (frame-size camera) 1) 2.0))
        (bottom (- (/ (aref (frame-size camera) 1) 2.0)))
        (near (near camera))
        (far (far camera)))
    (matrix4:make-matrix4 
     (/ 2 (- right left)) 0.0 0.0 (- (/ (+ right left) (- left right)))
     0.0 (/ 2 (- top bottom)) 0.0 (- (/ (+ top bottom) (- bottom top)))
     0.0 0.0 (- (/ (- far near))) (- (/ (+ far near) (- far near)))
     0.0 0.0 0.0 1.0)))

(defun make-camera (frame-size &optional (near 1.0) (far 1000.0) (fov 120.0)
                                      (cam->clip-function 
                                       #'perspective-projection))
  (let* ((frame-size-vec2 (if (typep frame-size '(simple-array single-float (2)))
                              frame-size
                              (make-array 2 :element-type 'single-float :initial-contents
                                          (list (float (elt frame-size 0))
                                                (float (elt frame-size 1))))))
         (camera (make-instance 'pos-dir-cam :cam->clip-func cam->clip-function 
                                :near near :far far :fov fov 
                                :frame-size frame-size-vec2)))
    (update-cam->clip camera)
    camera))

;;;--------------------------------------------------------------


