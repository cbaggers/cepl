;; More 3D - Multiple objects rotating

(in-package :cepl)
(defparameter *entities* nil)
(defparameter *camera* nil)
(defparameter *resolution* cgl:+default-resolution+)

(defglstruct vert-data ()
  (position :vec3)
  (color :vec4))

(def-gl-equivalent camera
  (cam-to-clip :type :mat4 :converter #'cam->clip)
  (world-to-cam :type :mat4 :converter #'world->cam))

(defpipeline prog-2 ((vert g-pc)
                     &uniform (cam camera) (model-to-world :mat4))
  (:vertex (setf gl-position (* (cam-to-clip cam)
                                (* (world-to-cam cam)
                                   (* model-to-world
                                      (v! (cgl:pos vert) 1.0)))))
           (out (interp-color :smooth) (cgl:col vert)))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape cgl:+default-resolution+)))

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))
(defun make-entity (&key pos e-stream)
  (make-instance 'entity :pos pos :e-stream e-stream))


(defun init ()
  (setf *camera* (make-camera *resolution*))
  (setf (pos *camera*) (v! 0 8 0))
  (prog-2 nil :cam *camera*)
  (let* ((verts (make-gpu-array `((,(v! +1  +1  +1)  ,(v! 0  1  0  1))
                                  (,(v! -1  -1  +1)  ,(v! 0  0  1  1))
                                  (,(v! -1  +1  -1)  ,(v! 1  0  0  1))
                                  (,(v! +1  -1  -1)  ,(v! 0.5  0.5  0  1))
                                  (,(v! -1  -1  -1)  ,(v! 0  1  0  1))
                                  (,(v! +1  +1  -1)  ,(v! 0  0  1  1))
                                  (,(v! +1  -1  +1)  ,(v! 1  0  0  1))
                                  (,(v! -1  +1  +1)  ,(v! 0.5  0.5  0  1)))
                                :element-type 'g-pc :dimensions 8))
         (indicies (make-gpu-array '(0 2 1   1 3 0   2 0 3   3 1 2
                                     5 6 4   4 7 5   7 4 6   6 5 7)
                    :dimensions 24 :element-type :unsigned-short))
         (e-stream (make-vertex-stream verts :index-array indicies)))
    (setf *entities* `(,(make-entity :pos (v!  0 0 -20) :e-stream e-stream)
                        ,(make-entity :pos (v!  0 0 -25) :e-stream e-stream)
                        ,(make-entity :pos (v!  5 0 -20) :e-stream e-stream)
                        ,(make-entity :pos (v!  0 0 -15) :e-stream e-stream)
                        ,(make-entity :pos (v! -5 0 -20) :e-stream e-stream)))))

(defun update-entity (entity)
  (let ((m2w (reduce #'m4:m* (list (m4:translation (pos entity))
                                   (m4:rotation-from-euler (rot entity))
                                   (m4:scale (scale entity))))))
    (setf (rot entity) (v:+ (rot entity) (v! 0.01 0.02 0)))
    (gmap #'prog-2 (e-stream entity) :model-to-world m2w)))

(defun step-demo ()
  (evt.sdl:pump-events)
  (update-swank)
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (prog-2 nil :cam *camera*)
  (map nil #'update-entity *entities*)
  (gl:flush)
  (cgl:update-display))

(defun reshape (dimensions)
  (setf (frame-size *camera*) dimensions)
  (prog-2 nil :cam *camera*)
  (apply #'gl:viewport 0 0 dimensions))

(let ((running nil))
  (defun run-demo ()
    (init)
    (reshape cgl:+default-resolution+)
    (setf running t)
    (loop :while running :do (continuable (step-demo))))

  (defun stop-demo () (setf running nil))

  (evt:observe (evt.sdl::*sys*)
    (setf running (typep e 'evt.sdl:will-quit))))

(evt:observe (evt.sdl::*window*)
  (when (eq (evt.sdl:action e) :resized)
    (reshape (evt.sdl:vec e))))
