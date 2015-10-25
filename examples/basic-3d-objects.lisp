;; More 3D - Multiple objects rotating
(in-package :cepl)

(defvar *entities* nil)
(defvar *camera* nil)

(defstruct-g vert-data ()
  (position :vec3)
  (color :vec4))

(defun-g b3d-vert ((vert g-pc) &uniform (cam camera) (model->world :mat4))
  (values (* (cam->clip cam)
             (* (world->cam cam)
                (* model->world
                   (v! (pos vert) 1.0))))
          (:smooth (col vert))))

(defun-g b3d-frag ((interp-color :vec4))
  interp-color)

(defpipeline render-widgets ()
    (g-> #'b3d-vert #'b3d-frag))

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))
(defun make-entity (&key pos e-stream)
  (make-instance 'entity :pos pos :e-stream e-stream))


(defun init ()
  (setf *camera* (make-camera))
  (setf (pos *camera*) (v! 0 8 0))
  (render-widgets nil :cam *camera*)
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
         (e-stream (make-buffer-stream verts :index-array indicies)))
    (setf *entities*
          (mapcar (lambda (_) (make-entity :pos _ :e-stream e-stream))
                  (list (v!  0 0 -20) (v!  0 0 -25) (v!  5 0 -20)
                        (v!  0 0 -15) (v! -5 0 -20))))))

(defun update-entity (entity)
  (let ((m2w (reduce #'m4:m* (list (m4:translation (pos entity))
                                   (m4:rotation-from-euler (rot entity))
                                   (m4:scale (scale entity))))))
    (setf (rot entity) (v:+ (rot entity) (v! 0.01 0.02 0)))
    (map-g #'render-widgets (e-stream entity) :model->world m2w)))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (render-widgets nil :cam *camera*)
  (map nil #'update-entity *entities*)
  (update-display))

(defun reshape (dimensions)
  (setf (frame-size *camera*) dimensions)
  (render-widgets nil :cam *camera*))

(let ((running nil))
  (defun run-loop ()
    (init)
    (reshape (current-viewport))
    (setf running t)
    (loop :while running :do (continuable (step-demo))))

  (defun stop-loop () (setf running nil)))

(evt:def-named-event-node sys-listener (e evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))

(evt:def-named-event-node window-listener (e evt:|window|)
  (when (eq (evt:action e) :resized)
    (reshape (evt:data e))))
