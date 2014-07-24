;; More 3D - Multiple objects rotating

(defparameter *resolution* (v! 640 480))
(defparameter *gpu-arrays* nil)
(defparameter *tex-array* nil)
(defparameter *tex* nil)
(defparameter *entity* nil)
(defparameter *loop* 0.0)
(defparameter *camera* (ccam:make-camera *resolution*))

(defpipeline gpu-draw ((vert p-n-t) &uniform (model-clip :mat4)
                       (tex :sampler-2d))
  (:vertex (setf gl-position (* model-clip (v! (cgl::pos vert) 1.0)))
           (out (coord :smooth) (cgl::tex vert)))
  (:fragment (out output-color (texture tex coord)))
  (:post-compile (reshape *resolution*)))

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -5) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))

(defun init () 
  (multiple-value-bind (garrays stream) (primitives:prim-array :box 1.0 t t)
    (setf *gpu-arrays* garrays
          *entity* (make-instance 'entity :e-stream stream)))
  (setf *tex-array* (make-c-array '(64 64) :vec4 :initial-contents 
                      (loop for i below 64 :collect
                           (loop :for j :below 64 :collect 
                              (v! (/ (random 100) 100) (/ (random 100) 100)
                                  (/ (random 100) 100) 1))))
        *tex* (make-texture :initial-contents *tex-array*))
  (make-texture :initial-contents *a*))

(defun model->clip (entity)
  (reduce #'m4:m* (list (ccam:cam->clip *camera*)
                        (ccam:world->cam *camera*)
                        (m4:translation (pos entity))
                        (m4:rotation-from-euler (rot entity))
                        (m4:scale (scale entity)))))

(defun draw ()
  (ttm:update)
  (incf *loop* 0.01)
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (setf (rot *entity*) (v:+ (rot *entity*) (v! 0.0008 0.0008 0)))
  (gpu-draw (e-stream *entity*) :model-clip (model->clip *entity*) :tex *tex*)
  (gl:flush)
  (cgl:update-display))

(defun reshape (frame-size-vec2)
  (setf (ccam:frame-size *camera*) frame-size-vec2)
  (cgl:viewport 0 0 (aref frame-size-vec2 0) (aref frame-size-vec2 1)))

(let ((running nil))
  (defun run-demo () 
    (init)
    (reshape *resolution*)  
    (setf running t)
    (loop :while running :do
       (case-events (event)
         (:quit () (setf running nil))
         (:windowevent (:event e :data1 x :data2 y)
                       (when (eql e sdl2-ffi:+sdl-windowevent-resized+)
                         (reshape (v! x y)))))
       (update-swank)
       (continuable (draw))))
  (defun stop-demo () (setf running nil)))
