;; 

(in-package :cepl)
(defglstruct vert-data ()
  (position :vec3)
  (color :vec4))

(defpipeline prog-2 ((vert vert-data) &uniform (cam-to-clip :mat4)
                     (world-to-cam :mat4) (model-to-world :mat4))
  (:vertex (setf gl-position (* cam-to-clip
                                (* world-to-cam 
                                   (* model-to-world
                                      (v! (vert-data-position vert)
                                            1.0)))))
           (out (interp-color :smooth) (vert-data-color vert)))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480)))

(defparameter *entity* nil)
(defparameter *camera* nil)
(defparameter *resolution* (v! 640 480))

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))
(defun make-entity (&key pos e-stream)
  (make-instance 'entity :pos pos :e-stream e-stream))


(defun init () 
  (setf *camera* (ccam:make-camera *resolution*))
  (setf (ccam:pos *camera*) (v! 0 8 0))
  (prog-2 nil :cam-to-clip (ccam:cam->clip *camera*))
  (let* ((verts (make-gpu-array `((,(v! +1  +1  +1)  ,(v! 0  1  0  1)) 
                                  (,(v! -1  -1  +1)  ,(v! 0  0  1  1))
                                  (,(v! -1  +1  -1)  ,(v! 1  0  0  1))
                                  (,(v! +1  -1  -1)  ,(v! 0.5  0.5  0  1))
                                  (,(v! -1  -1  -1)  ,(v! 0  1  0  1)) 
                                  (,(v! +1  +1  -1)  ,(v! 0  0  1  1))
                                  (,(v! +1  -1  +1)  ,(v! 1  0  0  1))
                                  (,(v! -1  +1  +1)  ,(v! 0.5  0.5  0  1)))
                                :element-type 'vert-data :dimensions 8))
         (indicies (make-gpu-array '(0 1 2   1 0 3   2 3 0   3 2 1
                                     5 4 6   4 5 7   7 6 4   6 7 5)
                    :dimensions 24 :element-type :unsigned-short))
         (e-stream (make-vertex-stream verts :index-array indicies)))
    (setf *entity* (make-entity :pos (v!  0 0 -20) :e-stream e-stream)))  
  ;;set options
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))

(defun entity-matrix (entity)
  (reduce #'m4:m* (list (m4:translation (pos entity))
                        (m4:rotation-from-euler (rot entity))
                        (m4:scale (scale entity)))))


;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defun get-time () (get-internal-real-time))

(tdefun update-pos (entity)
  (repeat
   (before (seconds 5) (setf (v-y (pos entity)) (* 10 %progress%)))
   (before (seconds 3) (setf (v-y (pos entity)) (- 10 (* 10 %progress%))))))

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (update-pos *entity*)
  (setf (rot *entity*) (v:+ (rot *entity*) (v! 0.01 0.02 0)))
  (prog-2 (e-stream *entity*) 
          :model-to-world (entity-matrix *entity*)
          :world-to-cam (ccam:world->cam *camera*))
  (gl:flush)
  (cgl:update-display))


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


(defun reshape (width height)  
  (setf (ccam:frame-size *camera*) (v! width height))
  (prog-2 nil :cam-to-clip (ccam:cam->clip *camera*))
  (cgl:viewport 0 0 width height))

(let ((running nil))
  (defun run-demo () 
    (init)
    (reshape 640 480)  
    (setf running t)
    (loop :while running :do
       (case-events (event)
         (:quit () (setf running nil))
         (:windowevent (:event e :data1 x :data2 y)
                       (when (eql e sdl2-ffi:+sdl-windowevent-resized+)
                         (reshape x y))))
       (ttm:update)
       (update-swank)
       (continuable (draw))))
  (defun stop-demo () (setf running nil)))
