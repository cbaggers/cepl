;; Loading a monkey head :D

;; working

(in-package :cepl)

(defglstruct vert-data 
  (position :vec3 :accessor pos)
  (color :vec4 :accessor color))

(defpipeline prog-2 ((vert vert-data) &uniform (cam-to-clip :mat4)
                     (world-to-cam :mat4) (model-to-world :mat4))
  (:vertex (setf gl-position (* cam-to-clip
                                (* world-to-cam 
                                   (* model-to-world
                                      (v! (pos vert) 1.0)))))
           (out (interp-color :smooth) (color vert)))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480)))


(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *monkey* nil)
(defparameter *camera* nil)

(defstruct entity 
  (stream nil)
  (position (v! 0 0 -3))
  (rotation (v! 0 0 0))
  (scale (v! 1 1 1)))

(defstruct camera 
  (position (v! 0 0 0))
  (look-direction (v! 0 0 -1))
  (up-direction (v! 0 1 0)))

(defun point-camera-at (camera point)
  (setf (camera-look-direction camera)
        (v:normalize (v:- point (camera-position camera)))) camera)

(defun calculate-cam-look-at-w2c-matrix (camera)
  (let* ((look-dir (v3:normalize (camera-look-direction camera)))
         (up-dir (v3:normalize (camera-up-direction camera)))
         (right-dir (v3:normalize (v3:cross look-dir up-dir)))
         (perp-up-dir (v3:cross right-dir look-dir))
         (rot-matrix (m4:transpose
                      (m4:rotation-from-matrix3
                       (m3:make-from-rows right-dir
                                          perp-up-dir
                                          (v3:v-1 (v! 0 0 0)
                                                  look-dir)))))
         (trans-matrix 
          (m4:translation (v3:v-1 (v! 0 0 0)
                                  (camera-position camera)))))
    (m4:m* rot-matrix trans-matrix)))

(defun resolve-cam-position (sphere-cam-rel-pos cam-target)
  (let* ((phi (* base-maths:+one-degree-in-radians+
                 (v-x sphere-cam-rel-pos)))
         (theta (* base-maths:+one-degree-in-radians+
                   (+ 90.0 (v-y sphere-cam-rel-pos))))
         (sin-theta (sin theta))
         (con-theta (cos theta))
         (sin-phi (sin phi))
         (cos-phi (cos phi))
         (dir-to-cam (v! (* sin-theta cos-phi)
                         con-theta
                         (* sin-theta sin-phi))))
    (v3:v+ cam-target (v3:v* dir-to-cam (v-z sphere-cam-rel-pos)))))

(defun init () 
  (setf *camera* (make-camera :position (v! 0.0 0.0 0.0)))
  (setf *frustrum-scale* (cepl-camera::calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix *frustrum-scale*))
  (prog-2 nil :cam-to-clip *cam-clip-matrix*)
  ;;create entities
  (let* ((monkey-data (first (model-parsers:parse-obj-file "monkey.obj")))
         (verts (mapcar #'(lambda (x) (list x (v! (random 1.0) (random 1.0)
                                                  (random 1.0) 1.0))) 
                        (first monkey-data)))
         (indicies (loop for face in (car (last monkey-data))
                      :append (mapcar #'car (first face))))
         (stream (make-vertex-stream
                  (make-gpu-array verts :dimensions (length verts)
                                  :element-type 'vert-data)
                  :length (length indicies)
                  :index-array (make-gpu-array 
                                   indicies
                                   :dimensions (length indicies)
                                   :element-type :unsigned-short))))
    (setf *monkey* (make-entity :rotation (v! -1.57079633 0 0) :stream stream)))
  ;;set options
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))

(defun entity-matrix (entity)
  (reduce #'m4:m* (list (m4:translation (entity-position entity))
                        (m4:rotation-from-euler (entity-rotation entity))
                        (m4:scale (entity-scale entity)))))

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (prog-2 nil :world-to-cam (calculate-cam-look-at-w2c-matrix *camera*))
  (setf (entity-rotation *monkey*) (v:+ (entity-rotation *monkey*)
                                        (v! 0.00 0.0 0.0)))
  (prog-2 (entity-stream *monkey*) :model-to-world (entity-matrix *monkey*))
  (gl:flush)
  (cgl:update-display))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0) (* *frustrum-scale*
                                                (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1) *frustrum-scale*)
  (prog-2 nil :cam-to-clip *cam-clip-matrix*)
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
       (update-swank)
       (continuable (draw))))
  (defun stop-demo () (setf running nil)))
