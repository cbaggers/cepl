;; More 3D - Multiple objects rotating

(defglstruct vert-data 
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

(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *entities* nil)
(defparameter *camera* nil)

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))
(defun make-entity (&key pos e-stream)
  (make-instance 'entity :pos pos :e-stream e-stream))

(defclass camera ()
  ((position :initform (v! 0 0 0) :initarg :pos :accessor pos)
   (look-direction :initform (v! 0 0 -1) :initarg :look-dir :accessor look-dir)
   (up-direction :initform (v! 0 1 0) :initarg :up-dir :accessor up-dir)))

(defun point-camera-at (camera point)
  (setf (look-dir camera)
        (v:normalize (v:- point (pos camera)))) camera)

(defun calculate-cam-look-at-w2c-matrix (camera)
  (let* ((look-dir (v3:normalize (look-dir camera)))
         (up-dir (v3:normalize (up-dir camera)))
         (right-dir (v3:normalize (v3:cross look-dir up-dir)))
         (perp-up-dir (v3:cross right-dir look-dir))
         (rot-matrix (m4:transpose
                      (m4:rotation-from-matrix3
                       (m3:make-from-rows right-dir
                                          perp-up-dir
                                          (v3:v-1 (v! 0 0 0) look-dir)))))
         (trans-matrix (m4:translation (v3:v-1 (v! 0 0 0) (pos camera)))))
    (m4:m* rot-matrix trans-matrix)))

(defun resolve-cam-position (sphere-cam-rel-pos cam-target)
  (let* ((phi (radians (v-x sphere-cam-rel-pos)))
         (theta (radians (+ 90.0 (v-y sphere-cam-rel-pos))))
         (sin-theta (sin theta))
         (dir-to-cam (v! (* sin-theta (cos phi))
                         (cos theta)
                         (* sin-theta (sin phi)))))
    (v3:v+ cam-target (v3:v* dir-to-cam (v-z sphere-cam-rel-pos)))))

(defun init () 
  (setf *camera* (make-instance 'camera :pos (v! 0 9 0)))
  (setf *frustrum-scale* (ccam:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (ccam:make-cam-clip-matrix *frustrum-scale*))
  (prog-2 nil :cam-to-clip *cam-clip-matrix*)
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
    (setf *entities* `(,(make-entity :pos (v!  0 0 -20) :e-stream e-stream)
                        ,(make-entity :pos (v!  0 0 -25) :e-stream e-stream)
                        ,(make-entity :pos (v!  5 0 -20) :e-stream e-stream)
                        ,(make-entity :pos (v!  0 0 -15) :e-stream e-stream)
                        ,(make-entity :pos (v! -5 0 -20) :e-stream e-stream))))  
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

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (prog-2 nil :world-to-cam (calculate-cam-look-at-w2c-matrix *camera*))  
  (loop :for entity :in *entities* :do
     (setf (rot entity) (v:+ (rot entity) (v! 0.01 0.02 0)))
     (prog-2 (e-stream entity) :model-to-world (entity-matrix entity)))
  (gl:flush)
  (cgl:update-display))

(defun reshape (width height)  
  (setf (m4:melm *cam-clip-matrix* 0 0) (* *frustrum-scale* (/ height width)))
  (setf (m4:melm *cam-clip-matrix* 1 1) *frustrum-scale*)
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
       (cepl-utils:update-swank)
       (continuable (draw))))
  (defun stop-demo () (setf running nil)))
