;; More 3D - Multiple objects rotating
;; woking

(in-package :cepl)

(defglstruct vert-data 
  (position :vec3 :accessor pos)
  (color :vec4 :accessor color))

(defpipeline prog-2 ((vert vert-data) &uniform (cam-to-clip :mat4)
                     (world-to-cam :mat4) (model-to-world :mat4))
  (:vertex (setf gl-position (* cam-to-clip
                                (* world-to-cam 
                                   (* model-to-world
                                      (v! (pos vert)
                                            1.0)))))
           (out (interp-color :smooth) (color vert)))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480)))

(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *entities* nil)
(defparameter *camera* nil)

(defclass element ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))
(defun make-element (&key pos e-stream)
  (make-instance 'element :pos pos :e-stream e-stream))

(defclass cam ()
  ((position :initform (v! 0 0 0) :initarg :pos :accessor pos)
   (look-direction :initform (v! 0 0 -1) :initarg :look-dir :accessor look-dir)
   (up-direction :initform (v! 0 1 0) :initarg :up-dir :accessor up-dir)))

(defun point-cam-at (cam point)
  (setf (look-dir cam)
        (v:normalize (v:- point (pos cam)))) cam)

(defun calculate-cam-look-at-w2c-matrix (cam)
  (let* ((look-dir (v3:normalize (look-dir cam)))
         (up-dir (v3:normalize (up-dir cam)))
         (right-dir (v3:normalize (v3:cross look-dir up-dir)))
         (perp-up-dir (v3:cross right-dir look-dir))
         (rot-matrix (m4:transpose
                      (m4:rotation-from-matrix3
                       (m3:make-from-rows right-dir
                                          perp-up-dir
                                          (v3:v-1 (v! 0 0 0) look-dir)))))
         (trans-matrix (m4:translation (v3:v-1 (v! 0 0 0) (pos cam)))))
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
  (setf *camera* (make-instance 'cam :pos (v! 0 9 0)))
  (setf *frustrum-scale* (ccam::calculate-frustrum-scale 45.0))
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
    (setf *entities* `(,(make-element :pos (v!  0 0 -20) :e-stream e-stream)
                        ,(make-element :pos (v!  0 0 -25) :e-stream e-stream)
                        ,(make-element :pos (v!  5 0 -20) :e-stream e-stream)
                        ,(make-element :pos (v!  0 0 -15) :e-stream e-stream)
                        ,(make-element :pos (v! -5 0 -20) :e-stream e-stream))))  
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

(defun element-matrix (element)
  (reduce #'m4:m* (list (m4:translation (pos element))
                        (m4:rotation-from-euler (rot element))
                        (m4:scale (scale element)))))

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (prog-2 nil :world-to-cam (calculate-cam-look-at-w2c-matrix *camera*))  
  (loop :for element :in *entities* :do
     (setf (rot element) (v:+ (rot element) (v! 0.01 0.02 0)))
     (prog-2 (e-stream element) :model-to-world (element-matrix element)))
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
       (update-swank)
       (continuable (draw))))
  (defun stop-demo () (setf running nil)))
