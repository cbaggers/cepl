;; loading a monkey head :D

(defparameter *near* 1.0)
(defparameter *far* 1000.0)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *monkey* nil)
(defparameter *camera* nil)
(defparameter *light-direction* 0.0)

(cgl:defglstruct vert-data 
  (position :vec3)
  (diffuse-color :vec4)
  (normal :vec3))

(cgl:defpipeline prog-1
    ((vert vert-data) &uniform (dir-to-light :vec3)
     (light-intensity :vec4) (norm-model-to-cam :mat3)
     (cam-to-clip :mat4) (model-to-cam :mat4)
     (ambient-intensity :float))
  (:vertex (setf gl-position (* cam-to-clip
                                (* model-to-cam				   
                                   (vec4 (vert-data-position
                                          vert) 1.0))))
           (out (interp-color :smooth) 
                (+ (* light-intensity 
                     (clamp (dot (normalize
                                  (* norm-model-to-cam
                                     (vert-data-normal vert)))
                                 dir-to-light) 
                            0.0 1.0))
                   (* (vec4 1.0 1.0 1.0 0.0) 
                      ambient-intensity))))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480 *near* *far*)))

(cgl:defpipeline prog-2
    ((vert vert-data) &uniform (dir-to-light :vec3) 
     (light-intensity :vec4) (norm-model-to-cam :mat3)
     (cam-to-clip :mat4) (model-to-cam :mat4)
     (ambient-intensity :float))
  (:vertex (setf gl-position 
                 (* cam-to-clip
                    (* model-to-cam				   
                       (vec4 (vert-data-position vert) 1.0))))
           (out (interp-color :smooth) 
                (+ (* light-intensity 
                      (vert-data-diffuse-color vert)
                      (clamp (dot (normalize 
                                   (* norm-model-to-cam
                                      (vert-data-normal vert)))
                                  dir-to-light) 
                             0.0 1.0))
                   (* (vert-data-diffuse-color vert)
                      ambient-intensity))))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480 *near* *far*)))

(defstruct entity 
  (stream nil)
  (position (v! 0 3 2))
  (rotation (v! 0 0 0))
  (scale (v! 1 1 1)))

(defstruct camera 
  (position (v! 0 0 0))
  (look-direction (v! 0 0 -1))
  (up-direction (v! 0 1 0)))

(defun point-camera-at (camera point)
  (setf (camera-look-direction camera)
        (v:normalize (v:- point (camera-position camera))))
  camera)

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

(defun load-lisp-model (filename)
  (let* ((monkey-data (utils:safe-read-from-string (utils:file-to-string filename)))
         (verts (loop for vert in (first monkey-data)
                   collect (list (v:* (v:merge-into-vector (first vert)) (v! 1 1 1)) 
                                 (v:merge-into-vector (second vert))
                                 (v:merge-into-vector (third vert)))))
         (stream (cgl:make-gpu-stream-from-gpu-arrays
                  (cgl:make-gpu-array verts :element-type 'vert-data
                                      :dimensions (length verts))
                  :length (length (second monkey-data))
                  :indicies-array (cgl:make-gpu-array
                                   (second monkey-data)
                                   :element-type :unsigned-short
                                   :dimensions (length (second monkey-data))))))
    (make-entity :rotation (v! -1.57079633 1 0) :stream stream)))

(defun init () 
  (setf *camera* (make-camera :position (v! 0 3 6)))
  (setf *frustrum-scale* (cepl-camera:calculate-frustrum-scale 45.0))
  (prog-1 nil :cam-to-clip (ccam:make-cam-clip-matrix *frustrum-scale*))
  (prog-2 nil :cam-to-clip (ccam:make-cam-clip-matrix *frustrum-scale*))
  
  ;;create monkey
  (setf *monkey* (load-lisp-model "monkey.data"))

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
  (reduce #'m4:m* 
          (list (m4:translation (entity-position entity))
                (m4:rotation-from-euler (entity-rotation entity))
                (m4:scale (entity-scale entity)))))

(defun draw ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((world-to-cam-matrix (calculate-cam-look-at-w2c-matrix
                               *camera*))
         (model-to-cam-matrix (m4:m* world-to-cam-matrix 
                                     (entity-matrix *monkey*)))
         (normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
         (light-vec (v! 0 1 0 0))
         (cam-light-vec (m4:mcol*vec4 world-to-cam-matrix
                                      light-vec)))
    (setf (entity-rotation *monkey*) 
          (v! (+ 0.002 (v-x (entity-rotation *monkey*)))
              (+ 0.001 (v-y (entity-rotation *monkey*)))
              (v-z (entity-rotation *monkey*))))
    (prog-1 (entity-stream *monkey*) 
            :dir-to-light (v! (v-x cam-light-vec) 
                              (v-y cam-light-vec)
                              (v-z cam-light-vec))
            :light-intensity  (v! 0 1 0 1)
            :model-to-cam model-to-cam-matrix
            :norm-model-to-cam normal-to-cam-matrix
            :ambient-intensity 0.2))
  (gl:flush)
  (sdl:update-display))

(defun reshape (width height near far)
  (prog-1 nil :cam-to-clip (cepl-camera:make-cam-clip-matrix 
                            *frustrum-scale* near far))
  (prog-2 nil :cam-to-clip (cepl-camera:make-cam-clip-matrix 
                            *frustrum-scale* near far))
  (gl:viewport 0 0 width height))

(defun run-demo () 
  (init)
  (reshape 640 480 *near* *far*)  
  (let ((running t))
    (loop :while running :do
       (sdl:case-events (event)
         (:quit-event (setf running nil))
         (:video-resize-event 
          (reshape (sdl:video-resize-w event)
                   (sdl:video-resize-h event)
                   *near* *far*)))
       (cepl-utils:update-swank)
       (continuable (draw)))))
