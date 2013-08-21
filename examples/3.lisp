;; Basic 3D

(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *entities* nil)

(defglstruct vert-data 
  (position :vec3)
  (color :vec4))

(defpipeline prog-1 ((vert vert-data) &uniform 
                     (cam-to-clip :mat4) (model-to-cam :mat4))
  (:vertex (out (the-color :smooth) (vert-data-color vert))
           (let ((cam-pos (* model-to-cam 
                             (vec4 (vert-data-position vert) 1.0))))
             (setf gl-position (* cam-to-clip cam-pos))))
  (:fragment (out output-color the-color))
  (:post-compile (reshape 640 480)))

(defstruct entity 
  (stream nil)
  (pos (v! 0.0 0.0 -20.0))
  (loop-angle 0.0)
  (scale 1.0)
  (matrix nil))

(defun init () 
  (setf *frustrum-scale* (ccam:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (ccam:make-cam-clip-matrix
                           *frustrum-scale*))
  (prog-1 nil :cam-to-clip *cam-clip-matrix*)
  (let* ((verts (make-gpu-array 
                 '((#(+1.0  +1.0  +1.0) #(0.0  1.0  0.0  1.0)) 
                   (#(-1.0  -1.0  +1.0) #(0.0  0.0  1.0  1.0))
                   (#(-1.0  +1.0  -1.0) #(1.0  0.0  0.0  1.0))
                   (#(+1.0  -1.0  -1.0) #(0.5  0.5  0.0  1.0))
                   (#(-1.0  -1.0  -1.0) #(0.0  1.0  0.0  1.0)) 
                   (#(+1.0  +1.0  -1.0) #(0.0  0.0  1.0  1.0))
                   (#(+1.0  -1.0  +1.0) #(1.0  0.0  0.0  1.0))
                   (#(-1.0  +1.0  +1.0) #(0.5  0.5  0.0  1.0)))
                 :element-type 'vert-data
                 :dimensions 8))
         (indicies (make-gpu-array 
                    '(0  1  2    1  0  3    2  3  0    3  2  1 
                      5  4  6    4  5  7    7  6  4    6  7  5)
                    :element-type :unsigned-short
                    :dimensions 24))
         (stream (make-vertex-stream
                  verts
                  :index-array indicies)))
    (setf *entities* (list (make-entity :stream stream)
                           (make-entity :stream stream
                                        :loop-angle 3.14))))
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0))  

(defun move-entity (ent)
  (let* ((new-loop (+ (entity-loop-angle ent) 0.01))
         (new-pos (v! (* 8.0 (sin new-loop)) 
                      0.0
                      (+ -20.0 (* 8.0 (cos new-loop)))))
         (new-scale (v3:make-vector3 (+ 2.0 (sin new-loop))
                                     (+ 2.0 (cos new-loop))
                                     1.0)))
    (setf (entity-matrix ent)
          (matrix4:m* (matrix4:translation new-pos)
                      (matrix4:scale new-scale)))
    (setf (entity-scale ent) new-scale)
    (setf (entity-loop-angle ent) new-loop)
    (setf (entity-pos ent) new-pos)))

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (loop :for entity :in *entities*
     :do (move-entity entity)
     (prog-1 (entity-stream entity)
             :model-to-cam (entity-matrix entity)))
  (gl:flush)
  (sdl:update-display))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
        (* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1)
        *frustrum-scale*)
  (prog-1 nil :cam-to-clip *cam-clip-matrix*)
  (cgl:viewport 0 0 width height))

(defun run-demo () 
  (init)
  (reshape 640 480)
  (let ((running t))
    (loop :while running :do
       (sdl:case-events (event)
         (:quit-event (setf running nil))
         (:video-resize-event 
          (reshape (sdl:video-resize-w event)
                   (sdl:video-resize-h event))))
       (cepl-utils:update-swank)
       (continuable (draw))))) 
