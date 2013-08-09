;; This is to expand on using uniforms. Testing translation,
;; rotation and scaling in a 3D scene. It is also a better 
;; test of the vao generation functions

(cgl:defglstruct vert-data 
  (position :vec3)
  (color :vec4))

(cgl:defpipeline prog-2 
    ((vert vert-data) &uniform (cam-to-clip :mat4)
		      (world-to-cam :mat4) (model-to-world :mat4))
  (:vertex (setf gl-position (* cam-to-clip
				(* world-to-cam 
				   (* model-to-world
				      (vec4 (vert-data-position vert)
					    1.0)))))
	   (out (interp-color :smooth) (vert-data-color vert)))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480)))

(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *entities* nil)
(defparameter *camera* nil)

(defstruct entity 
  (stream nil)
  (position (v! 0 0 -20))
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
  (setf *camera* (make-camera :position (v! 0 9 0)))
  (setf *frustrum-scale* (ccam:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (ccam:make-cam-clip-matrix 
			   *frustrum-scale*))
  (prog-2 nil :cam-to-clip *cam-clip-matrix*)

  (let* ((verts (cgl:make-gpu-array 
		 `((,(v! +1  +1  +1)  ,(v! 0  1  0  1)) 
		  (,(v! -1  -1  +1)  ,(v! 0  0  1  1))
		  (,(v! -1  +1  -1)  ,(v! 1  0  0  1))
		  (,(v! +1  -1  -1)  ,(v! 0.5  0.5  0  1))
		  (,(v! -1  -1  -1)  ,(v! 0  1  0  1)) 
		  (,(v! +1  +1  -1)  ,(v! 0  0  1  1))
		  (,(v! +1  -1  +1)  ,(v! 1  0  0  1))
		  (,(v! -1  +1  +1)  ,(v! 0.5  0.5  0  1)))
		 :element-type 'vert-data
         :dimensions 8))
	 (indicies (cgl:make-gpu-array 
		    '(0 1 2   1 0 3   2 3 0   3 2 1
		      5 4 6   4 5 7   7 6 4   6 7 5)
            :dimensions 24 :element-type :unsigned-short))
	 (stream (cgl:make-gpu-stream-from-gpu-arrays
              verts :indicies-array indicies)))
    (setf *entities* 
	  `(,(make-entity :position (v!  0 0 -20) :stream stream)
	    ,(make-entity :position (v!  0 0 -25) :stream stream)
	    ,(make-entity :position (v!  5 0 -20) :stream stream)
	    ,(make-entity :position (v!  0 0 -15) :stream stream)
	    ,(make-entity :position (v! -5 0 -20) :stream stream))))
  
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
  (reduce #'m4:m* (list
                   (m4:translation (entity-position entity))
                   (m4:rotation-from-euler (entity-rotation entity))
                   (m4:scale (entity-scale entity)))))

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)

  (prog-2 nil :world-to-cam (calculate-cam-look-at-w2c-matrix
			     *camera*))

  (loop :for entity :in *entities* :do
    (setf (entity-rotation entity) 
	  (v:+ (entity-rotation entity) (v! 0.01 0.02 0))))
  
  (loop for entity in *entities*
	do (prog-2 (entity-stream entity) 
		   :model-to-world (entity-matrix entity)))
  (gl:flush)
  (sdl:update-display))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
  	(* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1)
  	*frustrum-scale*)
  (prog-2 nil  :cam-to-clip *cam-clip-matrix*)
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
