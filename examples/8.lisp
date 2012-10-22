;; Lets play with directional lighting

(defparameter *near* 1.0)
(defparameter *far* 1000.0)
(defparameter *program-1* nil)
(defparameter *program-2* nil)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *monkey* nil)
(defparameter *camera* nil)
(defparameter *light-direction* 0.0)

(cgl:defglstruct vert-data 
  (position :type :float :length 3)
  (colour :type :float :length 4)
  (normal :type :float :length 3))

(defstruct entity 
  (stream nil)
  (position (v! 0 0 0))
  (rotation (v! -1.5707964 1.0 0))
  (scale 1.0))

(defstruct camera 
  (position (v! 0 0 4))
  (look-direction (v! 0 0 -1))
  (up-direction (v! 0 1 0)))

(defun point-camera-at (camera point)
  (setf (camera-look-direction camera)
	(vector3:normalize (vector3:v- point 
				       (camera-position camera))))
  camera)

(defun calculate-cam-look-at-w2c-matrix (camera)
  (let* ((look-dir (v3:normalize (camera-look-direction camera)))
	 (up-dir (v3:normalize (camera-up-direction camera)))
	 (right-dir (v3:normalize (v3:cross look-dir up-dir)))
	 (perp-up-dir (v3:cross right-dir look-dir))
	 (rot-matrix (m4:transpose
		      (m4::rotation-from-matrix3
		       (m3:make-from-rows right-dir
					  perp-up-dir
					  (v3:v-1 (v! 0 0 0)
						  look-dir)))))
	 (trans-matrix (m4:translation (v3:v-1 (v! 0 0 0)
					       (camera-position camera)))))
    (m4:m* rot-matrix trans-matrix)))

;----------------------------------------------


(defun load-lisp-model (filename)
  (let* ((monkey-data (utils:safe-read-from-string (utils:file-to-string filename)))
	 (verts (loop for vert in (first monkey-data)
		   collect (list (v:swizzle (first vert)) 
				 (v:swizzle (second vert))
				 (v:swizzle (third vert)))))
	 (stream (cgl:make-gpu-stream-from-gpu-arrays
		  :length (length (second monkey-data))
		  :gpu-arrays (cgl:make-gpu-array verts :element-type 'vert-data)
		  :indicies-array (cgl:make-gpu-array (second monkey-data)
						      :element-type :unsigned-short
						      :index-array t))))
    (make-entity :rotation (v! -1.57079633 1 0) :stream stream)))

(defun init () 
  (setf *camera* (make-camera :position (v! 0 3 6)))
  (let ((shaders (cgl:load-shaders "8-dir-vertex-lighting-pn.vert" 
				   "8-dir-vertex-lighting-pcn.vert" 
				   "8.frag")))
    (setf *program-1* (cgl:make-program (first shaders) (third shaders)))
    (setf *program-2* (cgl:make-program (second shaders) (third shaders))))
  (setf *frustrum-scale* (cepl-camera:calculate-frustrum-scale 45.0))
  (cgl:set-program-uniforms *program-1* :cameratoclipmatrix 
			    (cepl-camera:make-cam-clip-matrix *frustrum-scale*))
  (cgl:set-program-uniforms *program-2* :cameratoclipmatrix 
			    (cepl-camera:make-cam-clip-matrix *frustrum-scale*))
  
  ;;create monkey
  (setf *monkey* (load-lisp-model "monkey2.data"))
  
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


;; (m4:scale (entity-scale entity))
(defun entity-matrix (entity)
  (reduce #'m4:m* (list
		   (m4:translation (entity-position entity))
		   (m4:rotation-from-euler (entity-rotation entity)))))


;----------------------------------------------

(defun draw ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (setf *light-direction* (+ *light-direction* 0.1))
  (let* ((horizontal-length 1.0)
	 (vertical-length 0.0)
	 (cam-w2c (calculate-cam-look-at-w2c-matrix *camera*))
	 (model-to-cam-matrix (m4:m* cam-w2c
				     (entity-matrix *monkey*)))
	 (normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
	 (light-vec (v:normalize (v! (* (sin *light-direction*) horizontal-length) 
				     vertical-length
				     (* (cos *light-direction*) horizontal-length) 
				     0.0)))
	 (cam-light-vec (m4:mcol*vec4 cam-w2c light-vec)))
    (setf (entity-rotation *monkey*) (v! (v-x (entity-rotation *monkey*))
    					 (+ 0.01 (v-y (entity-rotation *monkey*)))
    					 (+ 0.02 (v-z (entity-rotation *monkey*)))))
    (cgl:draw-stream *program-2* 
		     (entity-stream *monkey*) 
		      :dirtolight (v! (v-x cam-light-vec) (v-y cam-light-vec) (v-z cam-light-vec))
		      :lightintensity (v! 1 1 1 1)
		      :modeltocameramatrix model-to-cam-matrix
		      :normalmodeltocameramatrix normal-to-cam-matrix
		      :ambientintensity 0.2))
  (gl:flush)
  (sdl:update-display))

;----------------------------------------------

(defun reshape (width height near far)
  (cgl:set-program-uniforms *program-1* :cameratoclipmatrix (cepl-camera:make-cam-clip-matrix 
							  *frustrum-scale* near far))
  (cgl:set-program-uniforms *program-2* :cameratoclipmatrix (cepl-camera:make-cam-clip-matrix 
							   *frustrum-scale* near far))
  (gl:viewport 0 0 width height))

;----------------------------------------------

(defun run-demo () 
  (init)
  (reshape 640 480 *near* *far*)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (reshape width height *near* *far*))
    (:idle ()
	   (cepl-utils:update-swank)
	   (base-macros:continuable (draw)))))
