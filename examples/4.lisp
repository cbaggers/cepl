;; This is to expand on using uniforms. Testing translation,
;; rotation and scaling in a 3D scene. It is also a better 
;; test of the vao generation functions

(defparameter *prog-1* nil)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *shaders* nil)
(defparameter *entities* nil)
(defparameter *camera* nil)

;; Define data formats 
(cgl:defglstruct vert-data 
  (position :type :float :length 3)
  (color :type :float :length 4))

;; The entities used in this demo
(defstruct entity 
  (stream nil)
  (position (v! 0.0 0.0 -20.0))
  (rotation (v! 0.0 0.0 0.0))
  (scale (v! 1.0 1.0 1.0)))

(defstruct camera 
  (position (v! 0.0 0.0 0.0))
  (look-direction (v! 0.0 0.0 -1.0))
  (up-direction (v! 0.0 1.0 0.0)))

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
					  (v3:v-1 (v! 0.0
						      0.0
						      0.0)
						  look-dir)))))
	 (trans-matrix 
	  (m4:translation (v3:v-1 (v! 0.0 0.0 0.0)
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

					;----------------------------------------------

(defun init () 
  (setf *camera* (make-camera :position (v! 0.0 9.0 0.0)))
  (setf *shaders* (mapcar #'cgl:make-shader `("4.vert" "4.frag")))
  (setf *prog-1* (cgl:make-program *shaders*))
  (setf *frustrum-scale* 
	(cepl-camera:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix 
			   *frustrum-scale*))
  (cgl:set-program-uniforms *prog-1* :cameratoclipmatrix *cam-clip-matrix*)

  ;;create entities
  (let* ((verts `((,(v! +1.0  +1.0  +1.0)  ,(v! 0.0  1.0  0.0  1.0)) 
		  (,(v! -1.0  -1.0  +1.0)  ,(v! 0.0  0.0  1.0  1.0))
		  (,(v! -1.0  +1.0  -1.0)  ,(v! 1.0  0.0  0.0  1.0))
		  (,(v! +1.0  -1.0  -1.0)  ,(v! 0.5  0.5  0.0  1.0))
		  (,(v! -1.0  -1.0  -1.0)  ,(v! 0.0  1.0  0.0  1.0)) 
		  (,(v! +1.0  +1.0  -1.0)  ,(v! 0.0  0.0  1.0  1.0))
		  (,(v! +1.0  -1.0  +1.0)  ,(v! 1.0  0.0  0.0  1.0))
		  (,(v! -1.0  +1.0  +1.0)  ,(v! 0.5  0.5  0.0  1.0))))
	 (indicies '(0  1  2 
		     1  0  3 
		     2  3  0 
		     3  2  1 
		     5  4  6 
		     4  5  7 
		     7  6  4 
		     6  7  5))
	 (stream (cgl:make-gpu-stream
		  :vao (cgl:make-vao 
			(cgl:gen-buffer
			 :initial-contents
			 (cgl:destructuring-allocate
			  'vert-data verts))
			:element-buffer 
			(cgl:gen-buffer 
			 :initial-contents 
			 (cgl:destructuring-allocate :unsigned-short
						     indicies)
			 :buffer-target :element-array-buffer))
		  :length (length indicies)
		  :index-type :unsigned-short)))
    (setf *entities* 
	  (list 
	   (make-entity :position (v! 0.0 0.0 -20.0)
			:stream stream)
	   (make-entity :position (v! 0.0 0.0 -25.0)
			:stream stream)
	   (make-entity :position (v! 5.0 0.0 -20.0)
			:stream stream)
	   (make-entity :position (v! 0.0 0.0 -15.0)
			:stream stream)
	   (make-entity :position (v! -5.0 0.0 -20.0)
			:stream stream))))
  
  ;;set options
  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))  

;; (defun entity-matrix (entity)
;;   (reduce #'m4:m* (list
;; 		   (m4:scale (entity-scale entity))
;; 		   (m4:rotation-from-euler (entity-rotation entity))
;; 		   (m4:translation (entity-position entity)))))

(defun entity-matrix (entity)
  (reduce #'m4:m* (list
		   (m4:translation (entity-position entity))
		   (m4:rotation-from-euler (entity-rotation entity))
		   (m4:scale (entity-scale entity)))))


					;----------------------------------------------

(defun draw ()
  (cgl::clear-depth 1.0)
  (cgl::clear :color-buffer-bit :depth-buffer-bit)

  (cgl:set-program-uniforms *prog-1* :worldtocameramatrix 
			    (calculate-cam-look-at-w2c-matrix
			     *camera*))

  (let ((entity (car *entities*)))
    (setf (entity-rotation entity) 
	  (v3:v+ (entity-rotation (car *entities*))
		 (v! 0.1 0.2 0.0))))
  
  (loop for entity in *entities*
     do (cgl::draw-streams *prog-1* (list (entity-stream entity)) 
			   :modeltoworldmatrix (entity-matrix entity)))
  (gl:flush)
  (sdl:update-display))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
  	(* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1)
  	*frustrum-scale*)
  (cgl:set-program-uniforms *prog-1* 
			    :cameratoclipmatrix *cam-clip-matrix*)
  (cgl::viewport 0 0 width height))

					;----------------------------------------------

;; currently anything changed in here is going to need a restart
;; this is obviously unacceptable and will be fixed when I can
;; extract the sdl event handling from their loop system.
(defun run-demo () 
  (init)
  (reshape 640 480)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (reshape width height))
    (:idle ()
	   (cepl-utils:update-swank)
	   (base-macros:continuable (draw)))))
