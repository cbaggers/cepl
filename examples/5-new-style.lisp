;; This is the same as 4.lisp but without vsync..hmm
;; didnt seem to work
;; unlimited framerate does though

;; Globals - Too damn many of them, but its in keeping with
;;           the tutorials online
(defparameter *prog-1* nil)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *shaders* nil)
(defparameter *entities* nil)
(defparameter *camera* nil)

;; Define data formats 
(cgl:defglstruct vert-data 
  (position :type :float :length 3)
  (colour :type :float :length 4))

;; The entities used in this demo
(defstruct entity 
  (stream nil)
  (position (v! 0.0 0.0 -3.0))
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
	 (trans-matrix (m4:translation (v3:v-1 (v! 0.0
						   0.0
						   0.0)
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
  (setf *camera* (make-camera :position (v! 0.0 0.0 0.0)))
  (setf *shaders* (cgl:load-shaders "6.vert" "6.frag"))
  (setf *prog-1* (cgl:make-program *shaders*))
  (setf *frustrum-scale* 
	(cepl-camera:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix 
			   *frustrum-scale*))
  (cgl:set-program-uniforms *prog-1* :cameratoclipmatrix *cam-clip-matrix*)

  ;;create entities
  (let* ((monkey-data (first 
		       (model-parsers:parse-obj-file "5.obj")))
	 (verts (mapcar #'(lambda (x)
			    (list x (make-array 4 :element-type 'single-float
						  :initial-contents 
						  (list (random 1.0) 
							(random 1.0) 
							(random 1.0)
							1.0)))) 
			(first monkey-data)))
	 (indicies (loop for face in (car (last monkey-data))
		      append (mapcar #'car (first face))))
	 (stream (cgl:make-gpu-stream-from-gpu-arrays
		  :length (length indicies)
		  :gpu-arrays (cgl:make-gpu-array 
			       verts
			       :element-type 'vert-data)
		  :indicies-array (cgl:make-gpu-array 
				   indicies
				   :element-type :unsigned-short
				   :index-array t))))
    (setf *entities* `(,(make-entity :rotation (v! -1.57079633 0 0)
				     :stream stream))))
  
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
  (reduce #'m4:m* (list
		   (m4:translation (entity-position entity))
		   (m4:rotation-from-euler (entity-rotation entity))
		   (m4:scale (entity-scale entity)))))


;----------------------------------------------

(defun draw ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (cgl:set-program-uniforms *prog-1* :worldtocameramatrix 
			    (calculate-cam-look-at-w2c-matrix
			     *camera*))

  (let ((entity (first *entities*)))
    (setf (entity-rotation entity) 
	  (v3:v+ (entity-rotation entity)
		 (v! 0.00 0.001 0.002))))
  
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
  (gl:viewport 0 0 width height))

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
