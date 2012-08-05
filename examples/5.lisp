;; This was meant to be a test of the awesome GLOP library instead
;; of SDL. It seems to have everything I need but it crashes 
;; immediately under my optimus setup under linux. Such a pain as 
;; it looks near perfect for my needs.



(in-package :cepl-examples)

;; Globals - Too damn many of them, but its in keeping with
;;           the tutorials online
(defparameter *prog-1* nil)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *shaders* nil)
(defparameter *vertex-data-list* nil)
(defparameter *vertex-data-gl* nil)
(defparameter *index-data-list* nil)
(defparameter *index-data-gl* nil)
(defparameter *vert-buffer* nil)
(defparameter *index-buffer* nil)
(defparameter *buffer-layout* nil)
(defparameter *vao-1* nil)
(defparameter *entities* nil)
(defparameter *camera* nil)

;; Define data formats 
(cgl:define-interleaved-attribute-format vert-data 
  (:type :float :components (x y z))
  (:type :float :components (r g b a)))

;; The entities used in this demo
(defstruct entity 
  (stream nil)
  (position (make-vector3 0.0 0.0 -20.0))
  (rotation (make-vector3 0.0 0.0 0.0))
  (scale (make-vector3 1.0 1.0 1.0)))

(defstruct camera 
  (position (make-vector3 0.0 0.0 0.0))
  (look-direction (make-vector3 0.0 0.0 -1.0))
  (up-direction (make-vector3 0.0 1.0 0.0)))

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
					  (v3:v-1 (make-vector3 0.0
								0.0
								0.0)
						  look-dir)))))
	 (trans-matrix (m4:translation (v3:v-1 (make-vector3 0.0
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
	 (dir-to-cam (make-vector3 (* sin-theta cos-phi)
				   con-theta
				   (* sin-theta sin-phi))))
    (v3:v+ cam-target (v3:v* dir-to-cam (v-z sphere-cam-rel-pos)))))

;----------------------------------------------

(defun init () 
  (setf *camera* (make-camera :position (make-vector3 0.0 9.0 0.0)))
  (setf *shaders* (mapcar #'cgl:make-shader `("4.vert" "4.frag")))
  (setf *prog-1* (cgl:make-program *shaders*))
  (setf *frustrum-scale* 
	(cepl-camera:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix 
			   *frustrum-scale*))
  (cgl:set-program-uniforms *prog-1* :cameratoclipmatrix *cam-clip-matrix*)

  ;;setup data 
  (setf *vertex-data-list* 
  	'(((+1.0  +1.0  +1.0)  (0.0  1.0  0.0  1.0)) 
  	  ((-1.0  -1.0  +1.0)  (0.0  0.0  1.0  1.0))
  	  ((-1.0  +1.0  -1.0)  (1.0  0.0  0.0  1.0))
  	  ((+1.0  -1.0  -1.0)  (0.5  0.5  0.0  1.0))
  	  ((-1.0  -1.0  -1.0)  (0.0  1.0  0.0  1.0)) 
  	  ((+1.0  +1.0  -1.0)  (0.0  0.0  1.0  1.0))
  	  ((+1.0  -1.0  +1.0)  (1.0  0.0  0.0  1.0))
  	  ((-1.0  +1.0  +1.0)  (0.5  0.5  0.0  1.0))))
  (setf *vertex-data-gl* 
  	(cgl:alloc-array-gl 'vert-data 
  			    (length *vertex-data-list*)))
  (cgl:destructuring-populate *vertex-data-gl* 
  			      *vertex-data-list*)

  (setf *index-data-list* 
  	'(0  1  2 
  	  1  0  3 
  	  2  3  0 
  	  3  2  1 
	  
  	  5  4  6 
  	  4  5  7 
  	  7  6  4 
  	  6  7  5))
  (setf *index-data-gl* 
  	(cgl:alloc-array-gl :short
  			    (length *index-data-list*)))
  ;; (cgl:destructuring-populate *index-data-gl* 
  ;; 			      *index-data-list*)
  (loop for index in *index-data-list*
       for i from 0
       do (setf (cgl::aref-gl *index-data-gl* i) index))

  ;;setup buffers
  (setf *vert-buffer* (cgl:gen-buffer))
  (setf *buffer-layout*
  	(cgl:buffer-data *vert-buffer* *vertex-data-gl*))

  (setf *index-buffer* (cgl:gen-buffer))
  (cgl:buffer-data *index-buffer* *index-data-gl* 
		   :buffer-type :element-array-buffer)

  ;;setup vaos
  (setf *vao-1* (cgl:make-vao *buffer-layout* *index-buffer*))

  ;;create entities
  (let ((stream (cgl:make-gl-stream 
  			      :vao *vao-1*
  			      :length (length *index-data-list*)
  			      :element-type :unsigned-short)))
    (setf *entities* 
	  (list 
	   (make-entity :position (make-vector3 0.0 0.0 -20.0)
			:stream stream)
	   (make-entity :position (make-vector3 0.0 0.0 -25.0)
			:stream stream)
	   (make-entity :position (make-vector3 5.0 0.0 -20.0)
			:stream stream)
	   (make-entity :position (make-vector3 0.0 0.0 -15.0)
			:stream stream)
	   (make-entity :position (make-vector3 -5.0 0.0 -20.0)
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

(defun draw (win)
  (cgl::clear-depth 1.0)
  (cgl::clear :color-buffer-bit :depth-buffer-bit)

  (cgl:set-program-uniforms *prog-1* :worldtocameramatrix 
			    (calculate-cam-look-at-w2c-matrix
			     *camera*))

  (loop for entity in *entities*
       do (setf (entity-rotation entity) 
	     (v3:v+ (entity-rotation (car *entities*))
		    (make-vector3 0.1 0.2 0.0))))

  (loop for entity in *entities*
       do (cgl::draw-streams *prog-1* (list (entity-stream entity)) 
  		   :modeltoworldmatrix (entity-matrix entity)))
  (gl:flush)
  (glop:swap-buffers win))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
  	(* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1)
  	*frustrum-scale*)
  (cgl:set-program-uniforms *prog-1* 
			    :cameratoclipmatrix *cam-clip-matrix*)
  (cgl::viewport 0 0 width height))

(defun update-swank ()
  (let ((connection (or swank::*emacs-connection*
			(swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

;----------------------------------------------

(defun run-demo ()
  (let ((win (glop:create-window "Glop test window" 640 480)))
    (init)
    (loop for evt = (glop:next-event win :blocking nil)
         with running = t
         while running
         if evt
         do (typecase evt
              (glop:key-press-event
               (when (eq (glop:keysym evt) :escape)
                 (glop:push-close-event win)))
              (glop:close-event (setf running nil))
	      (glop:resize-event (reshape 640 480)) ;need vars
              (t (format t "Unhandled event: ~A~%" evt)))
       else do (progn
		 (base-macros:continuable (update-swank))
		 (base-macros:continuable (draw win))))
    (glop:destroy-window win)))
