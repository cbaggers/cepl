;; In this file I'm looking at the main loop and thinking
;; about how I want to do event handling. I want to have the 
;; nuts and bolts of the main loop up front where I can see it
;; to allow quick changes and also just so you can really grasp
;; what is happening. It just feels like the main loop is too 
;; important to be left to magic!

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
  (position #v(0.0 0.0 -20.0))
  (rotation #v(0.0 0.0 0.0))
  (scale #v(1.0 1.0 1.0)))

(defstruct camera 
  (position #v(0.0 0.0 0.0))
  (look-direction #v(0.0 0.0 -1.0))
  (up-direction #v(0.0 1.0 0.0)))

(defun point-camera-at (camera point)
  (setf (camera-look-direction camera)
	(v:normalize (v:- point (camera-position camera))))
  camera)

(defun calculate-cam-look-at-w2c-matrix (camera)
  (let* ((look-dir (v:normalize (camera-look-direction camera)))
	 (up-dir (v:normalize (camera-up-direction camera)))
	 (right-dir (v:normalize (v:cross look-dir up-dir)))
	 (perp-up-dir (v:cross right-dir look-dir))
	 (rot-matrix (m4:transpose
		      (m4::rotation-from-matrix3
		       (m3:make-from-rows right-dir
					  perp-up-dir
					  (v:1- #v(0.0 0.0 0.0)
						look-dir)))))
	 (trans-matrix (m4:translation 
			(v:1- #v(0.0 0.0 0.0)
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
	 (dir-to-cam #v((* sin-theta cos-phi)
			con-theta
			(* sin-theta sin-phi))))
    (v:+ cam-target (v:* dir-to-cam (v-z sphere-cam-rel-pos)))))

;----------------------------------------------

(defun init () 
  (setf *camera* (make-camera :position #v(0.0 0.0 0.0)))
  (setf *shaders* (mapcar #'cgl:make-shader `("7.vert" "7.frag")))
  (setf *prog-1* (cgl:make-program *shaders*))
  (setf *frustrum-scale* 
	(cepl-camera:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix 
			   *frustrum-scale*))
  (cgl:set-program-uniforms *prog-1* :cameratoclipmatrix *cam-clip-matrix*)

  ;;setup data 
  (let ((monkey-data 
	 (first (model-parsers:parse-obj-file "6.obj"))))
    (setf *vertex-data-list* (gethash :vertices monkey-data))
    (setf *index-data-list* (gethash :faces monkey-data)))

  (setf *vertex-data-list* (mapcar 
			    #'(lambda (x) 
				(list x (list (random 1.0) (random 1.0) (random 1.0) 1.0))) *vertex-data-list*))

  (setf *index-data-list* 
	(loop for face in *index-data-list*
	   append (mapcar #'car (subseq face 0 3))))

  ;; put in glarrays
  (setf *vertex-data-gl* 
	(cgl:alloc-array-gl 'vert-data 
			    (length *vertex-data-list*)))
  (cgl:destructuring-populate *vertex-data-gl* 
			      *vertex-data-list*)
  (setf *index-data-gl* 
	  (cgl:alloc-array-gl :short
			      (length *index-data-list*)))
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
	   (make-entity :position #v(0.0 0.0 -15.0)
			:rotation #v(-1.57079633 0.0 0.0)
			:stream stream))))
  
  ;;set options
  (cgl::clear-color 0.0 0.0 0.0 0.0)
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
  (cgl::clear-depth 1.0)
  (cgl::clear :color-buffer-bit :depth-buffer-bit)

  (cgl:set-program-uniforms *prog-1* :worldtocameramatrix 
			    (calculate-cam-look-at-w2c-matrix
			     *camera*))

  (let ((entity (first *entities*)))
    (setf (entity-rotation entity) 
	  (v:+ (entity-rotation entity)
		 #v(0.00 0.01 0.01))))
  
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

(defun update-swank ()
  (let ((connection (or swank::*emacs-connection*
			(swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

;----------------------------------------------


;; [TODO] Should look for quit event and just return that if found.
(defun collect-sdl-events ()
  (let ((x (sdl:new-event)))
    (LOOP UNTIL (= 0 (LISPBUILDER-SDL-CFFI::SDL-POLL-EVENT x))
       collect x)))


;; currently anything changed in here is going to need a restart
;; this is obviously unacceptable and will be fixed when I can
;; extract the sdl event handling from their loop system.
(defun run-demo () 
  (sdl:with-init ()
    (sdl:window 
     640 480 
     :opengl t
     :resizable t
     :flags sdl-cffi::sdl-opengl
     :opengl-attributes '((:sdl-gl-doublebuffer 1)
			  (:sdl-gl-alpha-size 0)
			  (:sdl-gl-depth-size 16) 
			  (:sdl-gl-stencil-size 8)
			  (:sdl-gl-red-size 8)
			  (:sdl-gl-green-size 8)
			  (:sdl-gl-blue-size 8)
			  (:SDL-GL-SWAP-CONTROL 1)))
    (setf (sdl:frame-rate) 0)
    (init)
    (reshape 640 480)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    ;; I've been tearing apart sdl's 'with-events' macro to see
    ;; what they include in the main loop. I'm trying to make 
    ;; as thin a layer between the user and the code as possible
    ;; do I feel that the 'with-events' macro has a little too
    ;; much magic.
    ;; Below I have ripped out the parts I need to make this 
    ;; function in the same way as 6.lisp.
    ;; I am currently experimenting with time in the protocode
    ;; folder, and as soon as I have nailed that down I will
    ;; and player controls to this (or prehaps another) example.
    (let ((loops 0)
	  (draw-timer (cepl-time:make-time-buffer))
	  (draw-stepper (cepl-time:make-stepper (/ 1000.0 60)))
	  (fps-timer (cepl-time:make-time-buffer))
	  (fps-stepper (cepl-time:make-stepper 1000))
	  (running t))
      (do-until (not running)
	(dolist (event (collect-sdl-events))
	   (case (sdl:event-type event)
	     (:quit-event
	      (setf running nil))
	     (:video-resize-event
	      (let ((height
		     (sdl::video-resize-h event))
		    (width
		     (sdl::video-resize-w event)))
		(reshape width height)))))
	(cepl-time:on-step-call (draw-stepper 
				 (funcall draw-timer))
	  (continuable (update-swank))
	  (continuable (draw)))
	(sdl::process-audio)))))


