;; This is the same as 4.lisp but without vsync..hmm
;; didnt seem to work
;; unlimited framerate does though

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
;; for fps
(defparameter *loops* 0)
(defparameter *timer* (make-time-buffer))
(defparameter *stepper* (make-stepper 1000))

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

(let* ((verts '(((+1.0  +1.0  +1.0)  (0.0  1.0  0.0  1.0)) 
		  ((-1.0  -1.0  +1.0)  (0.0  0.0  1.0  1.0))
		  ((-1.0  +1.0  -1.0)  (1.0  0.0  0.0  1.0))
		  ((+1.0  -1.0  -1.0)  (0.5  0.5  0.0  1.0))
		  ((-1.0  -1.0  -1.0)  (0.0  1.0  0.0  1.0)) 
		  ((+1.0  +1.0  -1.0)  (0.0  0.0  1.0  1.0))
		  ((+1.0  -1.0  +1.0)  (1.0  0.0  0.0  1.0))
		  ((-1.0  +1.0  +1.0)  (0.5  0.5  0.0  1.0))))
	 (indicies '(0  1  2 
		     1  0  3 
		     2  3  0 
		     3  2  1 
		     5  4  6 
		     4  5  7 
		     7  6  4 
		     6  7  5))
	 (stream (cgl:make-gl-stream 
		  :vao (cgl:make-vao 
			`(,(cgl:gen-buffer
			    :initial-contents
			    (cgl:destructuring-allocate
			     'vert-data verts)))
			:element-buffer 
			(cgl:gen-buffer 
			 :initial-contents 
			 (cgl:destructuring-allocate :short
						     indicies)
			 :buffer-type :element-array-buffer))
		  :length (length indicies)
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

(defun opengl-context ()
  (let ((wm-info (cffi:foreign-alloc 'sdl-cffi::SDL-Sys-WM-info)))
    ;; Set the wm-info structure to the current SDL version.
    (sdl-cffi::set-sdl-version 
     (cffi:foreign-slot-value wm-info 
			      'sdl-cffi::SDL-Sys-WM-info 
			      'sdl-cffi::version))
    (sdl-cffi::SDL-Get-WM-Info wm-info)
    ;; For Windows
    #+windows(cffi:foreign-slot-value wm-info 
				      'sdl-cffi::SDL-Sys-WM-info 
				      'sdl-cffi::hglrc)
    ;; For X
    #-windows(cffi:foreign-slot-pointer 
	      (cffi:foreign-slot-pointer 
	       (cffi:foreign-slot-pointer wm-info
					  'sdl-cffi::SDL-Sys-WM-info
					  'sdl-cffi::info)
	       'sdl-cffi::SDL-Sys-WM-info-info
	       'sdl-cffi::x11) ;pointer
	      'sdl-cffi::SDL-Sys-WM-info-info-x11 ;type 
	      'sdl-cffi::hglrc))) ;undefined slot name

(defun entity-matrix (entity)
  (reduce #'m4:m* (list
		   (m4:translation (entity-position entity))
		   (m4:rotation-from-euler (entity-rotation entity))
		   (m4:scale (entity-scale entity)))))


;----------------------------------------------

(defun draw ()
  ;; (setf *loops* (1+ *loops*))
  ;; (on-step-call (*stepper* (funcall *timer*))
  ;;   (print *loops*)
  ;;   (setf *loops* 0))
  (cgl::clear-depth 1.0)
  (cgl::clear :color-buffer-bit :depth-buffer-bit)

  (cgl:set-program-uniforms *prog-1* :worldtocameramatrix 
			    (calculate-cam-look-at-w2c-matrix
			     *camera*))

  (let ((entity (first *entities*)))
    (setf (entity-rotation entity) 
	  (v3:v+ (entity-rotation entity)
		 (make-vector3 0.03 0.03 0.0))))
  
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
    (sdl:with-events () 
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:w width :h height) 
			   (reshape width height))
      (:idle ()
	     (base-macros:continuable (update-swank))
	     (base-macros:continuable (draw))))))
