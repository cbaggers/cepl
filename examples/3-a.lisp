;; This is to expand on using uniforms. Testing translation,
;; rotation and scaling in a 3D scene. It is also a better 
;; test of the vao generation functions


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

;; Define data formats 
(cgl:define-interleaved-attribute-format vert-data 
  (:type :float :components (x y z))
  (:type :float :components (r g b a)))

;; The entities used in this demo
(defstruct entity 
  (stream nil)
  (pos (make-vector3 0.0 0.0 -20.0))
  (loop-angle 0.0)
  (scale 1.0)
  (matrix nil))

;----------------------------------------------

(defun init () 
  (setf *shaders* (mapcar #'cgl:make-shader `("3.vert" "3.frag")))
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
  (setf *entities* 
  	(list 
  	 (make-entity :stream (cgl:make-gl-stream 
  			      :vao *vao-1*
  			      :length (length *index-data-list*)
  			      :element-type :unsigned-short))
	 (make-entity :loop-angle 180.0
		      :stream (cgl:make-gl-stream 
			       :vao *vao-1*
			       :length (length *index-data-list*)
			       :element-type :unsigned-short))))
  
  ;;set options
  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0))  

;----------------------------------------------

(defun move-entity (ent)
  (let* ((new-loop (mod (+ (entity-loop-angle ent) 0.05) 360.0))
	 (new-pos (make-vector3 (* 8.0 (sin new-loop)) 
				0.0
				(+ -20.0 (* 8.0 (cos new-loop))) ))
	 (new-scale (make-vector3 (+ 2.0 (sin new-loop))
				  (+ 2.0 (cos new-loop))
				  1.0)))
    (setf (entity-matrix ent) (matrix4:m*
    			       (matrix4:translation new-pos)
    				(matrix4:scale new-scale)))

    ;; (setf (entity-matrix ent) (matrix4:translation new-pos))

    (setf (entity-scale ent) new-scale)
    (setf (entity-loop-angle ent) new-loop)
    (setf (entity-pos ent) new-pos)
    ent))

(defun draw ()
  (cgl::clear-depth 1.0)
  (cgl::clear :color-buffer-bit :depth-buffer-bit)

  ;;move the entities
  (setf *entities* (mapcar #'move-entity *entities*))

  (loop for entity in *entities*
       do (cgl::draw-streams *prog-1* (list (entity-stream entity)) 
  		   :modeltocameramatrix (entity-matrix entity)))
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
     640 480 :opengl t
     :resizable t
     :opengl-attributes '((:sdl-gl-doublebuffer 1)
			  (:sdl-gl-alpha-size 0)
			  (:sdl-gl-depth-size 16) 
			  (:sdl-gl-stencil-size 8)
			  (:sdl-gl-red-size 8)
			  (:sdl-gl-green-size 8)
			  (:sdl-gl-blue-size 8)))
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
