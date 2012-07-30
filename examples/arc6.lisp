
(in-package :cepl-examples)


(defparameter *prog-1* nil)
(defparameter *shaders* nil)
(defparameter *vertex-data* nil)
(defparameter *index-data* nil)
(defparameter *vertex-array-gl* nil)
(defparameter *index-array-gl* nil)
(defparameter *vertex-buffer* nil)
(defparameter *index-buffer* nil)
(defparameter *buffer-layouts* nil)
(defparameter *vao* nil)
(defparameter *streams* nil)
(defparameter *frustrum-scale* nil)
(defparameter *cam->clip* nil)
(defparameter *entities* nil)
(defparameter *color-offset* nil)
(defparameter *cam->clip-uniform* nil)
(defparameter *model->cam-uniform* nil)

;; (cgl:define-interleaved-attribute-format vert-data
;;     (:type :float :components (x y z))
;;     (:type :float :components (r g b a)))


(defstruct entity 
  (pos (make-vector3 0.0 0.0 -20.0))
  (loop-angle 0.0)
  (scale 1.0))

(defun make-cam-clip-matrix ()
  (let* ((f-near 1.0)
	 (f-far 45.0)
	 (f-scale *frustrum-scale*))
    (matrix4:make-matrix4 f-scale 0.0 0.0 0.0
			  0.0 f-scale 0.0 0.0
			  0.0 0.0 (/ (+ f-far f-near)
				     (- f-near f-far)) -1.0
			  0.0 0.0 (/ (* 2 f-far f-near)
				     (- f-near f-far)) 0.0)))

(defun init () 
  ;; (setf *shaders* (mapcar #'cgl:make-shader 
  ;; 	    `("/home/baggers/Code/lisp/cepl/examples/tut6-1.vert"
  ;; 	      "/home/baggers/Code/lisp/cepl/examples/tut6-1.frag")))
  ;; (setf *prog-1* (cgl:make-program *shaders*))
  ;; (setf *frustrum-scale*
  ;; 	(defunct:calculate-frustrum-scale 45.0))
  ;; (setf *cam->clip* (cepl:make-cam-clip-matrix *frustrum-scale*))
  ;; (funcall *prog-1* nil :cameratoclipmatrix *cam->clip*)
  (setf *prog-1* 
	(defunct:make-program `("/home/baggers/Code/lisp/cepl/arc-tuts/tut6-1.vert"
				"/home/baggers/Code/lisp/cepl/arc-tuts/tut6-1.frag")))
  (setf *cam->clip-uniform* 
	(gl:get-uniform-location *prog-1* 
				 "cameraToClipMatrix"))
  (setf *model->cam-uniform* 
	(gl:get-uniform-location *prog-1*
				 "modelToCameraMatrix"))
  (setf *frustrum-scale* 
	(defunct:calculate-frustrum-scale 45.0))
  (setf *cam->clip* (make-cam-clip-matrix))
  (cgl:use-program *prog-1*)
  (gl:uniform-matrix *cam->clip-uniform*
		     4 (vector *cam->clip*) )
  ;---
  ;; (setf *vertex-data* '(((+1.0  +1.0  +1.0) 
  ;; 			  ( 0.0  1.0  0.0  1.0)) 
  ;; 			 ((-1.0  -1.0  +1.0) 
  ;; 			  ( 0.0  0.0  1.0  1.0))
  ;; 			 ((-1.0  +1.0  -1.0) 
  ;; 			  ( 1.0  0.0  0.0  1.0))
  ;; 			 ((+1.0  -1.0  -1.0) 
  ;; 			  ( 0.5  0.5  0.0  1.0))
  ;; 			 ((-1.0  -1.0  -1.0) 
  ;; 			  ( 0.0  1.0  0.0  1.0)) 
  ;; 			 ((+1.0  +1.0  -1.0) 
  ;; 			  ( 0.0  0.0  1.0  1.0))
  ;; 			 ((+1.0  -1.0  +1.0) 
  ;; 			  ( 1.0  0.0  0.0  1.0))
  ;; 			 ((-1.0  +1.0  +1.0) 
  ;; 			  ( 0.5  0.5  0.0  1.0))))
  ;; (setf *index-data* '(0  1  2 
  ;; 		       1  0  3 
  ;; 		       2  3  0 
  ;; 		       3  2  1 
  ;; 		       5  4  6 
  ;; 		       4  5  7 
  ;; 		       7  6  4 
  ;; 		       6  7  5))

  ;; (setf *vertex-array-gl* (cgl:alloc-array-gl 'vert-data (length *vertex-data*)))
  ;; (cgl:destructuring-populate *vertex-array-gl* *vertex-data*)

  ;; (setf *index-array-gl* (cgl:alloc-array-gl :short (length *index-data*)))
  ;; (loop for datum in *index-data*
  ;;      for index from 0
  ;;    do (setf (gl:glaref *index-array-gl* index) datum))

  ;; (setf *vertex-buffer* (cgl:gen-buffer))
  ;; (setf *buffer-layouts*
  ;; 	(cgl:buffer-data *vertex-buffer* *vertex-array-gl*))
  ;; (setf *index-buffer* (cgl:gen-buffer))
  ;; (cgl:buffer-data *index-buffer* *index-array-gl* 
  ;; 		   :buffer-type :element-array-buffer)
  (setf *vertex-array-gl* (defunct:make-gl-array-from-array :float
			   #(+1.0  +1.0  +1.0 
			     -1.0  -1.0  +1.0 
			     -1.0  +1.0  -1.0 
			     +1.0  -1.0  -1.0 
			     
			     -1.0  -1.0  -1.0 
			     +1.0  +1.0  -1.0 
			     +1.0  -1.0  +1.0 
			     -1.0  +1.0  +1.0 

			     0.0  1.0  0.0  1.0 
			     0.0  0.0  1.0  1.0
			     1.0  0.0  0.0  1.0
			     0.5  0.5  0.0  1.0
			     
			     0.0  1.0  0.0  1.0 
			     0.0  0.0  1.0  1.0
			     1.0  0.0  0.0  1.0
			     0.5  0.5  0.0  1.0)))
  (setf *index-array-gl* (defunct:make-gl-array-from-array :short
			  #(0  1  2 
			    1  0  3 
			    2  3  0 
			    3  2  1 
			    
			    5  4  6 
			    4  5  7 
			    7  6  4 
			    6  7  5)))
  (setf *vertex-buffer* 
	(defunct:setup-buffer :array-buffer *vertex-array-gl*))
  (setf *index-buffer* (defunct:setup-buffer 
			:element-array-buffer *index-array-gl*))

  ;; (setf *vao* (cgl:make-vao *buffer-layouts* *index-buffer*))
  ;; (setf *streams* (list
  ;; 		   (cgl::make-gl-stream 
  ;; 		    :vao *vao*
  ;; 		    :length 24)))

  (setf *vao* (gl:gen-vertex-array))
  (cgl:bind-vao *vao*)
  (cgl:bind-buffer :array-buffer *vertex-buffer*)
  (gl:enable-vertex-attrib-array 0)
  (gl:enable-vertex-attrib-array 1)

  (setf *color-offset* (* 4 3 8))
  (gl:vertex-attrib-pointer 0 3 :float :false
			    0 (cffi:null-pointer))
  (gl:vertex-attrib-pointer 1 4 :float :false 
			    0 (cffi:make-pointer 
			       *color-offset*))
  (cgl:bind-buffer :element-array-buffer *index-buffer*)

  ;;----

  (setf *entities* (list (make-entity :loop-angle 0.0)))

  ;;set options
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0))


(defun move-entity (ent)
  (let* ((new-loop (mod (+ (entity-loop-angle ent) 0.04) 360.0))
	 (new-pos (make-vector3 (* 4.0 (sin new-loop)) 
				(* 4.0 (cos new-loop)) 
				-20.0))
	 (new-scale (make-vector3 (+ 1.0 (sin new-loop))
				  (+ 1.0 (cos new-loop))
				  1.0)))
    (make-entity :pos new-pos
		 :loop-angle new-loop
		 :scale new-scale)))

;------------------------------

(defun reshape (width height)  
  (setf (matrix4:melm *cam->clip* 0 0)
	(* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam->clip* 1 1)
	*frustrum-scale*)
  (gl:viewport 0 0 width height))


(defun draw ()
  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (setf *entities* (mapcar #'move-entity *entities*))

  (loop for ent in *entities*
       do 
       (progn
	 (cgl:use-program *prog-1*)
	 (cgl:bind-vao *vao*)
	 (gl:uniform-matrix *model->cam-uniform* 4
	 		    (vector (matrix4:m*
	 			     (matrix4:translation
	 			      (entity-pos ent))
	 			     (matrix4:rotation-from-euler
	 			      (make-vector3
	 			       (entity-loop-angle ent)
	 			       (entity-loop-angle ent)
	 			       (entity-loop-angle ent)))))
	 		    nil )
	 (gl:draw-elements :triangles (gl:make-null-gl-array 
	 			       :unsigned-short)
	 		   :count 24)))
  
  (gl:flush)
  (sdl:update-display))

(defun update-swank ()
  (defunct:restartable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))

;; currently anything changed in here is going to need a restart
;; this is obviously unacceptable and will be fixed when I can
;; extract the sdl event handling from their loop system.
(defun run-demo () 
  (sdl:with-init ()
    (sdl:window 
     640 480 :opengl t
     :resizable t
     :opengl-attributes '((:sdl-gl-doublebuffer 1)
			  (:sdl-gl-alpha-size 8)
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

