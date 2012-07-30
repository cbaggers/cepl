;; This is just to get a feel of SDL
;; In the arc-tuts folder I was going through online tutorials
;; but these were usign GLUT which takes control of your main 
;; loop which isn't really great for flexible programming.


(in-package :cepl-examples)


(defparameter *prog-1* nil)
(defparameter *shaders* nil)
(defparameter *vertex-array* nil)
(defparameter *vertex-array-gl* nil)
(defparameter *vertex-buffer* nil)
(defparameter *buffer-layouts* nil)
(defparameter *vao* nil)
(defparameter *streams* nil)
(defparameter *move-loop-length* 100)
(defparameter *move-loop-pos* 0)


(cgl:define-interleaved-attribute-format vert-data
    (:type :float :components (x y z w)))


;;(0 4 :FLOAT NIL 0 #.(SB-SYS:INT-SAP #X00000000))
(defun init () 
  (setf *shaders* (mapcar #'cgl:make-shader `("2.vert" "2.frag")))
  (setf *prog-1* (cgl:make-program *shaders*))

  (setf *vertex-array* '((( 0.0   0.2  0.0  1.0))
			 ((-0.2  -0.2  0.0  1.0))
			 (( 0.2  -0.2  0.0  1.0))))

  (setf *vertex-array-gl* (cgl:alloc-array-gl 'vert-data (length *vertex-array*)))
  (cgl:destructuring-populate *vertex-array-gl* *vertex-array*)
  (setf *vertex-buffer* (cgl:gen-buffer))
  (setf *buffer-layouts*
  	(cgl:buffer-data *vertex-buffer* *vertex-array-gl*))
  (setf *vao* (cgl:make-vao *buffer-layouts*))
  (setf *streams* (list
		   (cgl::make-gl-stream 
		    :vao *vao*
		    :length 3))))


;------------------------------

(defun reshape (width height)  
  (gl:viewport 0 0 width height))


(defun draw ()
  (setf *move-loop-pos* (mod (+ 0.06 *move-loop-pos*) 
			     *move-loop-length*))

  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (cgl::clear :color-buffer-bit)

  (funcall *prog-1* *streams* 
  	   :offset (make-vector2 (* 0.5 (sin *move-loop-pos*))
  				 (* 0.5 (cos *move-loop-pos*))))
  
  (gl:flush)
  (sdl:update-display))

(defun update-swank ()
  (base-macros:continuable
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

