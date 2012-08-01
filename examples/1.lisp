;; This is simply to get a colored triangle up on the screen
;; in is pretty much the 'hello world' of opengl demos


(in-package :cepl-examples)


(defparameter *prog-1* nil)
(defparameter *shaders* nil)
(defparameter *vertex-array* nil)
(defparameter *vertex-array-gl* nil)
(defparameter *vertex-buffer* nil)
(defparameter *buffer-layouts* nil)
(defparameter *vao* nil)
(defparameter *streams* nil)


(cgl:define-interleaved-attribute-format vert-data 
  (:index 0 :type :float :components (x y z w))
  (:index 1 :type :float :components (r g b a)))

(defun init () 
  (setf *shaders* (mapcar #'cgl:make-shader `("1.vert" "1.frag")))
  (setf *prog-1* (cgl:make-program *shaders*))

  (setf *vertex-array* '((( 0.0     0.5  0.0  1.0)
			  ( 1.0     0.0  0.0  1.0))

			 (( 0.5  -0.366  0.0  1.0)
			  ( 0.0     1.0  0.0  1.0))

			 ((-0.5  -0.366  0.0  1.0)
			  ( 0.0     0.0  1.0  1.0))))

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
  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (cgl::clear :color-buffer-bit)
  
  (cgl:bind-buffer :array-buffer *vertex-buffer*)


  (gl:enable-vertex-attrib-array 0)
  (gl:enable-vertex-attrib-array 1)

  (funcall *prog-1* *streams*)
  
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
