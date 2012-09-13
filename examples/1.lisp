;; This is simply to get a colored triangle up on the screen

(defparameter *prog-1* nil)
(defparameter *streams* nil)

(cgl:define-interleaved-attribute-format vert-data 
  (:type :float :components (x y z w))
  (:type :float :components (r g b a)))

(defun init () 
  (setf *prog-1* (cgl:make-program (cgl:make-shaders "1.vert" 
						     "1.frag")))
  (setf *streams* 
	(list (cgl::make-gl-stream 
	       :vao (cgl:make-vao 
		     (cgl:gen-buffer 
		      :initial-contents 
		      (cgl:destructuring-allocate 
		       'vert-data  
		       '((( 0.0     0.5  0.0  1.0)
			  ( 1.0     0.0  0.0  1.0))
			 (( 0.5  -0.366  0.0  1.0)
			  ( 0.0     1.0  0.0  1.0))
			 ((-0.5  -0.366  0.0  1.0)
			  ( 0.0     0.0  1.0  1.0))))))
	       :length 3))))

(defun draw ()
  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (cgl::clear :color-buffer-bit)
  (cgl:draw-streams *prog-1* *streams*)
  (gl:flush)
  (sdl:update-display))

(defun init-and-run-demo ()
  (init)
  (main-loop))

(defun main-loop () 
  (reshape 640 480)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (gl:viewport 0 0 width height))
    (:idle ()
	   (base-macros:continuable (cepl-utils:update-swank))
	   (base-macros:continuable (draw)))))
