;; This is simply to get a colored triangle up on the screen

(defparameter *prog-1* nil)
(defparameter *streams* nil)

(cgl:defglstruct vert-data 
  (position :type :float :length 4)
  (colour :type :float :length 4))

(defun init () 
  (setf *prog-1* (apply #'cgl:make-program (cgl:make-shaders "1.vert" 
							     "1.frag")))
  (setf *streams* 
	(list (cgl:make-gpu-stream 
	       :vao (cgl:make-vao
		     (cgl:gen-buffer 
		      :initial-contents 
		      (cgl:destructuring-allocate 
		       'vert-data  
		       `((,(v!  0.0     0.5  0.0  1.0)
			  ,(v!  1.0     0.0  0.0  1.0))
			 (,(v!  0.5  -0.366  0.0  1.0)
			  ,(v!  0.0     1.0  0.0  1.0))
			 (,(v! -0.5  -0.366  0.0  1.0)
			  ,(v!  0.0     0.0  1.0  1.0))))))
	       :length 3))))

(defun draw ()
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  (cgl:draw-streams *prog-1* *streams*)
  (gl:flush)
  (sdl:update-display))

(defun init-and-run-demo ()
  (init)
  (main-loop))

(defun main-loop () 
  (gl:viewport 0 0 640 480)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (gl:viewport 0 0 width height))
    (:idle ()
	   (base-macros:continuable (cepl-utils:update-swank))
	   (base-macros:continuable (draw)))))
