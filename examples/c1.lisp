;; This is simply to get a colored triangle up on the screen

(defparameter *prog-1* nil)
(defparameter *data* nil)
(defparameter *streams* nil)

(cgl:defglstruct vert-data 
  (position :type :float :length 4)
  (colour :type :float :length 4))

(defun init () 
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (setf *prog-1* (cgl:make-program (cgl:make-shaders "1.vert"
						     "1.frag")))
  (let* ((data '((#( 0.0     0.5  0.0  1.0)
		  #( 1.0     0.0  0.0  1.0))
		 (#( 0.5  -0.366  0.0  1.0)
		  #( 0.0     1.0  0.0  1.0))
		 (#(-0.5  -0.366  0.0  1.0)
		  #( 0.0     0.0  1.0  1.0))))
	 (gl-array (cgl:make-gl-array 'vert-data
				      :initial-contents data)))
    (setf *data* (cgl:make-gpu-array :initial-contents gl-array))
    (setf *streams* (list (cgl:make-gpu-stream-from-gpu-arrays
			   :gpu-arrays (list *data*)  
			   :length 3)))))

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
	   (base-macros:continuable (draw))))
  (print "done"))
