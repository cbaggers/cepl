;; This is simply to get a colored triangle up on the screen

(defparameter *prog-1* nil)
(defparameter *gpu-array* nil)
(defparameter *streams* nil)

(cgl:defglstruct vert-data 
  (position :type :float :length 4)
  (colour :type :float :length 4))

(defun init () 
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (setf *prog-1* (cgl:make-program (cgl:load-shaders "1.vert" "1.frag")))
  (let* ((data '((#( 0.0     0.5  0.0  1.0)
		  #( 1.0     0.0  0.0  1.0))
		 (#( 0.5  -0.366  0.0  1.0)
		  #( 0.0     1.0  0.0  1.0))
		 (#(-0.5  -0.366  0.0  1.0)
		  #( 0.0     0.0  1.0  1.0)))))
    (setf *gpu-array* (cgl:make-gpu-array data 
					  :element-type 'vert-data))
    (setf *streams* (list (cgl:make-gpu-stream-from-gpu-arrays
			   :gpu-arrays (list *gpu-array*)  
			   :length 3)))))

(defun draw ()
  (gl:clear :color-buffer-bit)
  (cgl:draw-streams *prog-1* *streams*)
  (gl:flush)
  (sdl:update-display))

(defun run-demo () 
  (init)
  (gl:viewport 0 0 640 480)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (gl:viewport 0 0 width height))
    (:idle () (cepl-utils:update-swank)
  	      (base-macros:continuable (draw)))))
