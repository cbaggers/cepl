;; This is a simple triangle translating around a fixed point
;; It is to test basic uniform handling

(defparameter *prog-1* nil)
(defparameter *shaders* nil)
(defparameter *streams* nil)
(defparameter *move-loop-length* 200)
(defparameter *move-loop-pos* 0)

(cgl:defglstruct vert-data
    (position :type :float :length 4))

(defun init () 
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (setf *shaders* (cgl:make-shaders "2.vert" "2.frag"))
  (setf *prog-1* (cgl:make-program *shaders*))
  (setf *streams* 
	(list
	 (cgl::make-gpu-stream
	  :vao (cgl:make-vao 
		(cgl:gen-buffer :initial-contents 
				(cgl:destructuring-allocate 
				 'vert-data  
				 '((#( 0.0   0.2  0.0  1.0))
				   (#(-0.2  -0.2  0.0  1.0))
				   (#( 0.2  -0.2  0.0  1.0))))))
	  :length 3))))

(defun reshape (width height)  
  (gl:viewport 0 0 width height))

(defun draw ()
  (setf *move-loop-pos* (mod (+ 0.1 *move-loop-pos*)
			     *move-loop-length*))
  (cgl:clear :color-buffer-bit)
  (funcall *prog-1* *streams* 
  	   :offset (v:make-vector (* 0.3 (sin *move-loop-pos*))
				  (* 0.3 (cos *move-loop-pos*))))
  (cgl:flush)
  (sdl:update-display))

(defun run-demo () 
  (init)
  (reshape 640 480)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (reshape width height))
    (:idle ()
	   (cepl-utils:update-swank)
	   (base-macros:continuable (draw)))))
