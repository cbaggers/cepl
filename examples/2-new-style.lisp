;; This is a simple triangle translating around a fixed point
;; It is to test basic uniform handling

(defparameter *prog-1* nil)
(defparameter *streams* nil)
(defparameter *move-loop-length* 100)
(defparameter *move-loop-pos* 0)

(cgl:defglstruct vert-data
    (position :type :float :length 4))

(defun init () 
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (setf *prog-1* (cgl:make-program (cgl:load-shaders "2.vert" "2.frag")))
  (setf *streams* `(,(cgl:make-gpu-stream-from-gpu-arrays
		      :length 3
		      :gpu-arrays (cgl:make-gpu-array 
				   '((#( 0.0   0.2  0.0  1.0))
				     (#(-0.2  -0.2  0.0  1.0))
				     (#( 0.2  -0.2  0.0  1.0)))
				   :element-type 'vert-data)))))

(defun reshape (width height)  
  (gl:viewport 0 0 width height))

(defun draw ()
  (setf *move-loop-pos* (mod (+ 0.06 *move-loop-pos*) 
			     *move-loop-length*))
  (cgl:clear :color-buffer-bit)
  (funcall *prog-1* *streams* 
  	   :offset (v:make-vector (* 0.5 (sin *move-loop-pos*))
				  (* 0.5 (cos *move-loop-pos*))))
  (cgl:flush)
  (sdl:update-display))

(defun run-demo () 
  (init)
  (reshape 640 480)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (reshape width height))
    (:idle () (cepl-utils:update-swank)
	      (base-macros:continuable (draw)))))
