;; This is a simple triangle translating around a fixed point
;; It is to test basic uniform handling

(defparameter *prog-1* nil)
(defparameter *shaders* nil)
(defparameter *streams* nil)
(defparameter *move-loop-length* 100)
(defparameter *move-loop-pos* 0)

(cgl:defglstruct vert-data
    (position :type :float :length 4))

(defun init () 
  (setf *shaders* (cgl:make-shaders "2.vert" "2.frag"))
  (setf *prog-1* (cgl:make-program *shaders*))

  (let* ((data '((( 0.0   0.2  0.0  1.0))
		 ((-0.2  -0.2  0.0  1.0))
		 (( 0.2  -0.2  0.0  1.0)))) 
	 (gl-array (cgl:make-gl-array :element-type 'vert-data
				      :initial-contents data))
	 (gpu-array (cgl:make-gpu-array 
		     :initial-contents gl-array)))
    (setf *streams* (list (cgl:make-gpu-stream-from-gpu-arrays
			   :gpu-arrays (list gpu-array)  
			   :length 3))))
    (cgl:clear-color 0.0 0.0 0.0 0.0))

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
    (:idle ()
	   (base-macros:continuable (cepl-utils:update-swank))
	   (base-macros:continuable (draw)))))
