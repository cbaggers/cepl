;; This gives us a simple moving triangle

(defparameter *loop-pos* 0.0)

(cgl:defprogram prog-4 ((position :vec4) &uniform (offset :vec2))
  (:vertex (setf gl-position (+ position (vec4 (+ (x offset) (* 4.0 (x offset) (x position))) 
					       (* (x offset) (y offset))
					       (z position) 
					       (w position)))))
  (:fragment (out output-color (vec4 (/ 1.0 (+ 0.01 (* (x offset) (y offset))))
				     (+ 0.5 (x offset)) 
				     (+ 0.2 (x offset) (y offset)) 
				     1.0))))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((data (list (v!  0.0   0.2  0.0  1.0)
		     (v! -0.2  -0.2  0.0  1.0)
		     (v!  0.2  -0.2  0.0  1.0)))
	 (gstream (cgl:make-gpu-stream-from-gpu-arrays
		   :gpu-arrays (cgl:make-gpu-array data :element-type :vec4))))

    (do-until (find :quit-event (collect-sdl-event-types))
      (continuable (cepl-utils:update-swank))
      (continuable (draw gstream))
      (sdl::process-audio))))

(defun draw (gstream)
  (setf *loop-pos* (+ 0.0002 *loop-pos*))
  (gl:clear :color-buffer-bit)
  (loop for i below 15
	do (cffi-sys:with-pointer-to-vector-data 
	       (ptr (v! (* 0.7 (sin (* (/ i 1.0) *loop-pos*))) 
			(* 3 (cos (* i *loop-pos*)))))
	     (prog-4 gstream :offset ptr)))

  (gl:flush)
  (sdl:update-display))
