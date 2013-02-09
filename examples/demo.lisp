;; This gives us a simple moving triangle

(defparameter *loop-pos* 0.0)

(cgl:defprogram prog-4 ((position :vec4) &uniform (offset :vec4))

  (:vertex (setf gl-position (+ position offset)))

  (:fragment (out output-color (vec4 1.0
				     1.0
				     1.0 
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
  (gl:clear :color-buffer-bit)
  
  ;; (setf *loop-pos* (+ 0.01 *loop-pos*))

  (cffi-sys:with-pointer-to-vector-data 
	 
	 (ptr (v! 0.0
		  0.0
		  0.0
		  0.0))
    (prog-4 gstream :offset ptr)
    
    (gl:flush))
  (sdl:update-display))
