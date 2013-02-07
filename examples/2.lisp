;; This gives us a simple moving triangle

(cgl:defprogram prog-1 ((position :vec4) &uniform (offset :vec2))
  (:vertex (setf gl-position (+ position (vec4 (x offset) (y offset) 0.0 0.0))))
  (:fragment (out output-color (vec4 1.0 0.0 1.0 1.0))))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((data `((,(v!  0.0   0.2  0.0  1.0))
		 (,(v! -0.2  -0.2  0.0  1.0))
		 (,(v!  0.2  -0.2  0.0  1.0))))
	 (gstream (cgl:make-gpu-stream-from-gpu-arrays
		   :gpu-arrays (cgl:make-gpu-array 
				data :element-type :vec4)))
        (move-loop-pos 0))
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:w width :h height)
                           (gl:viewport 0 0 width height))
      (:idle () 
             (cepl-utils:update-swank)
             (base-macros:continuable 
               (progn
                 (gl:clear :color-buffer-bit)
                 (setf move-loop-pos (+ 0.06 move-loop-pos))
                 (prog-1 gstream
                         :offset (v:make-vector (* 0.5 (sin move-loop-pos))
                                                (* 0.5 (cos move-loop-pos))))
                 (gl:flush)
                 (sdl:update-display)))))))
