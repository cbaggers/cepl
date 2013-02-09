;; This gives us a simple moving triangle

(defparameter *loop-pos* 0.0)

(cgl:defprogram prog-1 ((position :vec4)
			&uniform (loop-pos :float))
  (:vertex (setf gl-position
		 (+ position (v! (* 0.5 (sin loop-pos))
				 (* 0.5 (cos loop-pos))
				 0.0 0.0))))
  (:fragment (out output-color (v! 1.0 0.0 1.0 1.0))))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((data `(,(v!  0.0   0.2  0.0  1.0)
		 ,(v! -0.2  -0.2  0.0  1.0)
		 ,(v!  0.2  -0.2  0.0  1.0)))
	 (gstream (cgl:make-gpu-stream-from-gpu-arrays
		   :gpu-arrays (cgl:make-gpu-array 
				data :element-type :vec4))))
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:w width :h height)
                           (gl:viewport 0 0 width height))
      (:idle () (cepl-utils:update-swank)
             (base-macros:continuable 
	       (draw gstream))))))

(defun draw (gstream)
  (setf *loop-pos* (+ 0.06 *loop-pos*))
  (gl:clear :color-buffer-bit)
  (prog-1 gstream :loop-pos *loop-pos*)
  (gl:flush)
  (sdl:update-display))
