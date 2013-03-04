;; This gives us a simple moving triangle

(defparameter *loop-pos* 0.0)

(cgl:defprogram prog-1 ((position :vec4) &uniform (offset :vec4))
  (:vertex (setf gl-position (+ position offset)))
  (:fragment (out output-color (vec4 1.0 1.0 1.0 1.0))))

(defun draw (gstream)
  (setf *loop-pos* (+ 0.01 *loop-pos*))
  (gl:clear :color-buffer-bit)
  (prog-1 gstream :offset (v! 0 0 0 0))
  (gl:flush)
  (sdl:update-display))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((data (cgl:make-gpu-array 
                (list (v!  0.0   0.2  0.0  1.0)
                      (v! -0.2  -0.2  0.0  1.0)
                      (v!  0.2  -0.2  0.0  1.0))
                :element-type :vec4))
         (gstream (cgl:make-gpu-stream-from-gpu-arrays
                   :gpu-arrays data)))
    (loop :until (find :quit-event (sdl:collect-event-types))
       :do
       (cepl-utils:update-swank)
       (continuable (draw gstream)))))
