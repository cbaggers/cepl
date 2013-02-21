;; This gives us a simple moving triangle

(defparameter *loop-pos* 0.0)

(cgl:defprogram prog-1 ((position :vec4) &uniform (offset :vec2))
  (:vertex (setf gl-position (+ (* (vec4 0.3 0.3 0.3 1.0) position)
                                (vec4 (x offset) 
                                      (y offset)
                                      0.0
                                      0.0))))
  (:fragment (out output-color (vec4 (tan (x offset))
                                     (sin (y offset))
                                     (cos (* (y offset) (x offset)))
                                     1.0))))

(defun draw (gstream)
  (setf *loop-pos* (+ 0.01 *loop-pos*))
  (gl:clear :color-buffer-bit)
  (loop for i below 50
     do (prog-1 gstream :offset (v! (tan (+ (* i 0.2) *loop-pos*)) 
                                    (cos (+ (* (cos i) 0.2) *loop-pos*)))))
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
    (loop :until (find :quit-event (collect-sdl-event-types)) :do
       (cepl-utils:update-swank)
       (continuable (draw gstream)))))
