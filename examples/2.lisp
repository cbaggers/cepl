;; This gives us a simple moving triangle

(defparameter *loop-pos* 0.0)

(cgl:defpipeline prog-1 ((position :vec4)
                         &uniform (offset :vec4))
  (:vertex (setf gl-position (+ (* (v! 0.7 0.7 0.7 1.0)
                                   position)
                                offset)))
  (:fragment (out output-color (vec4 (x offset)
                                     (y offset)
                                     0.3
                                     1.0))))

(defun draw (gstream)
  (setf *loop-pos* (+ 0.008 *loop-pos*))
  (gl:clear :color-buffer-bit)
  
  (loop for j below 4 do
       (let ((*loop-pos* (+ *loop-pos* (* j 1.3))))
         (loop for i below 15 do
              (prog-1 gstream 
                      :offset (v! (* 0.8 (sin (+ (/ (cos i) 5.0) *loop-pos*)))
                                  (* 0.8 (tan (+ (/ i 2.0) *loop-pos*)))
                                  0
                                  0)))))
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
         (gstream (cgl:make-gpu-stream-from-gpu-arrays data)))
    (loop :until (find :quit-event (sdl:collect-event-types))
       :do
       (cepl-utils:update-swank)
       (continuable (draw gstream)))))
