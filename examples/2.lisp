;; This gives us a simple moving triangle

(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *loop* 0.0)

(defpipeline prog-1 ((position :vec4) &uniform (offset :vec4) (loop :float))
  (:vertex (setf gl-position (+ (* (v! 0.7 0.7 0.7 1.0) position) offset)))
  (:fragment (out output-color (v! (x offset) (sin loop) 0.3 1.0))))

(defun draw (gstream)
  (setf *loop* (+ 0.01 *loop*))
  (gl:clear :color-buffer-bit)  
  (loop :for i :below 23 :do
       (let ((i (/ i 2.0)))
         (prog-1 gstream :offset (v! (sin (+ (tan (sin i)) *loop*)) 
                                     (sin (cos (+ i (cos *loop*)))) 
                                     0 0)
                         :loop *loop*)))
  (gl:flush)
  (sdl:update-display))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (cgl:viewport 0 0 640 480)
  (setf *gpu-array* (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                          (v! -0.2  -0.2  0.0  1.0)
                                          (v!  0.2  -0.2  0.0  1.0))
                                    :element-type :vec4
                                    :dimensions 3))
  (setf *vertex-stream* (make-vertex-stream *gpu-array*))
  (loop :until (find :quit-event (sdl:collect-event-types)) :do
     (cepl-utils:update-swank)
     (continuable (draw *vertex-stream*))))
