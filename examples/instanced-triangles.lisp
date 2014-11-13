;; Simple instancing example

(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *particle-positions* nil)
(defparameter *loop* 0.0)

(defsfun calc-offset ((i :float) (loop :float))
  (let ((i (/ i 2)))
    (return (v! (sin (+ i loop))
                (cos (+ i (sin loop)))
                0.0 
                (/ (+ 1 (sin (+ (* i 0.1) loop))) 2)))))

(defvshader vert ((position :vec4) &uniform (loop :float) (tex :sampler-1d))
  (setf gl-position (+ (v! (+ (s~ (texture tex (/ gl-instance-id 1000.0)) :xyz)
                              (* (s~ position :xyz) 0.3))
                           1)
                       (calc-offset gl-instance-id loop)))
  (out id (float gl-instance-id)))

(deffshader frag ((id :float) &uniform (loop :float))
  (out output-color (v! (+ 0.2(/ (+ (sin loop) (sin id)) 8.0))
                        (+ 0.2 (/ (+ (sin loop) (sin id)) 8.0))
                        0.4
                        1.0)))

(defpipeline prog-1 ((position :vec4) &uniform (loop :float) (tex :sampler-1d))
  vert frag)


(defun draw (gstream)
  (setf *loop* (+ 0.01 *loop*))
  (gl:clear :color-buffer-bit)    (ttm:update)
  (with-instances (1000)
    (prog-1 gstream :loop *loop* :tex *particle-positions*))
  (gl:flush)
  (cgl:update-display))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (cgl:viewport 0 0 640 480)
    (let ((pos (loop :for i :below 1000 :collect
                  (v! (/ (- (random 2000) 1000) 1000)
                      (/ (- (random 2000) 1000) 1000)))))
      (setf *gpu-array* (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                              (v! -0.2  -0.2  0.0  1.0)
                                              (v!  0.2  -0.2  0.0  1.0))
                                        :element-type :vec4
                                        :dimensions 3))
      (setf *particle-positions* (with-c-array
                          (temp (make-c-array '(1000) :vec2 :initial-contents pos))
                                   (make-texture :initial-contents temp)))
      (setf *vertex-stream* (make-vertex-stream *gpu-array*)))
    (loop :while running :do
       (case-events (event)
         (:quit () (setf running nil))
         (:windowevent (:event e :data1 x :data2 y)
                       (when (eql e sdl2-ffi:+sdl-windowevent-resized+)
                         (format t "window size is now: ~s*~s\n" x y))))
       (update-swank)
       (continuable (draw *vertex-stream*))))
  (defun stop-demo () (setf running nil)))
