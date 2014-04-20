;; This gives us a simple moving triangle

(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *loop* 0.0)

(defsfun calc-offset ((i :float) (loop :float))
  (let ((i (/ i 2)))
    (return (v! (sin (+ (cos i) loop))
                (cos (+ (tan i) loop))
                0.0 0.0))))

(defvshader vert ((position :vec4) &uniform (offset :vec4) (i :int) (loop :float))
  (setf gl-position (+ offset position (calc-offset (float i) loop))))

(deffshader frag (&uniform (loop :float))
  (out output-color (v! (cos loop) (sin loop) 0.3 1.0)))

(defpipeline prog-1 ((position :vec4) &uniform (offset :vec4)  
                     (i :int) (loop :float))
  vert frag)

(defparameter *fps* 0)
(let ((count 0))
  (tdefun fps ()
    (incf count)
    ((each (seconds 1)) (setf *fps* count) (setf count 0))))

(defun draw (gstream)
  (setf *loop* (+ 0.001 *loop*))
  (gl:clear :color-buffer-bit)  
  (ttm:update)
  (fps)
  (loop :for i :below 25 :do
     (let ((i (/ i 2.0)))
       (prog-1 gstream :i i :loop *loop* :offset (v! 0 0 0 0))))
  (gl:flush)
  (cgl:update-display))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (cgl:viewport 0 0 640 480)
    (setf *gpu-array* (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                            (v! -0.2  -0.2  0.0  1.0)
                                            (v!  0.2  -0.2  0.0  1.0))
                                      :element-type :vec4
                                      :dimensions 3))
    (setf *vertex-stream* (make-vertex-stream *gpu-array*))
    (loop :while running :do
       (case-events (event)
         (:quit () (setf running nil))
         (:windowevent (:event e :data1 x :data2 y)
                       (when (eql e sdl2-ffi:+sdl-windowevent-resized+)
                         (format t "window size is now: ~s*~s\n" x y))))
       (update-swank)
       (continuable (draw *vertex-stream*))))
  (defun stop-demo () (setf running nil)))
