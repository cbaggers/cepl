;; This gives us a simple moving triangle

(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *loop* 0.0)

(defsfun calc-offset ((i :float) (loop :float))
  (let ((i (/ i 2)))
    (return (v! (sin (+ (cos i) loop))
                (cos (+ i (cos loop)))
                0.0 0.0))))

(defvshader vert ((position :vec4) &uniform (offset :vec4) (i :int) (loop :float))
  (setf gl-position (+ position (calc-offset (float i) loop))))

(deffshader frag (&uniform (loop :float)) 
  (out output-color (v! (cos loop) (sin loop) 0.3 1.0)))

(defpipeline prog-1 ((position :vec4) &uniform (offset :vec4)  
                     (i :int) (loop :float))
  #'vert #'frag)

(defun draw (gstream)
  (setf *loop* (+ 0.005 *loop*))
  (gl:clear :color-buffer-bit)  
  (loop :for i :below 25 :do
     (let ((i (/ i 2.0)))
       (prog-1 gstream :i i :loop *loop*)))
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
       (sdl2:case-events (event)
         (:quit () (setf running nil))
         (:windowevent (:data1 x :data2 y) 
                       (format t "window size is now: ~s*~s" x y)))
       (cepl-utils:update-swank)
       (continuable (draw *vertex-stream*))))
  (defun stop-demo () (setf running nil)))
