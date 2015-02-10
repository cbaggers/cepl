(in-package :cepl)
;; This gives us a simple moving triangle

(in-package :cepl)
(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *loop* 0.1)

(defsfun calc-offset ((i :float) (loop :float))
  (let ((i (/ i 2)))
    (return (v! (sin (+ i loop))
                (cos (+ (sin i) loop))
                0.0 0.0))))

(defvshader vert ((position :vec4) &uniform (i :int) (loop :float))
  (let ((pos (v! (* (s~ position :xyz) 0.3) 1.0)))
    (setf gl-position (+ pos (calc-offset (+ (float i)) loop)) )))

(deffshader frag (&uniform (loop :float))
  (out output-color (v! (cos loop) (sin loop) 0.4 1.0)))

(defpipeline prog-1 ((position :vec4) &uniform (i :int) (loop :float))
  vert frag)

;;; ---------------------------------------------------------------------------
(defun draw (gstream)
  (setf *loop* (+ 0.04 *loop*))
  (gl:clear :color-buffer-bit :depth-buffer-bit)  
  (ttm:update)
  (loop :for i :below 37 :do
     (let ((i (/ i 2.0)))
       (gmap #'prog-1 gstream :i i :loop *loop*)))
  (gl:flush)
  (cgl:update-display))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (apply #'gl:viewport 0 0 cgl:+default-resolution+)
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
