(in-package :cepl)
;; This gives us a simple moving triangle

(defparameter *vertex-stream* nil)
(defparameter *array* nil)
(defparameter *loop* 0.0)

(defun-g vert ((position :vec4) &uniform (i :int) (loop :float))
  (let ((pos (v! (* (s~ position :xyz) 0.3) 1.0)))
    (+ pos (let ((i (/ (+ (float i)) 2)))
             (v! (sin (+ i loop))
                 (cos (+ (sin i) loop))
                 0.0 0.0)))))

(defun-g frag (&uniform (loop :float))
  (v! (cos loop) (sin loop) 0.4 1.0))

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (setf *loop* (+ 0.04 *loop*))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (ttm:update)
  (loop :for i :below 37 :do
     (let ((i (/ i 2.0)))
       (map-g #'prog-1 *vertex-stream* :i i :loop *loop*)))
  (gl:flush)
  (update-display))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (apply #'gl:viewport 0 0 +default-resolution+)
    (setf *array* (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                        (v! -0.2  -0.2  0.0  1.0)
                                        (v!  0.2  -0.2  0.0  1.0))
                                  :element-type :vec4
                                      :dimensions 3))
    (setf *vertex-stream* (make-buffer-stream *array*))
    (loop :while running :do (continuable (step-demo))))
  (defun stop-demo () (setf running nil)))

(evt:observe (cepl.events.sdl:|sys|)
  (when (typep e 'cepl.events.sdl:will-quit) (stop-demo)))
