(in-package :cepl)
;; This gives us a simple moving triangle

(defparameter *vertex-stream* nil)
(defparameter *loop* 0.0)

(defun-g calc-offset ((i :float) (loop :float))
  (let ((i (/ i 2)))
    (v! (sin (+ i loop))
        (cos (+ (sin i) loop))
        0.0 0.0)))

(defun-g vert ((position :vec4) &uniform (i :int) (loop :float))
  (let ((pos (v! (* (s~ position :xyz) 0.3) 1.0)))
    (+ pos (calc-offset (+ (float i)) loop))))

(defun-g frag (&uniform (loop :float))
  (v! (cos loop) (sin loop) 0.4 1.0))

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (evt.sdl:pump-events)
  (update-swank)
  (setf *loop* (+ 0.04 *loop*))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (ttm:update)
  (loop :for i :below 37 :do
     (let ((i (/ i 2.0)))
       (gmap #'prog-1 *vertex-stream* :i i :loop *loop*)))
  (gl:flush)
  (cgl:update-display))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (apply #'gl:viewport 0 0 cgl:+default-resolution+)
    (setf *vertex-stream* (make-buffer-stream
                           (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                            (v! -0.2  -0.2  0.0  1.0)
                                            (v!  0.2  -0.2  0.0  1.0))
                                      :element-type :vec4
                                      :dimensions 3)
                           :retain-arrays t))
    (loop :while running :do (continuable (step-demo))))
  (defun stop-demo () (setf running nil)))

(evt:observe (cepl.events.sdl:|sys|)
  (when (typep e 'cepl.events.sdl:will-quit) (stop-demo)))
