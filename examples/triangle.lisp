(in-package :cepl)
;; This is simply to get a colored triangle up on the screen

(defparameter *array* nil)
(defparameter *stream* nil)

(defun-g v ((vert g-pc))
  (values (v! (cgl:pos vert) 1.0)
          (:smooth (cgl:col vert))))

(defun-g f ((color :vec4))
  color)

(defpipeline prog-1 (g-> v f))

(defun step-demo ()
  (evt.sdl:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit)
  (gmap #'prog-1 *stream*)
  (gl:flush)
  (cgl:update-display))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (apply #'gl:viewport 0 0 cgl:+default-resolution+)
    (setf *array* (make-gpu-array
                   (list (list (v!  0.5 -0.366 0.0) (v! 0.0 1.0 0.0 1.0))
                         (list (v!  0.0    0.5 0.0) (v! 1.0 0.0 0.0 1.0))
                         (list (v! -0.5 -0.366 0.0) (v! 0.0 0.0 1.0 1.0)))
                   :element-type 'g-pc))
    (setf *stream* (make-vertex-stream *array*))
    (loop :while running :do (continuable (step-demo))))
  (defun stop-demo () (setf running nil))
  (evt:observe (evt.sdl::|sys|)
    (setf running (typep e 'evt.sdl:will-quit))))
