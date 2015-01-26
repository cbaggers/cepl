;; This is simply to get a colored triangle up on the screen

(defparameter *array* nil)
(defparameter *stream* nil)

(defpipeline prog-1 ((vert g-pc))
  (:vertex (setf gl-position (v! (cgl:pos vert) 1.0))
           (out (the-color :smooth) (cgl:col vert)))
  (:fragment (out outputColor the-color)))

(defun step-demo ()
  (evt.sdl:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit)
  (prog-1 *stream*)
  (gl:flush)
  (cgl:update-display))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (gl:front-face :ccw)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (gl:viewport 0 0 640 480)
    (setf *array* (make-gpu-array
                   (list (list (v!  0.5 -0.366 0.0) (v! 0.0 1.0 0.0 1.0))
                         (list (v!  0.0    0.5 0.0) (v! 1.0 0.0 0.0 1.0))
                         (list (v! -0.5 -0.366 0.0) (v! 0.0 0.0 1.0 1.0)))
                   :element-type 'g-pc))
    (setf *stream* (make-vertex-stream *array*))
    (loop :while running :do (continuable (step-demo))))
  (defun stop-demo () (setf running nil))
  (evt:observe (evt.sdl::*sys*)
    (setf running (typep e 'evt.sdl:will-quit))))
