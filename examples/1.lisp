;; This is simply to get a colored triangle up on the screen

(defparameter *array* nil)
(defparameter *stream* nil)

(defglstruct vert-data
  (position :vec4 :accessor pos)
  (colour :vec4 :accessor col))

(defpipeline prog-1 ((vert vert-data))
  (:vertex (setf gl-position (pos vert))
           (out (the-color :smooth) (col vert)))
  (:fragment (let ((lerp-value (/ (y gl-frag-coord) 500.0)))
               (out outputColor (mix the-color 
                                     (v! 0.2 0.2 0.2 1.0)
                                     lerp-value)))))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (gl:viewport 0 0 640 480)
    (setf *array* (make-gpu-array 
                   (list (list (v!  0.0    0.5 0.0 1.0) (v! 1.0 0.0 0.0 1.0))
                         (list (v!  0.5 -0.366 0.0 1.0) (v! 0.0 1.0 0.0 1.0))
                         (list (v! -0.5 -0.366 0.0 1.0) (v! 0.0 0.0 1.0 1.0)))
                   :dimensions 3
                   :element-type 'vert-data))
    (setf *stream* (make-vertex-stream *array*))
    (loop :while running :do
       (case-events (event)
         (:quit () (setf running nil)))
       (update-swank)
       (continuable (gl:clear :color-buffer-bit)
                    (prog-1 *stream*)
                    (gl:flush)
                    (cgl:update-display))))
  (defun stop-demo () (setf running nil)))
