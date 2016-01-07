(in-package :cepl)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g pos-col ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g vert ((vert pos-col) &uniform (s space-g) (w space-g))
  (in s
    (in s
      (in w
	(p! (v! 0 0 0 0))))
    0)
  (values (v! (pos vert) 1.0)
          (col vert)))

(defun-g frag ((color :vec4) &uniform (s space-g) (w space-g) (loop :float))
  (in s
    (in w (p! color))
    0)
  (+ color (* (v! (sin loop) (sin loop) (sin loop) 0) 0.5)))

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defvar *l* 0.0)
(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'prog-1 *stream* :s 1 :w 2 :loop (incf *l* 0.05))
  (update-display))

(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
                                      (list (v!    0   0.5 0) (v! 1 0 0 1))
                                      (list (v! -0.5 -0.36 0) (v! 0 0 1 1)))
                                :element-type 'pos-col)
        *stream* (make-buffer-stream *array*))
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))

(evt:def-named-event-node sys-listener (e evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))
