(in-package :cepl)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g pos-col ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (:smooth (col vert))))

(defun-g frag ((color :vec4))
  color)

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit)
  (gmap #'prog-1 *stream*)
  (cgl:update-display))

(defun run-demo ()
  (setf *running* t
        *array* (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
                                      (list (v!    0   0.5 0) (v! 1 0 0 1))
                                      (list (v! -0.5 -0.36 0) (v! 0 0 1 1)))
                                :element-type 'pos-col)
   *stream* (make-vertex-stream *array*))
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-demo ()
  (setf *running* nil))

(evt:observe (cepl.events.sdl:|sys|)
  (when (typep e 'cepl.events.sdl:will-quit) (stop-demo)))
