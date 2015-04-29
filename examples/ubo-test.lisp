(in-package :cepl)

(defparameter *array* nil)
(defparameter *ubo* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g test ()
  (scale :float :accessor scale))

(defun-g vert ((vert g-pc) &uniform (hmm test :ubo))
  (values (v! (* (pos vert) (scale hmm)) 1.0)
          (:smooth (col vert))))

(defun-g frag ((color :vec4))
  color)

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit)
  (map-g #'prog-1 *stream*) ;; :hmm *ubo*
  (cgl:update-display))

(defun run-demo ()
  (setf *running* t
        *ubo* (make-ubo (make-gpu-array '((1.2)) :element-type 'test))
        *array* (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
                                      (list (v!    0   0.5 0) (v! 1 0 0 1))
                                      (list (v! -0.5 -0.36 0) (v! 0 0 1 1)))
                                :element-type 'g-pc)
   *stream* (make-buffer-stream *array*))
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-demo ()
  (setf *running* nil))

(evt:observe (cepl.events.sdl:|sys|)
  (when (typep e 'cepl.events.sdl:will-quit) (stop-demo)))
