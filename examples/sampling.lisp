(in-package :cepl)

(defparameter *tex* nil)
(defparameter *stream* nil)
(defparameter *running* nil)
(defparameter *sam* nil)

(defun-g vert ((vert g-pt))
  (values (v! (pos vert) 1.0) (tex vert)))

(defun-g frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defpipeline prog-1 () (g-> #'vert #'frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (with-sampling ((*tex* *sam*))
    (map-g #'prog-1 *stream* :tex *tex*))
  (update-display))

(defun run-loop ()
  (setf *running* t
        *stream* (make-buffer-stream
                  (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! -1 1))
                                        (list (v!    0   0.5 0) (v! 1 1))
                                        (list (v! -0.5 -0.36 0) (v! 0 -1)))
                                  :element-type 'g-pt)
                  :retain-arrays t)
        *tex* (devil-helper:load-image-to-texture
               (merge-pathnames "brick/col.png" *examples-dir*))
        *sam* (make-sampler))
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))

(evt:def-named-event-node sys-listener (e evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))
