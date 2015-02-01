(in-package :cgl)

(cells:defobserver gl-initialized ((context gl-context) new)
  (when new (init-data)))

(defvar *quad* nil)
(defparameter *quad-stream* nil)

(defun init-data ()
  (unless *quad*
    (setf *quad* (make-gpu-array (list (v!  0.0   0.0)
                                       (v!  1.0   0.0)
                                       (v!  1.0   1.0)
                                       (v!  1.0   1.0)
                                       (v!  0.0   1.0)
                                       (v!  0.0   0.0))
                                 :element-type :vec2
                                 :dimensions 6))
    (setf *quad-stream* (make-vertex-stream *quad*))))
