(in-package :cgl)

(cells:defobserver gl-initialized ((context gl-context) new)
  (when new (init-data)))

(defvar *quad* nil)
(defvar *quad-stream* nil)

(defun init-data ()
  (unless *quad*
    (setf *quad* (make-gpu-array
                  (list (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                        (list (v! -1.0  -1.0 0 0) (v!  0.0   0.0))
                        (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                        (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                        (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                        (list (v!  1.0   1.0 0 0) (v!  1.0   1.0)))
                  :element-type 'g-pt
                  :dimensions 6))
    (setf *quad-stream* (make-buffer-stream *quad* :retain-arrays t))))
