(in-package :jungl)

(defun get-uniform-block-index (program name)
  (with-foreign-string (s name)
    (%gl:get-uniform-block-index program s)))

(defun gen-buffer ()
  (car (gl:gen-buffers 1)))
