(in-package :cgl)

(defun get-uniform-block-index (program name)
  (with-foreign-string (s name)
    (%gl:get-uniform-block-index program s)))
