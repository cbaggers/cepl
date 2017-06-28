(in-package :cepl.gpu-arrays.texture-backed)

(defun2 gpu-array-texture (gpu-array)
  (%cepl.types::gpu-array-t-texture gpu-array))

(defun2 gpu-array-texture-type (gpu-array)
  (%cepl.types::gpu-array-t-texture-type gpu-array))

(defun2 gpu-array-level-num (gpu-array)
  (%cepl.types::gpu-array-t-level-num gpu-array))

(defun2 gpu-array-layer-num (gpu-array)
  (%cepl.types::gpu-array-t-layer-num gpu-array))

(defun2 gpu-array-face-num (gpu-array)
  (%cepl.types::gpu-array-t-face-num gpu-array))

(defmethod resolution ((gpu-array gpu-array-t))
  (let ((dim (gpu-array-dimensions gpu-array)))
    (make-array (length dim) :element-type 'single-float
                :initial-contents (mapcar (lambda (i) (coerce i 'single-float))
                                          dim))))
