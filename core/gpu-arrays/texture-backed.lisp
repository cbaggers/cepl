(in-package :cepl.gpu-arrays.texture-backed)

(defun gpu-array-texture (gpu-array)
  (%cepl.types::gpu-array-t-texture gpu-array))

(defun gpu-array-texture-type (gpu-array)
  (%cepl.types::gpu-array-t-texture-type gpu-array))

(defun gpu-array-level-num (gpu-array)
  (%cepl.types::gpu-array-t-level-num gpu-array))

(defun gpu-array-layer-num (gpu-array)
  (%cepl.types::gpu-array-t-layer-num gpu-array))

(defun gpu-array-face-num (gpu-array)
  (%cepl.types::gpu-array-t-face-num gpu-array))
