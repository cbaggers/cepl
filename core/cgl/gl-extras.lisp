(in-package :cgl)

(defun tex-storage-1d (target levels internal-format width)
  (%gl:tex-storage-1d target levels (gl::internal-format->int internal-format)
                      width))

(defun tex-storage-2d (target levels internal-format width height)
  (%gl:tex-storage-2d target levels (gl::internal-format->int internal-format)
                      width height))

(defun tex-storage-3d (target levels internal-format width height depth)
  (%gl:tex-storage-3d target levels (gl::internal-format->int internal-format)
                      width height depth))

(defun active-texture-num (num)
  (gl:active-texture (+ #x84C0 num)))

(defun color-attachment-enum (attachment-num)
  (+ attachment-num #.(cffi:foreign-enum-value '%gl:enum :color-attachment0)))
