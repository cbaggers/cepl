(in-package :cepl.textures)

;; This is broken we need to be able to determine the type of col
;; from the pixel-format
;;
(defun clear-tex-ref (tex &key (mipmap-level 0) (layer 0) (cube-face 0))
  (let* ((arr (texref tex :mipmap-level mipmap-level
                      :layer layer
                      :cube-face cube-face))
         (format (element-type arr))
         (type (pixel-format-type (image-format->pixel-format format))))
    (with-foreign-object (col :float)
      (setf (cffi:mem-aref col :float) 0s0)
      (%gl:clear-tex-image (texture-id tex)
                           mipmap-level
                           format
                           type
                           col))))
