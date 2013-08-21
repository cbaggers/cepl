(in-package :cgl)

(defun cls ()
  (clear :color-buffer-bit)
  (gl:flush)
  (sdl:update-display))
