(in-package :cgl)

(defun cls ()
  (clear :color-buffer-bit)
  (gl:flush)
  (update-display))

(defun update-display ()
  (sdl2::sdl-gl-swap-window *gl-window*))
