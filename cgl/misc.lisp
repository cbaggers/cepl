(in-package :cgl)

(defun update-display ()
  (sdl2::sdl-gl-swap-window *gl-window*))
