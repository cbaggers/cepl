(in-package :cepl)

(defun repl (&optional (width 320) (height 240) (backend :sdl))
  (init width height backend "CEPL REPL" t)
  (format t "~%-----------------~%    CEPL-REPL    ~%-----------------~%"))

(defun init (&optional (width 320) (height 240) (backend :sdl) (title "CEPL")
               (resizable t))
  (jungl:make-context backend :width width :height height :resizable resizable
		      :title title)
  (cepl.host:cache-step-func))

(defun quit () (cepl.host:shutdown))
