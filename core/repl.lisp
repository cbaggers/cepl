(in-package :cepl)

(defun repl (&optional (width 320) (height 240))
  (init width height "CEPL REPL" t)
  (format t "~%-----------------~%    CEPL-REPL    ~%-----------------~%"))

(defun init (&optional (width 320) (height 240) (title "CEPL") (resizable t))
  (jungl:make-context :width width :height height :resizable resizable
		      :title title))

(defun quit () (cepl.host:shutdown))
