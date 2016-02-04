;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

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
