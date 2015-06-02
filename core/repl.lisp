;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cepl)

(defun repl (&optional (width 320) (height 240) (backend :sdl))
  (cgl:make-context backend :width width :height height :resizable t
                    :title "CEPL REPL")
  (format t "~%-----------------~%    CEPL-REPL    ~%-----------------~%"))

(defun quit () (cepl-backend:shutdown cepl-backend:*backend*))
