;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is a dumping ground for homless functions
;; If anything is here currently then it needs to be rehomed or
;; the essence of the functionality decided, generalised and 
;; rehomed.

(in-package :cepl)

(defun repl ()
  (in-package :cepl)
  (if (sdl:init-sdl)
      (sdl:window 640 480 :icon-caption "CEPL REPL" :title-caption "CEPL REPL")
      (error "Failed to initialise SDL"))
  (format t "-----------------~%    CEPL-REPL    ~%-----------------"))

