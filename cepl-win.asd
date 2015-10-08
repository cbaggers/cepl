;; THIS IS UNTESTED IT PROBABLY DOESNT WORK

;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; cepl.asd

(asdf:defsystem #:cepl-win
  :serial t
  :depends-on (#:cepl-backend-sdl)
  :components ((:file "platform-specific/package")
               (:file "platform-specific/win")))
