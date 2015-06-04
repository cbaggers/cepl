;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; cepl.asd

(asdf:defsystem #:cepl-backend-sdl
  :serial t
  :depends-on (#:cepl #:sdl2 :sb-cga)
  :components ((:file "backends/sdl/package")
               (:file "backends/sdl/sdl-event-sources")
               (:file "backends/sdl/cepl-sdl")))
