;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; cepl.asd

(asdf:defsystem #:cepl-model-helper
  :serial t
  :depends-on (#:cepl #:classimp #:rtg-math)
  :components ((:file "meshes-and-models/package")
               (:file "meshes-and-models/mesh")
               (:file "meshes-and-models/classimp-helpers")))
