;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; cepl.asd

(asdf:defsystem #:cepl
  :serial t
  :depends-on (#:cl-opengl
               #:lbm-sdl
               #:varjo
               #:cl-utilities
               #:cl-ppcre
               #:symbol-munger
               #:classimp
               #:temporary-file)
  :components ((:file "package")
               (:file "base-maths")
               (:file "base-macros")
               ;; (:file "base-lispbuilder")
               (:file "cepl-utils")
               (:file "time/base-time")
               (:file "cepl-gl-types")
               (:file "cepl-gl")
               (:file "vectors/base-vectors")
               (:file "vectors/vector2")
               (:file "vectors/vector3")
               (:file "vectors/vector4")
               (:file "vectors/vectors")
               (:file "matrices/base-matrices")
               (:file "matrices/matrix3")
               (:file "matrices/matrix4")
               (:file "matrices/matrices")
               (:file "quaternions")
               (:file "cepl-camera")
               (:file "model-parsers/parse-obj")
               (:file "model-parsers/parse-lisp")
               (:file "model-parsers/classimp-cepl-extras")
               (:file "cepl")))
