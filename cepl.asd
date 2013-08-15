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
               (:file "maths/base-maths")
               (:file "base-macros")
               ;; (:file "base-lispbuilder")
               (:file "cepl-utils")
               (:file "time/base-time")
               (:file "cgl/generics")
               (:file "cgl/pixel-format")
               (:file "cgl/cffi-extra-primitive-types")
               (:file "cgl/gl-extras")
               (:file "cgl/c-arrays")
               (:file "cgl/structs")
               (:file "cgl/buffers")
               (:file "cgl/buffer-gpu-arrays")
               (:file "cgl/vaos")
               (:file "cgl/gpu-streams")
               (:file "cgl/shaders")
               (:file "cgl/misc")
               (:file "cgl/textures")
               (:file "maths/vectors/base-vectors")
               (:file "maths/vectors/vector2")
               (:file "maths/vectors/vector3")
               (:file "maths/vectors/vector4")
               (:file "maths/vectors/vectors")
               (:file "maths/matrices/base-matrices")
               (:file "maths/matrices/matrix3")
               (:file "maths/matrices/matrix4")
               (:file "maths/matrices/matrices")
               (:file "maths/quaternions")
               (:file "cepl-camera")
               (:file "model-parsers/parse-obj")
               (:file "model-parsers/parse-lisp")
               (:file "model-parsers/classimp-cepl-extras")
               (:file "model-parsers/3dstub")
               (:file "cepl")))
