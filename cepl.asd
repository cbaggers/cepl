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
               #:swank
               #:sdl2
               #:varjo
               #:cl-utilities
               #:cl-ppcre
               #:symbol-munger)
  :components ((:file "package")
               (:file "utils")
               (:file "base-macros")
               (:file "maths/base-maths")
               (:file "maths/maths")
               (:file "declarative-values")
               (:file "cgl/context")
               (:file "cgl/generics")
               (:file "cgl/pixel-format")
               (:file "cgl/cffi-extra-primitive-types")
               (:file "cgl/gl-extras")
               (:file "cgl/c-values")
               (:file "cgl/c-arrays")
               (:file "cgl/structs")
               (:file "cgl/buffers")
               (:file "cgl/buffer-gpu-arrays")
               (:file "cgl/vaos")
               (:file "cgl/vertex-streams")
               (:file "cgl/uniforms")
               (:file "cgl/shaders")
               (:file "cgl/misc")
               (:file "cgl/textures")
               (:file "cgl/types")
               (:file "cgl/framebuffer.lisp")
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
               (:file "meshes-and-models/primitives")
               (:file "meshes-and-models/model-parsers/parse-obj")
               (:file "meshes-and-models/model-parsers/parse-lisp")
               (:file "sdl-extras")
               (:file "cepl")))
