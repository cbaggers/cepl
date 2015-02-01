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
               #:temporal-functions
               #:cl-utilities
               #:cl-ppcre
               #:symbol-munger
               #:cells
               #:classimp
               #:fn_)
  :components ((:file "package")
               (:file "utils")
               (:file "base-macros")
               (:file "maths/base-maths")
               (:file "maths/maths")
               (:file "cgl/context")
               (:file "cgl/generics")
               (:file "cgl/pixel-format")
               (:file "cgl/cffi-extra-primitive-types")
               (:file "cgl/gl-extras")
               (:file "cgl/c-values")
               (:file "cgl/c-arrays")
               (:file "cgl/structs")
               (:file "cgl/buffers")
               (:file "cgl/equivalent-types")
               (:file "cgl/buffer-gpu-arrays")
               (:file "cgl/vaos")
               (:file "cgl/vertex-streams")
               (:file "cgl/uniforms")
               (:file "cgl/shaders")
               (:file "cgl/misc")
               (:file "cgl/textures")
               (:file "cgl/types")
               (:file "cgl/framebuffer")
               (:file "cgl/default-data")
               (:file "cgl/swatch")
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
               (:file "meshes-and-models/mesh")
               (:file "meshes-and-models/classimp-helpers")               
               (:file "sdl-extras")
               (:file "events/event-base")
               (:file "events/events")
               (:file "events/sdl-event-sources")
               (:file "live/bootstrapping")
               (:file "time/time")
               (:file "cepl")))
