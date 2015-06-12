;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; cepl.asd

#+(and darwin sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:cepl
  :serial t
  :depends-on (#:cl-autowrap
               #:cl-plus-c
               #:cl-fad
               #:cl-opengl
               #:swank
               #:varjo
               #:temporal-functions
               #:cl-utilities
               #:cl-ppcre
               #:symbol-munger
               #:cells
               #:named-readtables)
  :components ((:file "core/package")
               (:file "core/backend")
               (:file "core/utils")
               (:file "core/static")
               (:file "core/generics")
               (:file "core/maths/base-maths")
               (:file "core/maths/maths")
               (:file "core/maths/vectors/base-vectors")
               (:file "core/maths/vectors/vector2")
               (:file "core/maths/vectors/vector3")
               (:file "core/maths/vectors/vector4")
               (:file "core/maths/vectors/vectors")
               (:file "core/maths/matrices/base-matrices")
               (:file "core/maths/matrices/matrix3")
               (:file "core/maths/matrices/matrix4")
               (:file "core/maths/matrices/matrices")
               (:file "core/maths/quaternions")
               (:file "core/cgl/cl-opengl-replacements")
               (:file "core/cgl/viewport")
               (:file "core/cgl/context-classes")
               (:file "core/cgl/context")
               (:file "core/cgl/generics")
               (:file "core/cgl/pixel-format")
               (:file "core/cgl/cffi-extra-primitive-types")
               (:file "core/cgl/gl-extras")
               (:file "core/cgl/c-arrays")
               (:file "core/cgl/structs")
               (:file "core/cgl/predefined-structs")
               (:file "core/cgl/equivalent-types")
               (:file "core/cgl/buffers")
               (:file "core/cgl/buffer-gpu-arrays")
               (:file "core/cgl/vaos")
               (:file "core/cgl/vertex-streams")
               (:file "core/cgl/uniforms")
               (:file "core/cgl/gpu-macros")
               (:file "core/cgl/gpu-pipeline-base")
               (:file "core/cgl/gpu-functions")
               (:file "core/cgl/gpu-shader-pipeline")
               (:file "core/cgl/gpu-compose-pipeline")
               (:file "core/cgl/ubo")
               (:file "core/cgl/misc")
               (:file "core/cgl/textures")
               (:file "core/cgl/samplers")
               (:file "core/cgl/gmap")
               (:file "core/cgl/fbo")
               (:file "core/cgl/blending-modes")
               (:file "core/cgl/default-data")
               (:file "core/camera/camera")
               ;; (:file "core/space/base-space")
               ;; (:file "core/space/space")
               (:file "core/events/base-events")
               (:file "core/events/event-classes")
               (:file "core/events/events")
               (:file "core/live/live-macros")
               (:file "core/live/bootstrapping")
               (:file "core/time/time")
               (:file "core/primitives/primitives")
               (:file "core/repl")
               (:file "examples/examples-data")))
