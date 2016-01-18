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
               #:named-readtables
               #:trivial-garbage
               #:defstruct-plus-methods
	       #:fn
	       #:skitter)
  :components ((:file "core/package")
               (:file "core/errors")
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
               (:file "core/maths/projection")

               (:file "core/events/event-nodes")
               (:file "core/events/event-propagation")

               (:file "core/jungl/errors")
               (:file "core/jungl/cl-opengl-replacements")
               (:file "core/jungl/viewport")
               (:file "core/jungl/context-classes")
               (:file "core/jungl/context")
               (:file "core/jungl/generics")
               (:file "core/jungl/pixel-format")
               (:file "core/jungl/cffi-extra-primitive-types")
               (:file "core/jungl/gl-extras")
               (:file "core/jungl/c-arrays")
               (:file "core/jungl/structs")
               (:file "core/jungl/predefined-structs")
               (:file "core/jungl/equivalent-types")
               (:file "core/jungl/buffers")
               (:file "core/jungl/buffer-gpu-arrays")
               (:file "core/jungl/vaos")
               (:file "core/jungl/vertex-streams")
               (:file "core/jungl/uniforms")
               (:file "core/jungl/map-g-constant")
	       (:file "core/jungl/compile-passes")
               (:file "core/jungl/gpu-macros")
               (:file "core/jungl/gpu-pipeline-base")
               (:file "core/jungl/gpu-pipeline-validation")
               (:file "core/jungl/gpu-functions")
               (:file "core/jungl/gpu-shader-pipeline")
               (:file "core/jungl/gpu-compose-pipeline")
               (:file "core/jungl/ubo")
               (:file "core/jungl/textures")
               (:file "core/jungl/samplers")
               (:file "core/jungl/map-g")
               (:file "core/jungl/fbo")
               (:file "core/jungl/blending-modes")
               (:file "core/jungl/default-data")
               (:file "core/jungl/misc")
               (:file "core/camera/camera")

               (:file "core/space/space")
	       (:file "core/space/space-errors")
	       (:file "core/space/space-walking")
	       (:file "core/space/space-transforms")
	       ;; (:file "core/space/predefined-spaces")
	       (:file "core/space/pos")
	       (:file "core/space/gpu")

               (:file "core/live/live-macros")
               (:file "core/live/bootstrapping")
               (:file "core/time/time")
               (:file "core/primitives/primitives")
               (:file "core/repl")
               (:file "examples/examples-data")))
