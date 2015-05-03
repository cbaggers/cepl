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
  :depends-on (#:cl-fad
               #:cl-opengl
               #:cl-devil
               #:swank
               #:sdl2
               #:varjo
               #:temporal-functions
               #:cl-utilities
               #:cl-ppcre
               #:symbol-munger
               #:cells
               #:named-readtables
               #:classimp
               #:fn_
               #:quickproject)
  :components ((:file "package")
               (:file "utils")
               (:file "generics")
               (:file "base-macros")
               (:file "maths/base-maths")
               (:file "maths/maths")
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
               (:file "cgl/cl-opengl-replacements")
               (:file "cgl/viewport")
               (:file "cgl/context")
               (:file "cgl/generics")
               (:file "cgl/pixel-format")
               (:file "cgl/cffi-extra-primitive-types")
               (:file "cgl/gl-extras")
               (:file "cgl/c-arrays")
               (:file "cgl/structs")
               (:file "cgl/predefined-structs")
               (:file "cgl/equivalent-types")
               (:file "cgl/buffers")
               (:file "cgl/buffer-gpu-arrays")
               (:file "cgl/vaos")
               (:file "cgl/vertex-streams")
               (:file "cgl/uniforms")
               (:file "cgl/gpu-macros")
               (:file "cgl/gpu-pipeline-base")
               (:file "cgl/gpu-functions")
               (:file "cgl/gpu-shader-pipeline")
               (:file "cgl/gpu-compose-pipeline")
               (:file "cgl/ubo")
               (:file "cgl/misc")
               (:file "cgl/textures")
               (:file "cgl/samplers")
               (:file "cgl/gmap")
               (:file "cgl/fbo")
               (:file "cgl/default-data")
               (:file "camera/camera")
               ;; (:file "space/base-space")
               ;; (:file "space/space")
               (:file "images/devil-helper")
               (:file "meshes-and-models/primitives")
               (:file "meshes-and-models/mesh")
               (:file "meshes-and-models/classimp-helpers")
               (:file "sdl-extras")
               (:file "events/base-events")
               (:file "events/events")
               (:file "events/sdl-event-sources")
               (:file "events/event.accum")
               (:file "live/live-macros")
               (:file "live/bootstrapping")
               (:file "time/time")
               (:file "projects/quickproject")
               (:file "cepl")))
