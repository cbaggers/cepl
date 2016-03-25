;;;; cepl.asd

#+(and darwin sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:cepl
  :description "Fast lispy way to work with OpenGL"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cl-autowrap
               #:cl-plus-c
               #:cl-fad
               #:cl-opengl
               #:cl-ppcre
               #:rtg-math
               #:varjo
               #:named-readtables
               #:fn
               #:documentation-utils)
  :components ((:file "package")
               (:file "host/api")
               (:file "core/utils")
               (:file "project")
               (:file "core/lifecycle")
               (:file "core/continuable")
               (:file "core/generics")
	       (:file "core/types/cepl-types")
               (:file "core/internals")
               (:file "core/types/types")
               (:file "core/spaces/constants")
               (:file "core/spaces/nht-routes")
               (:file "core/spaces/space")
               (:file "core/spaces/predefined-spaces")
               (:file "core/context/render-state")
               (:file "core/errors")
               (:file "core/viewports/viewport")
               (:file "core/context/context-classes")
               (:file "core/context/context")
               (:file "core/types/image-format")
               (:file "core/types/pixel-format")
               (:file "core/types/cffi-extra-primitive-types")
               (:file "core/c-arrays/c-arrays")
               (:file "core/types/structs")
               (:file "core/gpu-buffers/gpu-buffers")
               (:file "core/gpu-arrays/buffer-backed")
               (:file "core/vaos/vaos")
               (:file "core/streams/buffer-streams")
               (:file "core/pipelines/uniforms")
               (:file "core/pipelines/map-g-constant")
               (:file "core/pipelines/compile-passes")
               (:file "core/pipelines/gpu-macros")
               (:file "core/pipelines/gpu-pipeline-base")
               (:file "core/pipelines/gpu-pipeline-validation")
               (:file "core/pipelines/gpu-functions")
               (:file "core/pipelines/gpu-glsl-stages")
               (:file "core/pipelines/gpu-shader-pipeline")
               (:file "core/pipelines/gpu-compose-pipeline")
	       (:file "core/textures/def")
	       (:file "core/samplers/def")
               (:file "core/samplers/samplers")
               (:file "core/textures/textures")
               (:file "core/pipelines/map-g")
               (:file "core/gpu-arrays/texture-backed")
	       (:file "core/gpu-arrays/with-and-push")
               (:file "core/fbos/fbo")
	       (:file "core/ubos/ubo")
               (:file "core/blending/blending-modes")
	       (:file "core/fbos/fbo-blending")
               (:file "core/spaces/space-errors")
               (:file "core/spaces/space-walking")
               (:file "core/spaces/space-transforms")
               (:file "core/spaces/pos")
               (:file "core/spaces/gpu")
               (:file "core/types/predefined/gpu-structs")
	       (:file "core/context/make")
               (:file "core/repl")
	       ;;---
               (:file "docs/api/package")
	       (:file "core/c-arrays/docs")
	       (:file "core/gpu-arrays/docs")
	       (:file "core/streams/docs")
	       (:file "core/viewports/docs")
	       (:file "core/vaos/docs")))
