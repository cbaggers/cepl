;;; cepl.asd

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:cepl
  :description "Fast lispy way to work with OpenGL"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cffi
               #:cl-autowrap
               #:cl-fad
               #:cl-opengl
               #:cl-plus-c
               #:cl-ppcre
               #:documentation-utils
               #:fn
               #:ieee-floats
               #:named-readtables
               #:rtg-math
               #:varjo
               #:closer-mop)
  :components ((:file "package")
               (:file "host/api")
               (:file "core/ffi")
               (:file "core/utils")
               (:file "project")
               (:file "core/context/vars")
               (:file "core/lifecycle")
               (:file "core/measurements/measurements")
               (:file "core/memory/memory")
               (:file "core/types/cepl-types")
               (:file "core/context/gl-context")
               (:file "core/context/cepl-context")
               (:file "core/context/delayed-resource-init")
               (:file "core/context/capabilities")
               (:file "core/context/version")
               (:file "core/types/initalized-p")
               (:file "core/internals")
               (:file "core/types/types")
               (:file "core/spaces/constants")
               (:file "core/spaces/nht-routes")
               (:file "core/spaces/space")
               (:file "core/spaces/predefined-spaces")
               (:file "core/spaces/pos")
               (:file "core/context/render-state")
               (:file "core/errors")
               (:file "core/viewports/viewport")
               (:file "core/types/image-format")
               (:file "core/types/pixel-format")
               (:file "core/types/cffi-extra-primitive-types")
               (:file "core/types/cffi-helpers")
               (:file "core/c-arrays/def")
               (:file "core/c-arrays/aref-c")
               (:file "core/c-arrays/populate")
               (:file "core/c-arrays/make")
               (:file "core/c-arrays/map")
               (:file "core/c-arrays/rest")
               (:file "core/types/structs")
               (:file "core/gpu-buffers/gpu-buffers")
               (:file "core/gpu-arrays/buffer-backed")
               (:file "core/vaos/vaos")
               (:file "core/streams/buffer-streams")
               (:file "core/pipelines/generics")
               (:file "core/pipelines/uniforms")
               (:file "core/pipelines/map-g-constant")
               (:file "core/pipelines/gpu-macros")
               (:file "core/pipelines/gpu-pipeline-base")
               (:file "core/pipelines/pipeline-validation")
               (:file "core/pipelines/gpu-functions")
               (:file "core/pipelines/glsl-stages")
               (:file "core/pipelines/uniform-assigners-generation")
               (:file "core/pipelines/defpipeline")
               (:file "core/pipelines/gpu-lambda")
               (:file "core/pipelines/bake")
               (:file "core/textures/def")
               (:file "core/samplers/samplers")
               (:file "core/textures/textures")
               (:file "core/textures/texture-samplers")
               (:file "core/pipelines/map-g")
               (:file "core/gpu-arrays/texture-backed")
               (:file "core/gpu-arrays/with-and-push")
               (:file "core/fbos/fbo")
               (:file "core/ubos/ubo")
               (:file "core/blending/blending")
               (:file "core/spaces/space-errors")
               (:file "core/spaces/space-walking")
               (:file "core/spaces/space-transforms")
               (:file "core/spaces/pos-funcs")
               (:file "core/spaces/gpu")
               (:file "core/types/predefined/gpu-structs")
               (:file "core/context/make")
               (:file "core/repl")
               ;;---
               (:file "docs/api/package")
               (:file "core/types/docs-image-formats")
               (:file "core/types/docs-pixel-formats")
               (:file "core/types/predefined/docs")
               (:file "core/types/docs")
               (:file "core/measurements/docs")
               (:file "core/memory/docs")
               (:file "core/c-arrays/docs")
               (:file "core/gpu-buffers/docs")
               (:file "core/gpu-arrays/docs")
               (:file "core/streams/docs")
               (:file "core/viewports/docs")
               (:file "core/textures/docs")
               (:file "core/samplers/docs")
               (:file "core/fbos/docs")
               (:file "core/blending/docs")
               (:file "core/ubos/docs")
               (:file "core/vaos/docs")
               (:file "core/spaces/docs")
               (:file "core/pipelines/docs")
               (:file "core/misc/misc")))
