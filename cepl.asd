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
	       #:cl-ppcre
	       #:rtg-math
               #:varjo
               #:named-readtables
	       #:fn
	       #:swank)
  :components ((:file "package")
               (:file "host/api")
	       (:file "host/step")
               (:file "core/utils")
	       (:file "project")
	       (:file "core/lifecycle")
	       (:file "core/continuable")
               (:file "core/generics")

	       (:file "core/jungl/types")
	       (:file "core/jungl/space/nht-routes")
	       (:file "core/jungl/space/space")
	       (:file "core/jungl/space/predefined-spaces")

	       (:file "core/jungl/global-vars")
               (:file "core/jungl/generics")
	       (:file "core/jungl/render-state")
               (:file "core/jungl/errors")
               (:file "core/jungl/cl-opengl-replacements")
               (:file "core/jungl/viewport")
               (:file "core/jungl/context-classes")
               (:file "core/jungl/context")
               (:file "core/jungl/helpers")
               (:file "core/jungl/pixel-format")
               (:file "core/jungl/cffi-extra-primitive-types")
               (:file "core/jungl/gl-extras")
               (:file "core/jungl/c-arrays")
               (:file "core/jungl/structs")
               (:file "core/jungl/buffers")
               (:file "core/jungl/gpu-arrays-buffer-backed")
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

	       (:file "core/jungl/space/space-errors")
	       (:file "core/jungl/space/space-walking")
	       (:file "core/jungl/space/space-transforms")

	       (:file "core/jungl/space/pos")
	       (:file "core/jungl/space/gpu")

	       (:file "core/predefined/gpu-structs")
               (:file "core/repl")))
