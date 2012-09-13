;;;; cepl.asd

(asdf:defsystem #:cepl
  :serial t
  :depends-on (#:cl-opengl
	       #:lispbuilder-sdl
	       #:glop
	       #:cl-utilities)
  :components ((:file "package")
	       (:file "base-maths")
	       (:file "base-macros")
	       (:file "base-sdl")
	       (:file "cepl-utils")
	       (:file "time/base-time")
	       (:file "cepl-gl")
	       (:file "cepl-gl-data-abstraction")
	       (:file "vectors/base-vectors")
	       (:file "vectors/vector2")
	       (:file "vectors/vector3")
	       (:file "vectors/vector4")
	       (:file "vectors/vectors")
	       (:file "matrices/matrix3")
	       (:file "matrices/matrix4")
	       (:file "matrices/matrices")
	       (:file "cepl-camera")
	       (:file "model-parsers/parse-obj")
	       (:file "cepl")))
