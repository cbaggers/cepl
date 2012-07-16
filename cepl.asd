;;;; cepl.asd

(asdf:defsystem #:cepl
  :serial t
  :depends-on (#:cl-opengl
               #:cl-glu
               #:cl-glut
	       #:lispbuilder-sdl)
  :components ((:file "package")
	       (:file "base-maths")
	       (:file "base-macros")
	       (:file "cepl-utils")
	       (:file "math-macros")
	       (:file "cepl-gl")
	       (:file "vector2")
	       (:file "vector3")
	       (:file "vector4")
	       (:file "matrix3")
	       (:file "matrix4")
	       (:file "cepl")
	       (:file "defunct")))

