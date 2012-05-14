;;;; cepl.asd

(asdf:defsystem #:cepl
  :serial t
  :depends-on (#:cl-opengl
               #:cl-glu
               #:cl-glut)
  :components ((:file "package")
	       (:file "base")
	       (:file "vector3")
	       (:file "matrix3")
	       (:file "cepl")))

