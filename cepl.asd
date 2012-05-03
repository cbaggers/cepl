;;;; cepl.asd

(asdf:defsystem #:cepl
  :serial t
  :depends-on (#:cl-opengl
               #:cl-glu
               #:cl-glut)
  :components ((:file "package")
               (:file "cepl")))

