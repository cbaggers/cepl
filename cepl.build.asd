;;;; cepl.build.asd

(asdf:defsystem #:cepl.build
  :description "Common package and system behind CEPL's profiling tools"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "build/package")
               (:file "build/build")
               (:file "defn/package")
               (:file "defn/defn")))
