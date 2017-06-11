;;; cepl.documentation.asd

(asdf:defsystem #:cepl.documentation
  :description "Generate docs for CEPL"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:cepl #:staple)
  :components ((:file "docs/api/package")
               (:file "docs/api/gen")))
