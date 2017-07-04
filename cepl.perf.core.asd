;;;; cepl.perf.core.asd

(asdf:defsystem #:cepl.perf.core
  :description "Common package and system behind CEPL's profiling tools"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :components ((:file "perf/core/package")))
