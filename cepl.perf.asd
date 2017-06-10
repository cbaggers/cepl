;;;; cepl.perf.asd

(asdf:defsystem #:cepl.perf
  :description "Performance tools for CEPL & CEPL based projects"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:rtg-math
               :cepl.perf.core
               :bordeaux-threads
               :chanl)
  :components ((:file "perf/package")
               (:file "perf/impl")
               (:file "perf/tools")))
