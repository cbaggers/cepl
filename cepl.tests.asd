;;; cepl.tests.asd

(asdf:defsystem #:cepl.tests
  :description "Long overdue testing of CEPL"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:cepl.sdl2
               #:cepl.camera
               #:cepl.skitter.sdl2
               #:classimp
               #:fn
               #:named-readtables
               #:cl-fad
               #:temporal-functions
               #:structy-defclass
               #:swank.live
               #:fiveam)
  :components ((:file "tests/package")
               (:file "tests/funcs")))
