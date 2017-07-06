;;;; package.lisp

(uiop:define-package #:cepl.build
    (:use #:cl)
  (:export :profile :profile-block :release-unwind-protect
           :load-in-release-mode))
