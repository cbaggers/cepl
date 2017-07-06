;;;; package.lisp

(uiop:define-package #:cepl.build
    (:use #:cl)
  (:export :profile :profile-block :release-unwind-protect))

(in-package #:cepl.build)

(defvar *cepl-release-mode* nil)

(defmacro release-unwind-protect (protected &body cleanup)
  (if *cepl-release-mode*
      `(multiple-value-prog1 protected
         ,@cleanup)
      `(unwind-protect ,protected ,@cleanup)))

(defmacro profile-block (name &body body)
  (declare (ignore name))
  `(progn ,@body))
