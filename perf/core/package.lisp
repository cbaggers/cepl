;;;; package.lisp

(uiop:define-package #:cepl.perf.core
    (:use #:cl)
  (:export :profile :profile-block))

(in-package #:cepl.perf.core)

(defmacro profile-block (name &body body)
  (declare (ignore name))
  `(progn ,@body))
