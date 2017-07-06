;;;; package.lisp

(uiop:define-package #:cepl.perf
    (:use #:cl #:cepl.build #:%rtg-math)
  (:export :profile
           :load-with-instrumentation
           :start-profiling
           :stop-profiling
           :analyze
           :overview))
