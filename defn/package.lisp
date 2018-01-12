(uiop:define-package #:cepl.defn
    (:use #:cl #:alexandria)
  (:export :defn
           :defn-inline
           :defn-inlinable
           :parse-body+
           :locally+
           :defun+
           :defmethod+
           :define-defn-declaration))
