(in-package #:tools)

(defmacro mcol (form n)
  `(loop for i below ,n collect ,form))

(defun n-col (function n)
  (loop for i below n collect (funcall function)))
