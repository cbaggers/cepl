(in-package :cepl)

(defun-g test ((x :int))
  x)

(defun-g test-2 ((y :int))
  (test y))

(defun-g test-3 ((z :int))
  (test-2 z))
