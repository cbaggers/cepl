(in-package :cepl)

(defun-g aest ((x :int))
  x)

(defun-g aest-2 ((y :int))
  (aest y))

(defun-g aest-3 ((z :int))
  (aest-2 z))
