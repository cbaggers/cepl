(in-package :cepl)

(defparameter *default-space* nil)

(defstruct graphics-node
  (position (v! 0 0 0) :type (simple-array single-float (3)))
  (orientation (q:identity-quat) :type (simple-array single-float (4)))
  (space *default-space* :type space))
