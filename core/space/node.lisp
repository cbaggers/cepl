(in-package :cepl)

(defstruct graphics-node
  (position (v! 0 0 0 0) :type (simple-array single-float (4)))
  (space *euler-space* :type space))
