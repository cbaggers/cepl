(in-package :cepl)

(defclass node ()
  ((position)
   (orientation)
   (parent-space)
   (space)))

(defun make-node (space)
  (make-instance 'node :position (v! 0 0 0)
                 :orientation (q:make-quat 0 0 0 0)
                 :parent-space space))

(defparameter test-node (make-node space:WORLD))

(defparameter new-space 
  (make-space-m4 space:WORLD (m4:translation (v! 10 10 10))))
