(in-package :cepl)

(defstruct vspace
  (node (error "node must be specified at construction of space")
        :type node)
  (parent nil :type (or null vspace))
  (cache (make-vspace-cache) :type vspace-cache))


(defstruct vspace-cache
  (transform (m4:identity-matrix4) :type (simple-array single-float (16))))


(defun get-transform (space)
  (labels ((inner (space accum)
             (if space
                 (inner (vspace-parent space)
                        (m4:m* (node-transform (vspace-node space)) accum))
                 accum)))
    (inner (vspace-parent space) (node-transform (vspace-node space)))))


(defvar *default-vspace*
  (make-vspace :node (make-node)))
