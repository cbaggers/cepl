(in-package :spaces)

;;----------------------------------------------------------------------

(defstruct (space-event (:include cpl-event))
  (flush nil
	 :type boolean)
  (matrix-4 (m4:identity-matrix4)
	    :type (simple-array single-float (16))))

(defun %uid (space) (evt::event-node-uid space))

;;----------------------------------------------------------------------

(evt::def-event-node-type space
  (transform (m4:identity-matrix4)
	     :type (simple-array single-float (16)))
  (has-propagated nil :type boolean)
  (cpu t :type boolean))

;;----------------------------------------------------------------------

(defun parent-space (space)
  (first (evt::event-node-subscribers space)))

(defun space-inverse-transform (space)
  nil)

;;----------------------------------------------------------------------
