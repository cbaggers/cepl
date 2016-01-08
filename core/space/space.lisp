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
  (has-propagated nil :type boolean))

(defmethod print-object ((object space) stream)
  (format stream "#<SPACE ~s>" (%uid object)))

(defun space! (transform &optional parent-space)
  (assert (typep transform '(simple-array single-float (16))))
  (assert (or (null parent-space) (typep parent-space 'space)))
  (make-space :transform transform :subscribe-to parent-space))

;;----------------------------------------------------------------------

(defun parent-space (space)
  (first (evt::event-node-subscriptions space)))

(defun space-inverse-transform (space)
  nil)

;;----------------------------------------------------------------------
