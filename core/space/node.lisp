(in-package :cepl)

;; Node is the most minimal abstraction of 3d transform above matrix4
;; A slots of a basic node contain:
;; - A matrix4
;; - A flag that says whether that matrix is up to date
;; - A function that updates the matrix and returns the 'new' matrix4 that
;;   is up to date. It is not required that the matrix is newly allocated,
;;   only that the data is correct.

;;--------------------------------

(defstruct (node (:constructor %make-node) (:conc-name %node-))
  (id 0 :type fixnum)
  (transform (m4:identity-matrix4) :type (simple-array single-float (16)))
  (re-calc t :type boolean)
  (update-func #'node-transform
               :type (function (node) (simple-array single-float (16)))))

(defun make-node () (%make-node))

;;-  -  -  -  -  -  -  -

(declaim (ftype (function (node) (simple-array single-float (16)))
                node-transform))
(defun node-transform (node)
  (declare (optimize (speed 3) (debug 1) (safety 1)
                     (space 0) (compilation-speed 0)))
  (if (%node-re-calc node)
      (let ((func (%node-update-func node)))
        (declare ((function (node) (simple-array single-float (16))) func))
        (funcall func node))
      (%node-transform node)))

;;--------------------------------

(defstruct (pos-quat-node (:conc-name %pqn-) (:constructor %make-pqn)
                          (:include node))
  (pos (v! 0 0 0) :type (simple-array single-float (3)))
  (quat (q:identity-quat) :type (simple-array single-float (4))))

(defun make-pos-quat-node ()
  (%make-pqn :update-func #'%pos-quat-update))

;;-  -  -  -  -  -  -  -

(declaim (ftype (function (node) (simple-array single-float (16)))
                %pos-quat-update))
(defun %pos-quat-update (node)
  (declare (node node))
  (setf (%node-re-calc node) nil
        (%node-transform node) (m4:m* (m4:translation (%pqn-pos node))
                                      (q:to-matrix4 (%pqn-quat node)))))

;;-  -  -  -  -  -  -  -

(defun pqn-pos (node) (%pqn-pos node))
(defun pqn-quat (node) (%pqn-quat node))

(defun (setf pqn-pos) (value node)
  (setf (%node-re-calc node) t
        (%pqn-pos node) value))

(defun (setf pqn-quat) (value node)
  (setf (%node-re-calc node) t
        (%pqn-quat node) value))

;;--------------------------------

(defstruct (axis-angle-node (:conc-name %aan-) (:constructor %make-aan)
                            (:include pos-quat-node))
  (axis (v! 0 1 0) :type (simple-array single-float (3)))
  (angle 0.0 :type single-float))

(defun make-axis-angle-node ()
  (%make-aan :update-func #'%axis-ang-update))

;;-  -  -  -  -  -  -  -

(declaim (ftype (function (node) (simple-array single-float (16)))
                %axis-ang-update))
(defun %axis-ang-update (node)
  (setf (pqn-quat node) (q:make-quat-from-axis-angle (%aan-axis node)
                                                     (%aan-angle node)))
  ;; trusts %pos-quat-update to set re-calc correctly
  (%pos-quat-update node))

;;-  -  -  -  -  -  -  -

(defun aan-axis (node) (%aan-axis node))
(defun aan-angle (node) (%aan-angle node))

(defun (setf aan-axis) (value node)
  (setf (%node-re-calc node) t
        (%aan-axis node) value))

(defun (setf aan-angle) (value node)
  (setf (%node-re-calc node) t
        (%aan-angle node) value))

;;--------------------------------
