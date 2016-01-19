(in-package :space)

;;----------------------------------------------------------------------
;; Space Events

(defstruct (space-event (:include skitter:skitter-event))
  (flush nil
         :type boolean)
  (matrix-4 (m4:identity)
            :type (simple-array single-float (16))))

;;----------------------------------------------------------------------
;; Space Node

(skitter::def-event-node-type (space :constructor %make-space)
  (transform (m4:identity)
             :type (simple-array single-float (16)))
  (has-propagated nil :type boolean)
  (nht (make-hash-table) :type hash-table) ;; {TODO} swap out later
  (root nil :type (or null space)))

(defun make-space (transform &optional parent-space)
  (assert (typep transform '(simple-array single-float (16))))
  (assert (or (null parent-space) (typep parent-space 'space)))
  (%make-space :transform transform :subscribe-to parent-space
	       :root (when parent-space
		       (%find-root parent-space))))

(defmethod print-object ((object space) stream)
  (format stream "#<SPACE ~s>" (%uid object)))

(defun space! (transform &optional parent-space)
  (make-space transform parent-space))

(defun %uid (space) (evt::event-node-uid space))

;;----------------------------------------------------------------------
;; Hierarchical Transforms

(defun parent-space (space)
  (first (evt::event-node-subscriptions space)))

(defun %find-root (space)
  (labels ((walk (x)
	     (let ((ps (parent-space x)))
	       (if ps (walk ps) x))))
    (walk space)))

(defun space-inverse-transform (space)
  (m4:affine-inverse (space-transform space)))

;;----------------------------------------------------------------------
;; Non-Hierarchical Transforms

(defstruct transformer
  to
  from)

(defun %get-nht (space-a space-b)
  (or (gethash space-b (space-nht space-a))
      (error "these two spaces are not hierarchically related")))

(defun add-non-hierarchical-relationship (space-a space-b a->b b->a)
  (assert (and (null (parent-space space-a))
	       (null (parent-space space-b))))
  (assert (no-space-cycles space-a space-b))
  (setf (gethash space-b (space-nht space-a))
	(make-transformer :to a->b :from b->a)
	(gethash space-a (space-nht space-b))
	(make-transformer :to b->a :from a->b))
  space-a)

(defun remove-non-hierarchical-relationship (space-a space-b)
  (assert (and (null (parent-space space-a))
	       (null (parent-space space-b))))
  (remhash space-b (space-nht space-a))
  (remhash space-a (space-nht space-b)))

(defun no-space-cycles (space-a space-b)
  (declare (ignore space-a space-b))
  t)

;;----------------------------------------------------------------------
;; GPU

(varjo::def-v-type-class space-g (varjo::v-type)
  ((varjo::core :initform nil :reader varjo::core-typep)
   (varjo::glsl-string :initform "#<space>" :reader varjo::v-glsl-string)))

;; a name for the space
(defvar *current-space* (gensym "current-space"))
