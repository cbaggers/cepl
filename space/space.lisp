(in-package :base-space)

;;--------------------------------------------------------
;;--------------------------------------------------------
;; space tree system

(defparameter *space-tree* nil)
(defvar *heart-beat-byte* 0) ;; will be used in GC
(defun get-next-index () (prog1 *next-id* (incf index)))

;;--------------------------------------------------------
;; space

(defconstant +max-child+ 16) ;; must be multiple of 16
(defconstant +child-pool-size-bits+ (floor (log 16 2)))
(defstruct (space (:constructor %make-space))
  ;; unique id, must never clash or be reused
  (id -1 :type (unsigned-byte 32))
  ;; the transform this to get a point into this space from the parent
  ;; {TODO} dont like 16 being hardcoded
  (transform (m4:identity-matrix4) :type (simple-array single-float (16)))
  ;; the parent space, this is the only pointer in the space graphs. This is 
  ;; to ensure CL's GC will be able to clean this tree without requiring the 
  ;; 'user' of a space to free it explicitly
  (parent nil :type (or null space))
  ;; has to be stored so that we can use id as route
  ;; max number of children is +max-child+
  (child-count 0 :type (unsigned-byte 4))
  ;; used to identify which world we are in
  (world-space-id 0 :type (unsigned-byte 8)))

(defun space= (space-a space-b) (= (space-id space-a) (space-id space-b)))

;;--------------------------------------------------------
;; Utils

(defun calculate-id (parent-space)
  (+ (ash (space-id parent-space) +child-pool-size-bits+)
     (space-child-count parent-space)))

(defun find-parent-if (space predicate)
  (when space
    (if (funcall predicate space)
        space
        (find-parent-if (space-parent space) predicate))))
(defun collect-parent-transforms (space &optional stop-space)
  (when space
    (cons (space-transform space)
          (when (not (space= space stop-space))
            (collect-parent-transforms (space-parent space))))))

(defun room-for-child-p (space) (< (space-child-count space) +max-child+))

;;--------------------------------------------------------
;; WORLD SPACE
;; 
;; Pool of worlds. Needed as only +max-child+ children allowd
;; id of worldspace node is always 0

(defparameter *next-world-id* 0)
(progn
  (defparameter *world-pool* (make-array 32 :initial-element nil))
  (new-world-space)
  (defparameter /WORLD/ (aref *world-pool* 0)))

(defun new-world-space ()
  (assert (< *next-world-id* 32))
  (setf (aref *world-pool* *next-world-id*)
        (%make-space :id 0 :transform (m4:identity-matrix4)
                     :world-space-id *next-world-id*))
  (incf *next-world-id*))

(defun world-spacep (space) (= (space-id space) 0))

;;--------------------------------------------------------
;; Create Space

(defun %make-parallel-space-same-world (space-parallel-to nearest-viable-space)
  (let* ((parent-transforms (collect-parent-transforms 
                             space-parallel-to nearest-viable-space))
         (transform (if (= (length parent-transforms) 1)
                        (first parent-transforms)
                        (reduce #'m4:m* (rest parent-transforms)
                                :initial-value (first parent-transforms)))))
    (make-space-m4 nearest-viable-space transform)))

(defun %make-parallel-space-new-world (space-parallel-to)
  (let* ((parent-transforms (collect-parent-transforms space-parallel-to))
         (transform (if (= (length parent-transforms) 1)
                       (first parent-transforms)
                       (reduce #'m4:m* (rest parent-transforms)
                               :initial-value (first parent-transforms))))
         (world-space (new-world-space)))
    (make-space-m4 world-space transform)))

(defun %make-parallel-space (space-parallel-to)
  (let ((nearest-viable-space 
         (find-parent-if space-parallel-to #'room-for-child-p)))
    (if nearest-viable-space
        (%make-parallel-space-same-world space-parallel-to nearest-viable-space)
        (%make-parallel-space-new-world space-parallel-to))))

(defun %find-acceptable-parent (parent-space)
  (if (room-for-child-p parent-space)
      parent-space
      (%make-parallel-space parent-space)))

(defun %update-parent-with-child (parent-space)
  (incf (space-child-count parent-space))
  parent-space)

(defun make-space-m4 (parent-space mat4)
  (let ((parent-space (%find-acceptable-parent parent-space)))    
    (%make-space
     :id (calculate-id parent-space)
     :transform mat4
     :parent (%update-parent-with-child parent-space)
     :world-space-id (space-world-space-id parent-space))))

;;--------------------------------------------------------
;; Update Space

(defun update-space-m4 (space mat4)
  (setf (space-transform space) mat4)
  (handle-dirtying-of-child-spaces space))

;;--------------------------------------------------------
;;--------------------------------------------------------
;; Dirty Cache



;;--------------------------------------------------------
;;--------------------------------------------------------
;; Transform Caching

;;--------------------------------------------------------
;; datastructure



;;--------------------------------------------------------
;; indexing 




;;--------------------------------------------------------

(defun cache-transform (from-space to-space)
  (declare (ignore from-space to-space)))

