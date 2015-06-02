(in-package :base-space)

;;--------------------------------------------------------
;;--------------------------------------------------------
;; space tree system

(defparameter *space-tree* nil)
(defvar *heart-beat-byte* 0) ;; will be used in GC

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
     :depth (1+ (space-depth parent-space))
     :world-space-id (space-world-space-id parent-space))))

(defun batch-make-space (make-space-function parent-space arg-lists)
  (let ((parent-space parent-space))
    (loop :for args :in arg-lists 
       :for new-space := (apply make-space-function parent-space args)
       :do (setf parent-space (space-parent new-space))
       :collect new-space)))

;;--------------------------------------------------------
;; Update Space

(defun update-space-m4 (space mat4)
  (setf (space-transform space) mat4)
  ;;(handle-dirtying-of-child-spaces space)
  )

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
