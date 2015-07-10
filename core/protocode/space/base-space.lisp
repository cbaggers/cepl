;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; Functions and macros for handling space

(in-package :base-space)

;;--------------------------------------------------------
;; space

(defconstant +max-child+ 16) ;; must be multiple of 16
(defconstant +child-pool-size-bits+ (floor (log 16 2)))
(defstruct (space (:constructor %make-space))
  ;; unique id, must never clash or be reused
  (id -1 :type (unsigned-byte 32))
  ;; the transform this to get a point into this space from the parent
  (transform (m4:identity-matrix4) :type (simple-array single-float (16)))
  ;; the parent space, this is the only pointer in the space graphs. This is 
  ;; to ensure CL's GC will be able to clean this tree without requiring the 
  ;; 'user' of a space to free it explicitly
  (parent nil :type (or null space))
  ;; has to be stored so that we can use id as route
  ;; max number of children is +max-child+
  (child-count 0 :type (unsigned-byte 4))
  ;; used to identify which world we are in
  (world-space-id 0 :type (unsigned-byte 8))
  (depth 0 :type (unsigned-byte 8)))

(defun space= (space-a space-b) (= (space-id space-a) (space-id space-b)))

(defmethod print-object ((object space) stream)
  (format stream "#<SPACE ~{~a~^->~} :world ~a :child# ~a>"
          (collect-parents-slot object 'id)
          (space-world-space-id object)
          (space-child-count object)))

;;--------------------------------------------------------
;; Utils

(defun calculate-id (parent-space)
  (+ (ash (1+ (space-child-count parent-space))
          (* (space-depth parent-space) 4))
     (space-id parent-space)))

(defun find-parent-if (space predicate)
  (when space
    (if (funcall predicate space)
        space
        (find-parent-if (space-parent space) predicate))))

(defun collect-parents-slot (space slot-name &optional stop-space)
  (when space
    (cons (slot-value space slot-name)
          (when (or (null stop-space) (not (space= space stop-space)))
            (collect-parents-slot (space-parent space) slot-name stop-space)))))

(defun collect-parent-transforms (space &optional stop-space)
  (when space
    (cons (space-transform space)
          (when (or (null stop-space) (not (space= space stop-space)))
            (collect-parent-transforms (space-parent space) stop-space)))))

(defun room-for-child-p (space) (< (space-child-count space) (- +max-child+ 1)))

(defun new-world-space ()
  (assert (< *next-world-id* 32))
  (prog1
      (setf (aref *world-pool* *next-world-id*)
            (%make-space :id 0 :transform (m4:identity-matrix4)
                         :world-space-id *next-world-id*))
    (incf *next-world-id*)))

(defun spaces-on-same-path (space-a space-b)
  (declare (optimize speed))
  (let ((id-a (space-id space-a))
        (id-b (space-id space-b)))
    (if (< id-a id-b)
        (%id-on-same-path-p id-b id-a (space-depth space-a))
        (%id-on-same-path-p id-a id-b (space-depth space-b)))))

(defun %spaces-on-same-path (space-a space-b)
  (declare (optimize speed))
  (let ((id-a (space-id space-a))
        (id-b (space-id space-b)))
    (%id-on-same-path-p id-a id-b (space-depth space-b))))

(defun %id-on-same-path-p (id-a id-b b-depth)
  (declare (optimize speed)
           ((unsigned-byte 64) id-a id-b)
           ((unsigned-byte 4) b-depth))
  (let* ((x (logxor id-a id-b))
         (depth (* 4 b-depth)))
    (= (logand x (ash (the (unsigned-byte 64) 15)
                      depth))
       x)))

(defun lowest-common-ancestor (space-a space-b)
  (declare (optimize speed))
  (let ((id-a (space-id space-a))
        (id-b (space-id space-b)))
    (declare ((unsigned-byte 64) id-a id-b))
    (loop :for i :below (min id-a id-b)
       :if (= (logand id-a 15) (logand id-b 15))
       :do (progn (setq id-a (ash id-a -4))
                  (setq id-b (ash id-b -4)))
       :else :return (let ((result space-a))
                       (loop :for l :from i :below (space-depth space-a) :do
                          (setq result (space-parent result)))
                       result))))
(defun %walk-spaces-multiplying (space-a space-b)
  )
