(in-package :spaces)

;;----------------------------------------------------------------------
;; walk up the space graph collecting transforms

(defun collect-inverse-to (start-space ancestor-space)
  (labels ((combine-inverse (accum space)
n	     (m4:m* (space-inverse-transform space) accum)))
    (m4:m* (space-inverse-transform ancestor-space)
	   (reduce-ancestors #'combine-inverse start-space ancestor-space))))

(defun collect-transform-to (start-space ancestor-space)
  (labels ((combine-transform (accum space)
	     (m4:m* (space-transform space) accum)))
    (reduce-ancestors #'combine-transform start-space ancestor-space)))

;;----------------------------------------------------------------------
;; finding a common ancestor

(defun find-common-ancestor (space-a space-b)
  (labels ((collect-id (accum space) (cons space accum)))
    (loop :for left :in (reduce-ancestors #'collect-id space-a)
       :for right :in (reduce-ancestors #'collect-id space-b)
       :when (eq left right) :return left
       :finally (return nil))))

;;----------------------------------------------------------------------
;; get the matrix that transforms points from one space to another

(defun get-transform (from-space to-space)
  (let* ((common-ancestor (find-common-ancestor from-space to-space))
	 (i (collect-inverse-to from-space common-ancestor))
	 (f (collect-transform-to common-ancestor to-space)))
    (m4:m* i f)))

;;----------------------------------------------------------------------
