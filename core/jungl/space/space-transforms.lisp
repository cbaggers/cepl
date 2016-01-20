(in-package :jungl.space)

;;----------------------------------------------------------------------
;; walk up the space graph collecting transforms

(defun collect-inverse-to (start-space ancestor-space)
  (labels ((combine-inverse (accum space)
             (m4:m* (space-inverse-transform space) accum)))
    ;; [TODO] unneccesary identity multiply, pass it start-space
    ;;        transform as initial-value and (parent-space start-space) as
    ;;        'of-space arg
    (m4:m* (space-inverse-transform ancestor-space)
           (reduce-ancestors #'combine-inverse start-space ancestor-space
			     (m4:identity)))))

(defun collect-transform-to (start-space ancestor-space)
  (labels ((combine-transform (accum space)
             (m4:m* (space-transform space) accum)))
    ;; [TODO] unneccesary identity multiply, pass it start-space
    ;;        transform as initial-value and (parent-space start-space) as
    ;;        'of-space arg
    (reduce-ancestors #'combine-transform start-space ancestor-space
                      (m4:identity))))

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
  (let ((f-root (space-root from-space))
	(t-root (space-root to-space)))
    (if (eq f-root t-root)
	(%get-hierarchical-transform from-space to-space)
	(%get-non-hierarchical-transform from-space f-root
					 to-space t-root))))

(defun %get-hierarchical-transform (from-space to-space)
  (let* ((common-ancestor (find-common-ancestor from-space to-space))
	 (i (collect-inverse-to from-space common-ancestor))
         (f (collect-transform-to to-space common-ancestor)))
    (m4:m* i f)))

(defun %get-non-hierarchical-transform (from-space from-root to-space to-root)
  (let* ((ft (collect-inverse-to from-space from-root))
         (tt (collect-transform-to to-space to-root))
	 (transform-func (transformer-to (%get-nht from-root to-root))))
    (m4:m* (funcall transform-func ft) tt)))

;;----------------------------------------------------------------------
