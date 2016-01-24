(in-package :jungl.space)


;;----------------------------------------------------------------------
;; Model

(defun %mspace-transform (mspace)
  (declare ;;(optimize (speed 3) (debug 0))
	   (inline sr-to %mspace-only-sr))
  (sr-to (%mspace-only-sr mspace)))

(defun %set-mspace-transform (mspace transform &optional to-space)
  (let ((relationship (aref (%space-neighbours mspace) 0)))
    (when to-space
      (unless (= (sr-target-id relationship) (%space-nht-id to-space))
	(error "The model space ~s only has a relationship to the space ~s. Cannot define a relationship to the non-neighbour space ~s"
	       mspace (%space-ref (sr-target-id relationship)) to-space)))
    (setf (sr-to relationship)
	  transform)))

(defun %mspace-to-hspace-transform (from-space to-space)
  (if (eq (%space-root to-space) from-space)
      (collect-inverse-to to-space from-space)
      (let ((dest-root (%space-root to-space)))
	(m4:m* (collect-inverse-to to-space dest-root)
	       (%rspace-to-rspace-transform from-space dest-root)))))

(defun %mspace-to-rspace-transform (mspace rspace)
  (let* ((only-sr (%mspace-only-sr mspace))
	 (neighbour-id (sr-target-id only-sr))
	 (to-neighbour (sr-to only-sr))
	 (rspace-id (%space-nht-id rspace)))
    (if (= rspace-id neighbour-id)
	to-neighbour
	(m4:m* (%rspace-ids-transform neighbour-id rspace-id)
	       to-neighbour))))

;;----------------------------------------------------------------------
;; Relational

(defun %rspace-to-rspace-transform (space-a space-b)
  (%rspace-ids-transform (%space-nht-id space-a) (%space-nht-id space-b)))

(defun %rspace-to-hspace-transform (from-space to-space)
  (if (eq (%space-root to-space) from-space)
      (collect-inverse-to to-space from-space)
      (let ((dest-root (%space-root to-space)))
	(m4:m* (collect-inverse-to to-space dest-root)
	       (%rspace-to-rspace-transform from-space dest-root)))))

(defun %set-rspace-transform (from-space to-space transform)
  (unless (and (= (%space-kind from-space) +relational-space+)
	       (= (%space-kind to-space) +relational-space+))
    (if (= (%space-kind to-space) +model-space+)
	(error "Model->Relational relationships are one way so you have to set the transform on the model-space")
	(error "Relational spaces can only have direct relationships with other relational spaces.")))
  (%set-rspace-to-neighbour-transform (%space-nht-id from-space)
				      (%space-nht-id to-space)
				      transform))

;;----------------------------------------------------------------------
;; Hierarchical
;;
;; the hspace transform is from the parent to the current space
;;

(defun %hspace-transform (hspace)
  (declare ;;(optimize (speed 3) (debug 0))
   (inline sr-from %space-neighbours))
  (sr-to (aref (%space-neighbours hspace) 0)))

(defun %hspace-inverse-transform (space)
  (m4:affine-inverse (%hspace-transform space)))

(defun %set-hspace-transform (hspace transform &optional to-space)
  (when to-space
    (unless (eq (%space-parent hspace) to-space)
      (error "cannot set a relationship between the hierarchical space ~s and a space that is not the parent:~%parent:~s~%space-provided:~s"
	     hspace (%space-parent hspace) to-space)))
  (setf (sr-to (aref (%space-neighbours hspace) 0))
	transform))

(defun %hspace-to-mspace-transform (from-space to-space)
  (if (eq (%space-root to-space) from-space)
      (collect-inverse-to to-space from-space)
      (error "cannot transform from a hierarchical space to a model space if the space is not a child of the model space")))

(defun %hspace-to-rspace-transform (from-space to-space)
  (if (eq (%space-root from-space) to-space)
      (collect-transform-to from-space to-space)
      (let ((dest-root (%space-root from-space)))
	(m4:m* (%rspace-to-rspace-transform from-space dest-root)
	       (collect-inverse-to to-space dest-root)))))

(defun %hspace-to-hspace-transform (from-space to-space)
  (if (eq (%space-root from-space) (%space-root to-space))
      (%get-hierarchical-transform from-space to-space)
      (let ((from-root (%space-root from-space)))
	(m4:m*
	 (get-transform from-root to-space)
	 (collect-inverse-to from-space from-root)))))

(defun collect-inverse-to (start-space ancestor-space)
  (labels ((combine-inverse (accum space)
             (m4:m* (%hspace-inverse-transform space) accum)))
    ;; [TODO] unneccesary identity multiply, pass it start-space
    ;;        transform as initial-value and (parent-space start-space) as
    ;;        'of-space arg
    (m4:m* (%hspace-inverse-transform ancestor-space)
           (reduce-ancestors #'combine-inverse start-space ancestor-space
			     (m4:identity)))))

(defun collect-transform-to (start-space ancestor-space)
  (labels ((combine-transform (accum space)
             (m4:m* (%hspace-transform space) accum)))
    ;; [TODO] unneccesary identity multiply, pass it start-space
    ;;        transform as initial-value and (parent-space start-space) as
    ;;        'of-space arg
    (reduce-ancestors #'combine-transform start-space ancestor-space
                      (m4:identity))))


(defun find-common-ancestor (space-a space-b)
  (labels ((collect-id (accum space) (cons space accum)))
    (loop :for left :in (reduce-ancestors #'collect-id space-a)
       :for right :in (reduce-ancestors #'collect-id space-b)
       :when (eq left right) :return left
       :finally (return nil))))

(defun %get-hierarchical-transform (from-space to-space)
  (let* ((common-ancestor (find-common-ancestor from-space to-space))
	 (i (collect-inverse-to from-space common-ancestor))
         (f (collect-transform-to to-space common-ancestor)))
    (m4:m* i f)))

;;----------------------------------------------------------------------
;; get the matrix that transforms points from one space to another

(defun get-transform (from-space to-space)
  (declare (inline %mspace-to-rspace-transform
		   %mspace-to-hspace-transform
		   %rspace-to-hspace-transform
		   %hspace-to-mspace-transform
		   %hspace-to-rspace-transform
		   %hspace-to-hspace-transform))
  (kind-case* (from-space to-space)
	      :m->m (error "model to model space transforms are impossible as the model space defines a one-way relationship to a single neighbour space")
	      :m->r (%mspace-to-rspace-transform from-space to-space)
	      :m->h (%mspace-to-hspace-transform from-space to-space)

	      :r->m (error "relatation to model space transforms are impossible as the model space defines a one-way relationship to a single neighbour space")
	      :r->r (%rspace-to-rspace-transform from-space to-space)
	      :r->h (%rspace-to-hspace-transform from-space to-space)

	      :h->m (%hspace-to-mspace-transform from-space to-space)
	      :h->r (%hspace-to-rspace-transform from-space to-space)
	      :h->h (%hspace-to-hspace-transform from-space to-space)))

(defun get-transform-via (from-space to-space via-space)
  (m4:m* (get-transform via-space to-space)
	 (get-transform from-space via-space)))



(defun (setf get-transform) (m4-value from-space &optional to-space)
  (if to-space
      (kind-case (from-space :error t)
		 :m (%set-mspace-transform from-space m4-value to-space)
		 :r (%set-rspace-transform from-space to-space m4-value)
		 :h (%set-hspace-transform from-space m4-value to-space))
      (kind-case (from-space :error t)
		 :m (%set-mspace-transform from-space m4-value)
		 :r (error "Cannot directly set the transform when relationship is ambiguous, be sure to use the optional to-space arg")
		 :h (%set-hspace-transform from-space m4-value))))

;;----------------------------------------------------------------------
