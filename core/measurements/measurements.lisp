(in-package :cepl.measurements)

;;----------------------------------------------------------------------

(defgeneric resolution (object)) ;; returns a vecn
(defgeneric (setf resolution) (value object))

(defmethod resolution ((object t))
  (error "CEPL: Cannot extract a resolution from ~s object:~%~s"
	 (type-of object) object))



;;----------------------------------------------------------------------

(defgeneric dimensions (object)) ;; returns a list
(defgeneric (setf dimensions) (value object))

(defmethod dimensions ((object t))
  (error "CEPL: Cannot extract dimensions from ~s object:~%~s"
	 (type-of object) object))

(defmethod dimensions ((object array))
  (array-dimensions object))

;;----------------------------------------------------------------------
