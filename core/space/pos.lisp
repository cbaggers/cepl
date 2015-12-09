(in-package :spaces)

;; terminology
;; -----------
;; point - a vector, has no information on space so all points with equivilent
;;         components are equal
;; location - a point in all spaces
;; pos - a point in a given space. two pos are only pos-eq if their point and
;;       space are equal. But they are pos-equal if the point is equal after
;;       transform between the spaces

(defstruct (pos (:constructor %%-pos!))
  (space (error "positions can not be made without a space")
	 :type spaces::space
	 :read-only t)
  (point (v! 0 0 0) :type (or (simple-array single-float (2))
			      (simple-array single-float (3))
			      (simple-array single-float (4)))))

(defvar *default-pos-space*)

(defun p! (vec &optional (space *default-pos-space*))
  (%%-pos! :space space :point vec))

(defun re-space (new-space pos)
  "makes a new point in the same location as the first but relative to the
   provided new space"
  (p! (m4:transform (get-transform (pos-space pos) new-space)
		    (pos-point pos))
      new-space))

;;----------------------------------------------------------------------

(cgl:def-equivalent-type pos :vec4)
