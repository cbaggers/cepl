(in-package :cepl.space)

;; terminology
;; -----------
;; point - a vector, has no information on space so all points with equivilent
;;         components are equal
;; location - a point in all spaces
;; pos - a point in a given space. two pos are only pos-eq if their point and
;;       space are equal. But they are pos-equal if the point is equal after
;;       transform between the spaces

(defstruct pos
  (space (error "positions can not be made without a space")
	 :type vec-space
	 :read-only t))

;; (defstruct (pos2 (:include pos) (:constructor %%-pos2!))
;;   (point (v! 0 0) :type (simple-array single-float (2))))

(defstruct (svec3 (:include pos) (:constructor %%-svec3!))
  (point (v! 0 0 0) :type (simple-array single-float (3))))

(defstruct (svec4 (:include pos) (:constructor %%-svec4!))
  (point (v! 0 0 0 0) :type (simple-array single-float (4))))

(defvar *default-pos-space*)

(defun sv! (vec &optional (space *default-pos-space*))
  (case= (length vec)
    (3 (%%-svec3! :space space :point vec))
    (4 (%%-svec4! :space space :point vec))))

(defun to-space (destination-space pos)
  (typecase pos
    (svec3 (m4:*v (get-transform (pos-space pos) destination-space)
		 (svec3-point pos)))
    (svec4 (m4:*v (get-transform (pos-space pos) destination-space)
		 (svec4-point pos)))))

(defun re-space (new-space pos)
  "makes a new point in the same location as the first but relative to the
   provided new space"
  (typecase pos
    (svec3 (sv! (m4:*v (get-transform (pos-space pos) new-space)
		      (svec3-point pos))
	      new-space))
    (svec4 (sv! (m4:*v (get-transform (pos-space pos) new-space)
		      (svec4-point pos))
	      new-space))))

;;----------------------------------------------------------------------
