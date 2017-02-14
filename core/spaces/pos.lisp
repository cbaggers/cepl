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

;;----------------------------------------------------------------------
