(in-package :jungl.space)

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
	 :type space
	 :read-only t))

;; (defstruct (pos2 (:include pos) (:constructor %%-pos2!))
;;   (point (v! 0 0) :type (simple-array single-float (2))))

(defstruct (pos3 (:include pos) (:constructor %%-pos3!))
  (point (v! 0 0 0) :type (simple-array single-float (3))))

(defstruct (pos4 (:include pos) (:constructor %%-pos4!))
  (point (v! 0 0 0 0) :type (simple-array single-float (4))))

(defvar *default-pos-space*)

(defun p! (vec &optional (space *default-pos-space*))
  (case= (length vec)
    (3 (%%-pos3! :space space :point vec))
    (4 (%%-pos4! :space space :point vec))))

(defun to-space (destination-space pos)
  (typecase pos
    (pos3 (m4:*v (get-transform (pos-space pos) destination-space)
		 (pos3-point pos)))
    (pos4 (m4:*v (get-transform (pos-space pos) destination-space)
		 (pos4-point pos)))))

(defun re-space (new-space pos)
  "makes a new point in the same location as the first but relative to the
   provided new space"
  (typecase pos
    (pos3 (p! (m4:*v (get-transform (pos-space pos) new-space)
		      (pos3-point pos))
	      new-space))
    (pos4 (p! (m4:*v (get-transform (pos-space pos) new-space)
		      (pos4-point pos))
	      new-space))))

;;----------------------------------------------------------------------
;; gpu

(varjo::def-v-type-class pos4-g (varjo:v-vec4)
  ((varjo::core :initform nil :reader varjo:core-typep)
   (varjo::glsl-string :initform "#<pos4-g>" :reader varjo:v-glsl-string)))

(varjo:v-defmacro p! (v &rest r)
  (if r
      `(%p! ,v ,@r ,*current-space*)
      `(%p! ,v ,*current-space*)))

(varjo:v-defun %p! (v s) "#<pos4-g(~a, ~a)>"
	       (:vec4 space-g) pos4-g)

(varjo:v-defun %p! (v w s) "#<pos4-g(~a, ~a, ~a)>"
	       (:vec3 :float space-g) pos4-g)

(varjo:v-defun %p! (v1 v2 s) "#<pos4-g(~a, ~a, ~a)>"
	       (:vec2 :vec2 space-g) pos4-g)

(varjo:v-defun %p! (v1 z w s) "#<pos4-g(~a, ~a, ~a, ~a)>"
	       (:vec2 :float :float space-g) pos4-g)

(varjo:v-defun %p! (x y z w s) "#<pos4-g(~a, ~a, ~a, ~a, ~a)>"
	       (:float :float :float :float space-g) pos4-g)


(varjo:v-defun v! (p) "~a" (pos4-g) :vec4)
(varjo:v-defun v! (p) "~a" (:vec4) :vec4)
