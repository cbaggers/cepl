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
;; gpu

(varjo::def-v-type-class svec4-g (varjo:v-vec4)
  ((varjo::core :initform nil :reader varjo:core-typep)
   (varjo::glsl-string :initform "#<svec4-g>" :reader varjo:v-glsl-string)))

(add-type-shadow 'svec4 'svec4-g)

(varjo:v-defmacro sv! (v &rest r)
  (if r
      `(%sv! ,v ,@r ,*current-space*)
      `(%sv! ,v ,*current-space*)))

(varjo:v-defun %sv! (v s) "#<svec4-g(~a, ~a)>"
	       (:vec4 vec-space-g) svec4-g)

(varjo:v-defun %sv! (v w s) "#<svec4-g(~a, ~a, ~a)>"
	       (:vec3 :float vec-space-g) svec4-g)

(varjo:v-defun %sv! (v1 v2 s) "#<svec4-g(~a, ~a, ~a)>"
	       (:vec2 :vec2 vec-space-g) svec4-g)

(varjo:v-defun %sv! (v1 z w s) "#<svec4-g(~a, ~a, ~a, ~a)>"
	       (:vec2 :float :float vec-space-g) svec4-g)

(varjo:v-defun %sv! (x y z w s) "#<svec4-g(~a, ~a, ~a, ~a, ~a)>"
	       (:float :float :float :float vec-space-g) svec4-g)


(varjo:v-defun v! (p) "~a" (svec4-g) :vec4)

(varjo:v-defun get-transform (s1 s2) "#<cepl.space:get-transform(~a, ~a)>"
	       (vec-space-g vec-space-g) :mat4)
