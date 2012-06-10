;;;; package.lisp

(defpackage :base
  (:use :cl)
  (:export :+float-threshold+
	   :+pi+
	   :float-zero
	   :float-zero-sq
	   :c-sqrt
	   :c-inv-sqrt))

(defpackage :math-macros
  (:use :cl)
  (:export :apply-across-elements
	   :v-x :v-y :v-z :v-w))

(defpackage :vector2
  (:use :cl)
  (:export :make-vector2 :v= :v/= :v+ :v+1 :v- :v-1 :v*
	   :v*vec :v/ :v/vec :negate :vlength-squared
	   :vlength :distance-squared :distance :dot
	   :absolute-dot :normalize :perp-dot
	   :*unit-x* :*unit-y* :*unit-scale*)
  (:import-from :base :float-zero
		      :float-zero-sq
		      :c-sqrt
		      :c-inv-sqrt)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y))

(defpackage :vector3
  (:use :cl)
  (:export :make-vector3 :v= :v/= :v+ :v+1 :v- :v-1 :v*
	   :v*vec :v/ :v/vec :negate :vlength-squared
	   :vlength :distance-squared :distance :dot
	   :absolute-dot :normalize :cross
	   :*unit-x* :*unit-y* :*unit-z* :*unit-scale*)
  (:import-from :base :float-zero
		      :float-zero-sq
		      :c-sqrt
		      :c-inv-sqrt)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z))

(defpackage :vector4
  (:use :cl)
  (:export :make-vector4 :v= :v/= :v+ :v+1 :v- :v-1 :v*
	   :v*vec :v/ :v/vec :negate :vlength-squared
	   :vlength :distance-squared :distance :dot
	   :absolute-dot :normalize :cross
	   :*unit-x* :*unit-y* :*unit-z* :*unit-w* :*unit-scale*)
  (:import-from :base :float-zero
		      :float-zero-sq
		      :c-sqrt
		      :c-inv-sqrt)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z :v-w))

(defpackage :matrix3
  (:use :cl)
  (:import-from :base :float-zero
		      :c-sqrt)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z :v-w))

;;[TODO] why does adding :vector3 in the :use cause conflicts?
(defpackage :matrix4
  (:use :cl)
  (:import-from :base :float-zero
		      :c-sqrt)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z :v-w))

(defpackage :cepl
  (:use :cl)
  (:import-from :vector2
		:make-vector2)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4))


(defpackage :arc-tuts
  (:use :cl :cepl )
  (:import-from :vector2
		:make-vector2)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4)
  (:import-from :math-macros
		:v-x :v-y :v-z :v-w))
