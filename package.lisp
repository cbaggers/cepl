;;;; package.lisp

(defpackage #:base
  (:use #:cl)
  (:export :+float-threshold+
	   :+pi+
	   :float-zero
	   :c-sqrt
	   :c-inv-sqrt))

(defpackage #:vector3
  (:use #:cl #:base)
  (:export :make-vector3 :v= :v/= :v+ :v+1 :v- :v-1 :v*
	   :v*vec :v/ :v/vec :negate :vlength-squared
	   :vlength :distance-squared :distance :dot
	   :absolute-dot :normalize :cross
	   :*unit-x* :*unit-y* :*unit-z* :*unit-scale*
	   :v-x :v-y :v-z))

(defpackage #:matrix3
  (:use #:cl #:base #:vector3)
  (:import-from :base :float-zero
		      :c-sqrt)
  (:import-from :vector3 :v-x
		         :v-y
		         :v-z
		         :make-vector3))

(defpackage #:cepl
  (:use #:cl #:base #:vector3 #:matrix3))

