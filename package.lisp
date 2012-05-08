;;;; package.lisp

(defpackage #:cepl-matrix3
  (:use #:cl))

(defpackage #:cepl-vec3
  (:use #:cl)
  (:export :vec3 :c-= :c-/= :c-+ :c=+1 :c--1 :c-*
	   :c-*vec :c-/ :c-/vec :c-negate :c-length-squared
	   :c-length :c-distance-squared :c-distance :c-dot
	   :c-absolute-dot :c-normalize :c-cross
	   :*unit-x* :*unit-y* :*unit-z* :*unit-scale*
	   :c-x :c-y :c-z))

(defpackage #:cepl
  (:use #:cl #:cepl-vec3 #:cepl-matrix3))

