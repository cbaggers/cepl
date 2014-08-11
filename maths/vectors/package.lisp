;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; package.lisp --- The vector package in math
;;;;
;;;; Copyright (c) 2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================
(in-package #:cl-user)

(defpackage :base-vectors
  (:use :cl)
  (:export :v! :v-x :v-y :v-z :v-w
           :v!byte :v!ubyte :v!int))

(defpackage :vector2
  (:use :cl)
  (:nicknames :v2)
  (:export :make-vector2 :v+ :v+1 :v- :v-1 :v* :v-eq
           :v*vec :v/ :v/vec :negate :vlength-squared
           :vlength :distance-squared :distance :dot
           :absolute-dot :normalize :perp-dot
           :*unit-x* :*unit-y* :*unit-scale*
           :vzerop :unitp :cross)
  (:import-from :base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :base-macros
                :apply-across-elements)
  (:import-from :base-vectors :v-x :v-y))

(defpackage :vector3
  (:use :cl)
  (:nicknames :v3)
  (:export :make-vector3 :v+ :v+1 :v- :v-1 :v* :v-eq
           :v*vec :v/ :v/vec :negate :vlength-squared
           :vlength :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :*unit-x* :*unit-y* :*unit-z* :*unit-scale*
           :vzerop :unitp :cross)
  (:import-from :base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :base-macros
                :apply-across-elements)
  (:import-from :base-vectors :v-x :v-y :v-z))

(defpackage :vector4
  (:use :cl)
  (:nicknames :v4)
  (:export :make-vector4 :v+ :v+1 :v- :v-1 :v* :v3* :v-eq
           :v*vec :v/ :v/vec :negate :vlength-squared
           :vlength :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :*unit-x* :*unit-y* :*unit-z* :*unit-w* :*unit-scale*
           :vzerop :unitp)
  (:import-from :base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :base-macros
                :apply-across-elements)
  (:import-from :base-vectors :v-x :v-y :v-z :v-w))

(defpackage :vectors
  (:use :cl)
  (:nicknames :v)
  (:export :v :make-vector :zerop :unitp := :+ :/= :1+ :1- :- :*
           :/ :length :length-squared :distance :distance-squared
           :dot :absolute-dot :perp-dot :normalize :cross :eq
           :swizzle :merge-into-vector) 
  (:shadow :zerop :+ :eq := :/= :1+ :1- :- :* :/ :length)
  (:import-from :vector2
                :make-vector2)
  (:import-from :vector3 
                :make-vector3)
  (:import-from :vector4
                :make-vector4))
