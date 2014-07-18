;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; package.lisp --- The matrix package
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

(defpackage :base-matrices
  (:use :cl)
  (:export :m!))

(defpackage :matrix3
  (:use :cl)
  (:nicknames :m3)
  (:export :melm :identity-matrix3 :zero-matrix3 
           :make-matrix3 :make-from-rows :get-rows
           :get-row :make-from-columns :get-columns
           :get-column :determinate-cramer :inverse
           :mzerop :identityp :meql :transpose :adjoint
           :mtrace :rotation-from-euler 
           :rotation-from-axis-angle :scale
           :rotation-x :rotation-y :rotation-z
           :get-fixed-angles :get-axis-angle :m+ :m- :negate
           :m* :mcol*vec3 :mrow*vec3 :m*scalar)
  (:import-from :base-maths :float-zero
                :c-sqrt)
  (:import-from :vector3 
                :make-vector3)
  (:import-from :base-macros
                :apply-across-elements)
  (:import-from :base-vectors :v-x :v-y :v-z :v-w))

(defpackage :matrix4
  (:use :cl)
  (:nicknames :m4)
  (:export :melm :identity-matrix4 :zero-matrix4 
           :2dclipspace-to-imagespace-matrix4 :make-matrix4
           :mzerop :identityp :meql :minor :adjoint 
           :determinant :affine-inverse :transpose 
           :translation :rotation-from-matrix3
           :rotation-from-euler :rotation-from-axis-angle
           :scale :rotation-x :rotation-y
           :rotation-z :get-fixed-angles :mtrace
           :get-axis-angle :m+ :m- :negate :m*scalar
           :mcol*vec4 :mrow*vec4 :m* :transform
           :to-matrix3 :get-row :get-rows :get-column
           :get-columns)
  (:import-from :base-maths :float-zero
                :c-sqrt)
  (:import-from :vector3 
                :make-vector3)
  (:import-from :vector4
                :make-vector4)
  (:import-from :base-macros
                :apply-across-elements)
  (:import-from :base-vectors :v-x :v-y :v-z :v-w))


(defpackage :matrices
  (:use :cl)
  (:nicknames :m)
  (:export :zerop :unitp :+ :eq := :/= :1+ :1- :- :*
           :identityp :elt :elm :get-rows :get-row
           :get-columns :get-column :determinant
           :inverse :transpose :trace :negate) 
  (:shadow :zerop :unitp :+ :eq := :/= :1+ :1- :- :*
           :elt :trace))
