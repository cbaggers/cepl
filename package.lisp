;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; package.lisp

(defpackage :base-maths
  (:use :cl)
  (:export :+float-threshold+
           :+one-degree-in-radians+
           :+pi+
           :float-zero
           :float>=0
           :float<=0
           :float>0
           :float<0
           :float-greater-than-zero
           :c-sqrt
           :c-inv-sqrt
           :degrees-to-radians
           :radians-to-degrees))

(defpackage :base-macros
  (:use :cl)
  (:export :defmemo
           :once-only
           :continuable
           :apply-across-elements))

;; uncomment when using lispbuilder instead of lispbuilder-mini
;; (defpackage :lbm-sdl
;;   (:use :cl)
;;   (:nicknames :sdl
;;   (:export :init-sdl
;;            :quit-sdl
;;            :with-init-sdl
;;            :collect-sdl-event-types
;;            :get-sdl-event
;;            :case-events))

(defpackage :base-time
  (:use :cl)
  (:nicknames :ct :ctime)
  (:export :absolute-system-time
           :make-time-buffer
           :make-time-cache
           :make-itime-buffer
           :make-itime-cache
           :make-stepper
           :on-step-call
           :withinp
           :beforep
           :t<
           :t>
           :temporally-expired
           :untilp
           :afterp
           :tlambda
           :with-expired))

(defpackage :cepl-utils
  (:use :cl)
  (:nicknames :utils)
  (:export :intersperse
           :update-swank
           :walk-replace
           :file-to-string
           :flatten
           :mkstr
           :symb
           :symb-package
           :make-keyword
           :kwd
           :group
           :safe-read-from-string
           :sub-at-index
           :symbolicate-package
           :lispify-name))

(defpackage :cepl-gl
  (:use :cl :cffi :base-macros)
  (:nicknames :cgl)
  (:import-from :cl-opengl
                :clear-color
                :enable
                :disable
                :cull-face
                :front-face
                :depth-mask
                :depth-func
                :depth-range
                :clear
                :clear-depth
                :flush
                :viewport)
  (:shadow :float)
  (:export :clear-color
           :cls
           :defpipeline
           :defpipeline?
           :enable
           :disable
           :cull-face
           :front-face
           :glambda
           :depth-mask
           :depth-func
           :depth-range
           :clear
           :clear-depth
           :flush
           :viewport
           :bind-buffer
           :gen-buffer
           :buffer-data
           :buffer-sub-data
           :multi-buffer-data
           :buffer-reserve-raw-block
           :buffer-reserve-block
           :buffer-reserve-blocks
           :gl-type-format
           :bind-vao
           :bind-vertex-array
           :make-vao
           :make-vao-from-formats
           :make-vao-from-buffer
           :make-vao-from-gpu-arrays
           :defglstruct
           :gl-pull
           :gl-pull-1
           :gl-push
           :gl-push-1
           :foreign-type-index
           :make-c-array
           :free-c-array
           :aref-gl
           :destructuring-populate
           :destructuring-allocate
           :free-all-buffers-in-pool
           :make-gpu-array
           :make-gpu-arrays
           :gl-subseq
           :gpu-array-pull
           :gpu-array-push
           :with-gpu-array-as-c-array
           :free-all-vaos-in-pool
           :make-gpu-stream
           :gpu-stream-vao
           :gpu-stream-start
           :gpu-stream-length
           :gpu-stream-draw-type
           :make-gpu-stream-from-gpu-arrays
           :free-managed-resources
           :program-attrib-count
           :program-attributes
           :program-uniform-count
           :program-uniforms
           :get-uniforms
           :use-program
           :shader-type-from-path
           :make-shader
           :load-shader
           :load-shaders
           :link-shaders
           :make-program
           :gl!
           :gpu!))

(defpackage :base-vectors
  (:use :cl)
  (:export :v! :v-x :v-y :v-z :v-w))

(defpackage :base-matrices
  (:use :cl)
  (:export :m!))

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
  (:export :make-vector4 :v+ :v+1 :v- :v-1 :v* :v-eq
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
           :m* :m*vec :m*scalar)
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

(defpackage :quaternions
  (:use :cl :base-maths)
  (:nicknames :q)
  (:export :w :x :y :z :zero-quit :zero-quatp 
           :unit-quatp :identity-quat :identity-quatp
           :make-quat :make-quat-from-vec3 
           :make-quat-from-rotation-matrix3 
           :make-quat-from-axis-angle
           :make-quat-from-vectors
           :make-quat-from-fixed-angles 
           :magnitude :norm :quat-eql :quat-!eql
           :copy :get-axis-angle :normalize :qconjugate
           :inverse :q+1 :q+ :q-1 :q- :q* :q*quat
           :dot :rotate :lerp :slerp :approx-slerp
           :to-matrix3 :to-matrix4))

(defpackage :cepl-camera
  (:nicknames :ccam)
  (:use :cl)
  (:export :calculate-frustrum-scale
           :make-cam-clip-matrix)
  (:import-from :vector2
                :make-vector2)
  (:import-from :vector3 
                :make-vector3)
  (:import-from :vector4
                :make-vector4))

(defpackage :model-parsers
  (:use :cl)
  (:export :parse-obj-file
           :parse-lisp-model
           :load-lisp-model)
  (:import-from :vector2
                :make-vector2)
  (:import-from :vector3 
                :make-vector3)
  (:import-from :vector4
                :make-vector4))

(defpackage :3dstub
  (:nicknames :stub)
  (:use :cl
        :base-vectors
        :base-matrices
        :base-maths)
  (:export :vc :vcn :initialize :point-camera-at
           :update-view :draw :primitive-model 
           :load-model :camera-position :pos :rot :color
           :scale))

(defpackage :cepl
  (:use :cl
        :base-vectors
        :base-matrices
        :base-maths
        :base-time
        :base-macros
        ;; :base-lispbuilder
        )
  (:import-from :cepl-gl
                :defpipeline
                :defpipeline?
                :glambda
                :defglstruct
                :gl-pull
                :gl-pull-1
                :gl-push
                :gl-push-1
                :make-c-array
                :free-c-array
                :aref-gl
                :destructuring-populate
                :destructuring-allocate
                :make-gpu-array
                :make-gpu-arrays
                :gl-subseq
                :with-gpu-array-as-c-array
                :make-gpu-stream
                :make-gpu-stream-from-gpu-arrays
                :gl! 
                :gpu!)
  (:import-from :3dstub :pos :rot :color :scale)
  (:export :repl))
