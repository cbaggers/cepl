;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; package.lisp

(defpackage :cepl-generics
  (:use :cl)
  (:export :pos
           :rot
           :dir
           :vec
           :size
           :norm
           :tex
           :col
           :action
           :button
           :button-state
           :clicks
           :data
           :delta
           :etype
           :id
           :key
           :key-state
           :pos
           :repeating
           :state
           :timestamp))

(defpackage :cepl-backend
  (:use :cl)
  (:export :init
           :start
           :shutdown
           :get-event-pump
           :get-swap-func
           :*backend*))

(defpackage :cepl-utils
  (:use :cl)
  (:nicknames :utils)
  (:export :gdefun
           :dbind
           :assoc-bind
           :sn-equal
           :listify
           :replace-nth
           :hash-values
           :hash-keys
           :intersperse
           :walk-replace
           :file-to-string
           :flatten
           :find-in-tree
           :mkstr
           :symb
           :make-keyword
           :kwd
           :group
           :safe-read-from-string
           :sub-at-index
           :symb-package
           :lispify-name
           :symbol-name-equal
           :range
           :rangei
           :arange
           :arangei
           :mapcat
           :deferror
           :split-seq-by-seq
           :print-mem
           :map-hash
           :last1
           :p->))

(defpackage :base-maths
  (:use :cl)
  (:export :clamp
           :clampf
           :+float-threshold+
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
           :degrees
           :radians))

(defpackage :maths
  (:use :cl)
  (:export :lerp :mix :stepv :clamp :smoothstep :pulse
           :spline))

(defpackage :base-vectors
  (:use :cl)
  (:export :v! :v-x :v-y :v-z :v-w
           :v!byte :v!ubyte :v!int))

(defpackage :base-matrices
  (:use :cl)
  (:export :m!))

(defpackage :vector2
  (:use :cl)
  (:nicknames :v2)
  (:shadow :eql :+ :- :* :/ :length :zerop)
  (:export :make-vector2 :+ :- :* :eql
           :*vec :/ :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :perp-dot
           :*unit-x* :*unit-y* :*unit-scale*
           :zerop :unitp :cross :face-foreward :lerp
           :bezier :spline)
  (:import-from :base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :base-vectors :v-x :v-y))

(defpackage :vector3
  (:use :cl)
  (:nicknames :v3)
  (:shadow :incf :eql :+ :- :* :/ :length :zerop)
  (:export :make-vector3 :eql :+ :- :* :/
           :*vec :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :*unit-x* :*unit-y* :*unit-z* :*unit-scale*
           :zerop :unitp :cross :face-foreward :lerp
           :bezier :spline :incf)
  (:import-from :base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :base-vectors :v-x :v-y :v-z))

(defpackage :vector4
  (:use :cl)
  (:nicknames :v4)
  (:shadow :eql :+ :- :* :/ :length :zerop)
  (:export :make-vector4 :+ :- :* :/ :v3* :eql
           :*vec :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :*unit-x* :*unit-y* :*unit-z* :*unit-w* :*unit-scale*
           :zerop :unitp :face-foreward :lerp
           :bezier :spline)
  (:import-from :base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :base-vectors :v-x :v-y :v-z :v-w))

(defpackage :vectors
  (:use :cl)
  (:nicknames :v)
  (:export :v :make-vector :zerop :unitp := :+ :/= :1+ :1- :- :*
           :/ :length :length-squared :distance :distance-squared
           :dot :absolute-dot :perp-dot :normalize :cross :eql
           :swizzle :s~ :merge-into-vector :negate :face-foreward :lerp
           :mix :bezier :x :y :z :w)
  (:shadow :zerop :+ :eql := :/= :1+ :1- :- :* :/ :length)
  (:import-from :vector2
                :make-vector2)
  (:import-from :vector3
                :make-vector3)
  (:import-from :vector4
                :make-vector4))

(defpackage :matrix3
  (:use :cl)
  (:nicknames :m3)
  (:shadow :eql)
  (:export :melm :identity-matrix3 :zero-matrix3
           :make-matrix3 :make-from-rows :get-rows
           :get-row :make-from-columns :get-columns
           :get-column :determinate-cramer :inverse
           :mzerop :identityp :transpose :adjoint
           :mtrace :rotation-from-euler
           :rotation-from-axis-angle :scale
           :rotation-x :rotation-y :rotation-z
           :get-fixed-angles :get-axis-angle :m+ :m- :negate
           :m* :m*vec :mcol*vec3 :mrow*vec3 :m*scalar :eql)
  (:import-from :base-maths :float-zero
                :c-sqrt)
  (:import-from :vector3
                :make-vector3)
  (:import-from :base-vectors :v-x :v-y :v-z :v-w))

(defpackage :matrix4
  (:use :cl)
  (:nicknames :m4)
  (:shadow :eql)
  (:export :melm :identity-matrix4 :zero-matrix4
           :2dclipspace-to-imagespace-matrix4 :make-matrix4
           :mzerop :identityp :minor :adjoint
           :determinant :affine-inverse :transpose
           :translation :rotation-from-matrix3
           :rotation-from-euler :rotation-from-axis-angle
           :scale :rotation-x :rotation-y
           :rotation-z :get-fixed-angles :mtrace
           :get-axis-angle :m+ :m- :negate :m*scalar
           :mcol*vec4 :mrow*vec4 :m* :transform
           :to-matrix3 :get-row :get-rows :get-column
           :get-columns :eql)
  (:import-from :base-maths :float-zero
                :c-sqrt)
  (:import-from :vector3
                :make-vector3)
  (:import-from :vector4
                :make-vector4)
  (:import-from :base-vectors :v-x :v-y :v-z :v-w))


(defpackage :matrices
  (:use :cl)
  (:nicknames :m)
  (:export :zerop :unitp :+ :eql := :/= :1+ :1- :- :*
           :identityp :elt :elm :get-rows :get-row
           :get-columns :get-column :determinant
           :inverse :transpose :trace :negate
           :to-string)
  (:shadow :zerop :unitp :+ :eql := :/= :1+ :1- :- :*
           :elt :trace))

(defpackage :quaternions
  (:use :cl :base-maths)
  (:nicknames :q)
  (:shadow :lerp)
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

(defpackage :base-space
  (:use :cl :base-vectors :base-matrices)
  (:nicknames :cspace)
  (:shadow :space)
  ;;(:export :things)
  )


(defpackage :%cgl
  (:use :cl :cffi :cepl-utils :varjo :base-vectors :cepl-generics
        :split-sequence)
  )
(defpackage :cepl-gl
  (:use :cl :cffi :cepl-utils :varjo :base-vectors :cepl-generics
        :split-sequence :%cgl)
  (:nicknames :cgl)
  (:import-from :utils
                :deferror
                :print-mem)
  (:shadow :float)
  (:export :gl-context
           :*quad*
           :*quad-stream*
           ;;- - - - - - - -
           :make-context
           :has-feature
           :*gl-context*
           :%context-flags
           :major-version
           :minor-version
           :max-server-wait-timeout
           :min-map-buffer-alignment
           :extension-count
           :supported-shading-versions-count
           :timestamp
           :%draw-indirect-buffer-binding
           :%element-array-buffer-binding
           :%query-buffer-binding
           :%texture-buffer-binding
           :%vertex-array-binding
           :color-clear-value
           :color-writemask
           :depth-clear-value
           :depth-func~1
           :depth-test
           :depth-writemask
           :doublebuffer
           :draw-buffer
           :draw-bufferi
           :draw-framebuffer-binding
           :max-color-attachments
           :max-color-texture-samples
           :max-depth-texture-samples
           :max-draw-buffers
           :max-dual-source-draw-buffers
           :max-framebuffer-height
           :max-framebuffer-layers
           :max-framebuffer-samples
           :max-framebuffer-width
           :max-integer-samples
           :max-samples
           :read-buffer
           :read-framebuffer-binding
           :renderbuffer-binding
           :stencil-back-fail
           :stencil-back-func
           :stencil-back-pass-depth-fail
           :stencil-back-pass-depth-pass
           :stencil-back-ref
           :stencil-back-value-mask
           :stencil-back-writemask
           :stencil-clear-value
           :stencil-fail
           :stencil-func
           :stencil-pass-depth-fail
           :stencil-pass-depth-pass
           :stencil-ref
           :stencil-test
           :stencil-value-mask
           :stencil-writemask
           :stereo
           ;;- - - - - - - -
           :*current-viewport*
           :viewport
           :with-viewport
           :with-fbo-viewport
           :viewport-resolution
           :clear-gl-context-cache
           :free
           :update-display
           :valid-pixel-format-p
           :pixel-format
           :lisp-type->pixel-format
           :pixel-format->lisp-type
           :pixel-format->internal-format
           :internal-format->pixel-format
           :internal-format->lisp-type
           :lisp-type->internal-format
           :internal-format-from-pixel-format
           :pixel-format-from-internal-format
           :describe-pixel-format
           :defstruct-g
           :c-array-byte-size
           :gl-calc-byte-size
           :make-c-array-from-pointer
           :with-c-array
           :with-c-arrays
           :free-c-array
           :clone-c-array
           :make-c-array
           :c-array-pointer
           :aref-c
           :%aref-c
           :c-populate
           :subseq-g
           :subseq-c
           :pull-g
           :pull1-g
           :push-g
           :dimensions ; [TODO] this isnt really inline with array-dimensions
           :backed-by ; [TODO] is this the right name for the job?
           :element-type
           :1d-p
           :bind-buffer
           :force-bind-buffer
           :unbind-buffer
           :gen-buffer
           :buffer-data-raw
           :buffer-data
           :buffer-sub-data
           :multi-buffer-data
           :buffer-reserve-block-raw
           :buffer-reserve-block
           :buffer-reserve-blocks
           :make-gpu-array
           :make-gpu-arrays
           :with-gpu-array-as-c-array
           :with-gpu-array-as-pointer
           :suitable-array-for-index-p
           :bind-vao
           :bind-vertex-array
           :make-vao-from-formats
           :make-vao
           :make-raw-vertex-stream
           :make-buffer-stream
           :make-texture
           :bind-texture
           :with-texture-bound
           :mutable-texturep
           :upload-c-array-to-gpuarray-t ; this is a crap name
           :calc-sampler-type
           :sampler-type
           :dimensions-at-mipmap-level
           :establish-texture-type
           :gl-texture
           :gpu-array-t
           :texref
           :defpipeline
           :g->
           :defun-g
           :defmacro-g
           :define-compiler-macro-g
           :with-instances
           :free-managed-resources
           :free-buffer
           :free-buffers
           :free-vertex-stream
           :free-texture
           :free-gpu-array
           :free-vao
           :g-pn
           :g-pc
           :g-pt
           :g-pnc
           :g-pnt
           :g-pntc
           :pos
           :col
           :norm
           :tex
           ;;----------
           :map-g
           ;;----------
           :make-fbo
           :make-fbos
           :with-bind-fbo
           :fbo-attach
           :fbo-detach
           :attachment
           :with-fbo-slots
           :attachment-compatible
           :attachment-gpu-array
           :attachment-override-blending
           :attachment-source-rgb
           :attachment-source-alpha
           :attachment-destination-rgb
           :attachment-destination-alpha
           :fbo-detach
           ;;----------
           :make-ubo
           :ubo-data
           :ubo-index
           ;;----------
           :def-equivalent-type
           ;;----------
           :clear))

(defpackage :varjo-bridge-types
  (:use :cl))

(defpackage :cepl-camera
  (:nicknames :ccam)
  (:use :cl :cepl-generics :base-vectors)
  (:export :camera
           :make-camera
           :orthographic-projection
           :perspective-projection
           :world->cam
           :look-at
           :world-up
           :pos
           :dir
           :frame-size
           :fov
           :far
           :near
           :cam->clip-func
           :cam->clip
           :world->cam
           :make-cam-clip-matrix)
  (:import-from :vector2
                :make-vector2)
  (:import-from :vector3
                :make-vector3)
  (:import-from :vector4
                :make-vector4))

(defpackage :primitives
  (:use :cl
        :base-vectors
        :base-matrices
        :base-maths)
  (:export :latice-data
           :primitive-data
           :cap-data
           :plain-data
           :box-data
           :equilateral-triangle-data
           :sphere-data
           :prim-array
           :swap-winding-order))

(defpackage :tools
  (:use :cl
        :base-vectors
        :base-matrices
        :base-maths)
  (:export :rqpos))

(defpackage :cepl.events
  (:use :cl :cepl-utils :cells :cepl-generics)
  (:nicknames :evt)
  (:export :event
           :event-cell
           :case-events
           :map-evt
           :merge-evt
           :filter-evt
           :pump-events
           :|all-events|
           :observe
           :undefobserver
           :def-event-node
           :|mouse|
           :|sys|
           :|window|
           :|keyboard|
           :button-state
           :key-state
           :will-quit
           :window
           :win
           :mouse-scroll
           :mouse-button
           :mouse-motion
           :action
           :button
           :clicks
           :delta
           :etype
           :id
           :key
           :repeating
           :source-id
           :state
           :timestamp
           :data))

(defpackage :live
  (:use :cl :cepl-utils)
  (:export :main-loop
           :update-swank
           :peek
           :continuable))

(defpackage :cepl
  (:use :cl
        :cepl-generics
        :base-vectors
        :base-matrices
        :base-maths
        :temporal-functions
        :cepl-camera
        :cl-fad
        :cepl.events
        :named-readtables
        :cepl-gl)
  (:shadow :quit)
  (:import-from :live
                :continuable
                :update-swank
                :peek)
  (:import-from :utils
                :deferror
                :print-mem
                :p->)
  (:export :repl
           :quit
           :make-project
           ;----
           :pos
           :rot
           :dir
           :vec
           :size
           :norm
           :tex
           :col
           ;;---
           :case-events
           :map-evt
           :merge-evt
           :filter-evt
           :|all-events|
           :observe
           :undefobserver
           :def-event-node
           ;;---
           :update-swank
           :peek
           :*examples-directory*
           ;;---
           :v! :v-x :v-y :v-z :v-w
           :v!byte :v!ubyte :v!int
           ;;---
           :m!
           ;;---
           :rqpos
           :continuable
           ;;---
           :def-time-units
           :milliseconds
           :seconds
           :minutes
           :hours
           :tlambda
           :tdefun
           :before
           :after
           :between
           :each
           :then
           :repeat
           :whilst
           :%progress%
           :signal-expired
           :expiredp
           :expiredp+
           :make-stepper
           ;;---
           :camera
           :make-camera
           :orthographic-projection
           :perspective-projection
           :world->cam
           :look-at
           :world-up
           :pos
           :dir
           :frame-size
           :fov
           :far
           :near
           :cam->clip-func
           :cam->clip
           :world->cam
           :make-cam-clip-matrix
           ;;---
           :update-swank
           :peek
           ;;---
           :*quad*
           :*quad-stream*
           :clear
           :update-display
           :pixel-format
           :lisp-type->pixel-format
           :pixel-format->lisp-type
           :pixel-format->internal-format
           :internal-format->pixel-format
           :internal-format->lisp-type
           :lisp-type->internal-format
           :describe-pixel-format
           :with-instances
           :defpipeline
           :defun-g
           :defmacro-g
           :defstruct-g
           :pull-g
           :pull1-g
           :push-g
           :make-c-array
           :with-c-array
           :with-c-arrays
           :free-c-array
           :aref-c
           :c-array-pointer
           :c-populate
           :make-gpu-array
           :make-gpu-arrays
           :subseq-g
           :subseq-c
           :with-gpu-array-as-c-array
           :with-gpu-array-as-pointer
           :make-buffer-stream
           :make-texture
           :sampler-type
           :with-texture-bound
           :g-pn
           :g-pc
           :g-pt
           :g-pnc
           :g-pnt
           :g-pntc
           :texref
           :map-g
           :make-fbo
           :make-fbos
           :with-bind-fbo
           :with-fbo-slots
           :fbo-attach
           :attachment
           :attachment-compatible
           :fbo-detach
           :*current-viewport*
           :viewport
           :with-viewport
           :with-fbo-viewport
           :def-equivalent-type
           ;;---
           :make-ubo
           :ubo-data
           :ubo-index))
