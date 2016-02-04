;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; package.lisp

(defpackage :cepl.host
  (:use :cl)
  (:export :init
           :start
           :shutdown
	   :cache-step-func
           :get-step-func
           :get-swap-func))

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
           :clicks
           :data
           :delta
           :etype
           :id
           :key
           :pos
           :repeating
           :state
           :timestamp))

(defpackage :cepl-utils
  (:use :cl)
  (:nicknames :utils)
  (:export :gdefun
           :dbind
           :assoc-bind
	   :case=
           :sn-equal
           :listify
           :replace-nth
           :hash-values
           :hash-keys
           :intersperse
           :walk-replace
	   :make-length-same
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
	   :asserting
           :split-seq-by-seq
           :print-mem
           :map-hash
	   :with-hash
	   :with-hash*
           :last1
           :p->
	   :split-string))

(defpackage :%jungl
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
	:cepl-generics :split-sequence :named-readtables)
  (:shadowing-import-from :rtg-math :v!))

(defpackage :jungl.space.routes
  (:use #:cl #:fn #:named-readtables #:cepl-utils)
  (:export :id! :free-id :reset :get-route :map-route :reduce-route :add-id))

(defpackage :jungl
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
	:cepl-generics :split-sequence :%jungl :named-readtables)
  (:shadowing-import-from :rtg-math :v!)
  (:import-from :utils
                :deferror
                :print-mem)
  (:shadow :float :space)
  (:export :cls
           :gl-context
           :*quad*
           :*quad-stream*
           :current-viewport
           ;;- - - - - - - -
           :make-context
           :fbo
           :has-feature
           :*gl-context*
           :%context-flags
           :major-version
           :minor-version
           :version-float
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
           :viewport
	   :make-viewport
	   :clone-viewport
           :with-viewport
           :with-fbo-viewport
           :viewport-resolution
	   :viewport-resolution-v!
           :clear-gl-context-cache
           :free
           :update-display
           :valid-pixel-format-p
           :pixel-format
           :pixel-format->lisp-type
           :pixel-format->internal-format
           :internal-format->pixel-format
           :internal-format->lisp-type
           :lisp-type->pixel-format
           :lisp-type->internal-format
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
           :make-sampler
           :with-sampling
           :lod-bias
           :min-lod
           :max-lod
           :magnify-filter
           :minify-filter
           :set-minify-filter
           :calc-minify-filter
           :wrap
           :compare
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
           :mode-rgb
           :mode-alpha
           :source-rgb
           :source-alpha
           :destination-rgb
           :destination-alpha
           :blending
           :make-blending-params
           :with-blending
           :per-attachment-blending-available-p
           :fbo-detach
           ;;----------
           :make-ubo
           :ubo-data
           :ubo-index
           ;;----------
           :clear
	   ;;----------
	   :def-compile-pass
	   :def-deep-pass
	   :set-uniform
	   :remove-uniform
	   :set-arg-val
	   ))

(defpackage :jungl.space
  (:use :cl :cepl-utils :rtg-math.types :rtg-math :named-readtables
	:varjo :varjo-lang)
  (:shadow :space)
  (:shadowing-import-from :rtg-math :m! :v!)
  (:import-from :jungl :def-compile-pass :def-deep-pass :set-uniform :remove-uniform
		:set-arg-val)
  (:export :get-transform :get-transform-via :p! :in :space! :make-space
	   :make-space*
	   :with-rendering-via
	   :*screen-space* :*ndc-space* :*clip-space* :*world-space*
	   :model-space-p :relational-space-p
	   :space :pos4 :space-g :pos4-g :let-model-space
	   :parent-space
	   :space-inverse-transform
	   :add-non-hierarchical-relationship
	   :update-non-hierarchical-relationship
	   :remove-non-hierarchical-relationship))

(defpackage :cepl
  (:use :cl
        :cepl-generics
        :rtg-math
        :rtg-math.base-maths
        :cl-fad
        :named-readtables
        :jungl)
  (:shadow :quit)
  (:import-from :jungl.space :p! :space-g :in)
  (:import-from :utils
                :deferror
                :print-mem
                :p->)
  (:import-from :rtg-math :s~)
  (:export :repl
           :quit
           :make-project
           ;;----
	   :dvec
	   :dvec*
	   ;;----
           :pos
           :rot
           :dir
           :vec
           :size
           :norm
           :tex
           :col
           ;;---
           :v! :v-x :v-y :v-z :v-w :s~
           :v!byte :v!ubyte :v!int
           ;;---
           :m!
           ;;---
           :rqpos
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
           :g->
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
           :calc-sampler-type
           :make-sampler
           :with-sampling
           :lod-bias
           :min-lod
           :max-lod
           :magnify-filter
           :minify-filter
           :set-minify-filter
           :calc-minify-filter
           :wrap
           :compare
           :with-texture-bound
           :g-pn
           :g-pc
           :g-pt
           :g-pnc
           :g-pnt
           :g-pntc
           :texref
           :map-g
           :fbo
           :make-fbo
           :make-fbos
           :with-bind-fbo
           :with-fbo-slots
           :fbo-attach
           :attachment
           :attachment-compatible
           :attachment-gpu-array
           :mode-rgb
           :mode-alpha
           :source-rgb
           :source-alpha
           :destination-rgb
           :destination-alpha
           :blending
           :make-blending-params
           :with-blending
           :per-attachment-blending-available-p
           :fbo-detach
           :viewport
           :current-viewport
	   :make-viewport
	   :clone-viewport
           :with-viewport
           :with-fbo-viewport
           ;;---
           :node
           :make-node
           :node-transform
           :make-pos-quat-node
           :pqn-pos
           :pqn-quat
           :make-axis-angle-node
           :aan-axis
           :aan-angle
           ;;---
           :make-ubo
           :ubo-data
           :ubo-index
           :cls
	   ;;---
	   :p!
	   :space-g
	   :in
	   ;;---
	   :continuable))
