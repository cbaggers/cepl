;;;; package.lisp

(uiop:define-package :cepl-utils
  (:use :cl)
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
	   :filter-hash
	   :assocr
           :with-hash
           :with-hash*
           :last1
           :p->
           :split-string
           :ni-call
           :ni-val
           :n-of
           :n-of*
           :just-ignore
	   :defvar*
	   :defparameter*
	   :read-integers
           :ensure-vec-index
           :def-artificial-id))

(uiop:define-package :cepl.errors
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math)
  (:export :buffer-backed-texture-establish-image-format
	   :buffer-backed-texture-invalid-args
	   :buffer-backed-texture-invalid-image-format
	   :buffer-backed-texture-invalid-samplers
	   :delete-multi-func-error
	   :dispatch-called-outside-of-map-g
	   :dont-define-space-to-self
	   :failed-to-test-compile-gpu-func
	   :gfun-invalid-arg-format
	   :glsl-version-conflict
	   :glsl-version-conflict-in-gpu-func
	   :gpu-func-spec-not-found
	   :image-format->lisp-type-failed
	   :image-format->pixel-format-failed
	   :invalid-compose-gpipe-form
	   :invalid-context-for-assert-gpipe
	   :invalid-context-for-assert-options
	   :invalid-context-for-def-glsl-stage
	   :invalid-contnext-for-assert-gpipe
	   :invalid-defpipeline-options
	   :invalid-keywords-for-shader-gpipe-args
	   :invalid-shader-gpipe-form
	   :invalid-shader-gpipe-stage-keys
	   :lisp-type->image-format-failed
	   :make-buffer-stream-with-no-gpu-arrays
	   :make-gpu-array-from-c-array-mismatched-dimensions
	   :make-tex-array-not-match-type
	   :make-tex-array-not-match-type2
	   :make-tex-no-content-no-type
	   :symbol-stage-designator
           :symbol-stage-designators
	   :not-enough-args-for-implicit-gpipe-stages
	   :pixel-format->image-format-failed
	   :pixel-format-in-bb-texture
	   :shader-pipeline-non-null-args
	   :stage-not-found
	   :struct-in-glsl-stage-args
	   :multi-func-error
	   :attachments-with-different-sizes
	   :invalid-cube-fbo-args
           :functions-in-non-uniform-args
           :mapping-over-partial-pipeline))

(uiop:define-package :cepl.host
  (:use :cl)
  (:export :init
           :request-context
           :shutdown
           :window-size
           :set-primary-thread-and-run
           ;;---
           :set-step-func
           :set-swap-func
           :set-window-size-func))

(uiop:define-package :cepl.lifecycle
  (:use :cl)
  (:export :shutting-down-p
           :listen-to-lifecycle-changes
           :stop-listening-to-lifecycle-changes))

(uiop:define-package :cepl.measurements
  (:use :cl)
  (:export :dimensions
	   :resolution))

(uiop:define-package :%cepl.types
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :split-sequence :named-readtables
	:cepl.errors)
  (:export :+gl-id-bit-size+
           :gl-id
           :+unknown-gl-id+
           :+null-gl-id+
           :unknown-gl-id-p

           :%make-gpu-array
	   :gpu-array
	   :gpu-array-p
	   :gpu-array-dimensions

	   :%make-gpu-array-t
	   :make-uninitialized-gpu-array-t
	   :gpu-array-t
	   :gpu-array-t-p
	   :gpu-array-t-texture
	   :gpu-array-t-texture-type
	   :gpu-array-t-level-num
	   :gpu-array-t-layer-num
	   :gpu-array-t-face-num
	   :gpu-array-t-image-format
	   :+null-texture-backed-gpu-array+

	   :%make-gpu-array-bb
	   :make-uninitialized-gpu-array-bb
	   :gpu-array-bb
	   :gpu-array-bb-p
	   :gpu-array-bb-buffer
	   :gpu-array-bb-access-style
	   :gpu-array-bb-element-type
	   :gpu-array-bb-byte-size
	   :gpu-array-bb-offset-in-bytes-into-buffer
	   :+null-buffer-backed-gpu-array+

	   :%make-gpu-buffer
	   :make-uninitialized-gpu-buffer
	   :gpu-buffer
	   :gpu-buffer-p
	   :gpu-buffer-id
           :gpu-buffer-cache-id
	   :gpu-buffer-arrays
	   :gpu-buffer-managed
	   :+null-gpu-buffer+

	   :%%make-texture
	   :%%make-buffer-texture
	   :make-uninitialized-texture
	   :texture
	   :texture-p
	   :texture-id
           :texture-cache-id
	   :texture-base-dimensions
	   :texture-type
	   :texture-last-sampler-id
	   :texture-image-format
	   :texture-mipmap-levels
	   :texture-layer-count
	   :texture-cubes-p
	   :texture-mutable-p
	   :texture-allocated-p
	   :buffer-texture
	   :buffer-texture-p
	   :buffer-texture-backing-array
	   :buffer-texture-owns-array
	   :+null-texture+

           :vao-id
           :+null-vao+

	   :make-blending-params
	   :blending-params
	   :blending-params-p
	   :blending-params-mode-rgb
	   :blending-params-mode-alpha
	   :blending-params-source-rgb
	   :blending-params-source-alpha
	   :blending-params-destination-rgb
	   :blending-params-destination-alpha
	   :copy-blending-params

	   :%make-sampler
	   :make-uninitialized-sampler
	   :sampler
	   :sampler-p
	   :%sampler-id
	   :%sampler-id-box
           :%sampler-context-id
	   :%sampler-type
	   :%sampler-texture
	   :%sampler-lod-bias
	   :%sampler-min-lod
	   :%sampler-max-lod
	   :%sampler-expects-mipmap
	   :%sampler-minify-filter
	   :%sampler-magnify-filter
	   :%sampler-wrap
	   :%sampler-expects-depth
	   :%sampler-compare

	   :sampler-id-box
           :sampler-shared-p
	   :make-sampler-id-box
	   :sampler-id-box-id
	   :sampler-id-box-shared-p

	   :lod-bias
	   :min-lod
	   :max-lod
	   :magnify-filter
	   :minify-filter
	   :set-minify-filter
	   :wrap
	   :compare

	   :%%make-fbo
	   :make-uninitialized-fbo
	   :fbo
	   :fbo-p
	   :%fbo-id
	   :%fbo-color-arrays
	   :%fbo-depth-array
	   :%fbo-draw-buffer-map
	   :%fbo-clear-mask
	   :%fbo-is-default
	   :%fbo-blending-params
           :+null-fbo+
	   :make-att
	   :att
	   :att-array
	   :att-blend
	   :att-bparams
	   :att-owned-p

	   :%make-ubo
	   :ubo
	   :ubo-p
	   :ubo-id
	   :ubo-data
	   :ubo-index
	   :ubo-owns-gpu-array

	   :make-pixel-format
	   :pixel-format
	   :pixel-format-p
	   :pixel-format-components
	   :pixel-format-type
	   :pixel-format-normalize
	   :pixel-format-sizes
	   :pixel-format-reversed
	   :pixel-format-comp-length

	   :make-raw-buffer-stream
	   :make-uninitialized-buffer-stream
	   :buffer-stream
	   :buffer-stream-p
	   :buffer-stream-vao
	   :buffer-stream-start
           :buffer-stream-start-byte
	   :buffer-stream-length
	   :buffer-stream-index-type
	   :buffer-stream-gpu-arrays
	   :buffer-stream-managed

	   :%make-c-array
	   :c-array
	   :c-array-p
           :c-array-pointer
           :c-array-dimensions
           :c-array-element-type
	   :c-array-element-type
	   :c-array-element-byte-size
	   :c-array-row-byte-size
	   :c-array-struct-element-typep
	   :c-array-element-pixel-format
	   :c-array-element-from-foreign
	   :c-array-element-to-foreign

	   :%make-viewport
	   :viewport
	   :viewport-p
	   :copy-viewport
	   :%viewport-resolution-x
	   :%viewport-resolution-y
	   :%viewport-origin-x
	   :%viewport-origin-y

	   ;;---
	   :holds-gl-object-ref-p))

(uiop:define-package :cepl.memory
  (:use :cl :cffi :%cepl.types)
  (:export :free
	   :initialized-p
	   ;;---
	   :pull-g
           :pull1-g
           :push-g))

(uiop:define-package :cepl.types.foreign
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :split-sequence :named-readtables
	:cepl.errors :%cepl.types :cepl.memory))

(uiop:define-package :cepl.types
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :split-sequence :named-readtables
	:cepl.errors :%cepl.types :cepl.memory)
  (:export :defstruct-g
	   :lisp-type->pixel-format
	   :image-format->lisp-type
	   :image-format->pixel-format
	   :lisp-type->image-format
	   :pixel-format->image-format
	   :pixel-format->lisp-type
	   ;;---
	   :element-type
	   :element-byte-size
	   ;;---
	   :get-typed-from-foreign
	   :get-typed-to-foreign))

(uiop:define-package :cepl.types.predefined
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :split-sequence :named-readtables :cepl.types
	:cepl.errors :%cepl.types :cepl.memory)
  (:export :g-pc
	   :g-pn
	   :g-pnc
	   :g-pnt
	   :g-pntc
	   :g-pt
	   ;;---
	   :bi-tangent
	   :col
	   :norm
	   :pos
	   :tangent
	   :tex
	   ;;---
	   :g-pn-position
	   :g-pn-normal
	   :g-pc-position
	   :g-pc-color
	   :g-pt-position
	   :g-pt-texture
	   :g-pnc-position
	   :g-pnc-normal
	   :g-pnc-color
	   :g-pnt-position
	   :g-pnt-normal
	   :g-pnt-texture
	   :g-pntc-position
	   :g-pntc-normal
	   :g-pntc-texture
	   :g-pntc-color
	   :g-pnb-position
	   :g-pnb-normal
	   :g-pnb-tangent
	   :g-pnb-bi-tangent
	   :g-pncb-position
	   :g-pncb-normal
	   :g-pncb-tangent
	   :g-pncb-bi-tangent
	   :g-pncb-color
	   :g-pntb-position
	   :g-pntb-normal
	   :g-pntb-tangent
	   :g-pntb-bi-tangent
	   :g-pntb-texture
	   :g-pntcb-position
	   :g-pntcb-normal
	   :g-pntcb-tangent
	   :g-pntcb-bi-tangent
	   :g-pntcb-texture
	   :g-pntcb-color))

(uiop:define-package :cepl.internals
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors :cepl.measurements)
  (:export :1d-p
           :clear-gl-context-cache
           :gl-assign-attrib-pointers
           :s-arrayp
           :s-def
           :s-extra-prim-p
           :s-prim-p
           :symbol-names-cepl-structp
           :*expanded-gl-type-names*
           :cffi-type->gl-type
	   :color-attachment-enum
	   :draw-buffer-enum
	   :gl-type-size
	   :uploadable-lisp-seq
	   ;;---
	   :def-compile-pass
	   :def-deep-pass
	   :set-uniform
	   :remove-uniform
	   :set-arg-val
	   ;;---
	   :populate
	   :%default-framebuffer
	   :*gl-window*
           :window-dimensions
           :window-resolution
	   :*on-context*))

(uiop:define-package :cepl.render-state
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors)
  (:export))

(uiop:define-package :cepl.viewports
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence :cepl.measurements
	:named-readtables :cepl.errors :cepl.internals)
  (:export :current-viewport
	   :viewport
	   :viewport-p
	   :viewport-resolution-x
	   :viewport-resolution-y
	   :viewport-origin-x
	   :viewport-origin-y
	   :make-viewport
	   :viewport-dimensions
	   :viewport-resolution
	   :viewport-origin
	   :with-viewport
	   :with-fbo-viewport
	   :copy-viewport
	   :viewport-params-to-vec4))

(uiop:define-package :cepl.context
    (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
          :cepl.memory :cepl.types :%cepl.types :split-sequence
          :named-readtables :cepl.errors :cepl.internals)
  (:export :gl-context
           :*gl-context*
           :make-context
           :has-feature
           :major-version
           :minor-version
           :version-float
           :split-float-version
           :max-server-wait-timeout
           :min-map-buffer-alignment
           :extension-count
           :supported-shading-versions-count
           :timestamp
           :color-clear-value
           :color-writemask
           :depth-clear-value
           :depth-func~1
           :depth-test
           :depth-writemask
           :doublebuffer
           :draw-buffer
           :draw-buffer-i
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
           :array-buffer-binding
           :element-array-buffer-binding
           :uniform-buffer-binding
           :vertex-array-binding
           :read-framebuffer-binding
           :draw-framebuffer-binding
           :framebuffer-binding
           ;; :%array-buffer-binding
           ;; :%read-buffer-binding
           ;; :%copy-write-buffer-binding
           ;; :%draw-indirect-buffer-binding
           ;; :%query-buffer-binding
           ;; :%texture-buffer-binding

           ;;----------------------------
           ;; CEPL.Context
           :*cepl-context*
           :gpu-buffer-bound
           :texture-bound
           :vao-bound
           :read-fbo-bound
           :draw-fbo-bound
           :fbo-bound))

(uiop:define-package :cepl.image-formats
  (:use #:cl #:fn #:named-readtables #:cepl-utils :%cepl.types :cepl.errors)
  (:export :image-formatp
	   :valid-image-format-for-buffer-backed-texturep
	   :color-renderable-formatp
	   :depth-formatp
	   :stencil-formatp
	   :depth-stencil-formatp
	   :*unsigned-normalized-integer-formats*
	   :*signed-normalized-integer-formats*
	   :*signed-integral-formats*
	   :*unsigned-integral-formats*
	   :*floating-point-formats*
	   :*regular-color-formats*
	   :*special-color-formats*
	   :*srgb-color-formats*
	   :*red/green-compressed-formats*
	   :*bptc-compressed-formats*
	   :*s3tc/dxt-compessed-formats*
	   :*depth-formats*
	   :*stencil-formats*
	   :*depth-stencil-formats*
	   :*color-renderable-formats*
	   :*valid-image-formats-for-buffer-backed-texture*
	   :*image-formats*))

(uiop:define-package :cepl.pixel-formats
  (:use #:cl #:fn #:named-readtables #:cepl-utils :%cepl.types
	:cepl.types :cepl.errors :cepl.internals)
  (:export :pixel-format
	   :pixel-format-p
	   :pixel-format-components
	   :pixel-format-type
	   :pixel-format-normalize
	   :pixel-format-sizes
	   :pixel-format-reversed
	   :pixel-format-comp-length))

(uiop:define-package :cepl.c-arrays
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors :cepl.internals :cepl.image-formats
	:cepl.pixel-formats :cepl.memory :cepl.measurements)
  (:export :with-c-array
	   :with-c-arrays
           :element-byte-size
           :element-type
           :pointer
           :aref-c
           :aref-c*
           :c-array
	   :c-array-p
           :c-array-pointer
           :c-array-dimensions
           :c-array-element-type
           :clone-c-array
           :free-c-array
           :make-c-array
           :make-c-array-from-pointer
           :subseq-c
	   :across-c-ptr
	   :map-c-into
	   :map-c
	   :ptr-index))

(uiop:define-package :cepl.gpu-buffers
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays :%cepl.types
	:cepl.internals :cepl.memory)
  (:export :with-buffer
           :gpu-buffer
	   :gpu-buffer-p
           :gpu-buffer-id
           :gpu-buffer-arrays
           :buffer-data
           :buffer-data-raw
           :buffer-reserve-block
           :free-buffer
           :free-buffers
           :make-gpu-buffer
           :make-gpu-buffer-from-id
           :multi-buffer-data))

(uiop:define-package :cepl.gpu-arrays.buffer-backed
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
	:cepl.errors :%cepl.types :cepl.internals :cepl.image-formats
	:cepl.c-arrays :cepl.gpu-buffers :cepl.memory :cepl.measurements)
  (:export :gpu-array-buffer
	   :gpu-array-format
	   :gpu-array-access-style
	   :free-gpu-array
	   :make-gpu-array
	   :make-gpu-arrays
	   :subseq-g
	   :subseq-g-raw))

(uiop:define-package :cepl.vaos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence :cepl.context
	:named-readtables :cepl.errors :cepl.c-arrays :cepl.internals
	:cepl.gpu-buffers :cepl.gpu-arrays.buffer-backed)
  (:export :free-vao
	   :free-vaos
	   :with-vao-bound
	   :make-vao
	   :make-vao-from-id))

(uiop:define-package :cepl.streams
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors :cepl.c-arrays :cepl.internals
	:cepl.gpu-buffers :cepl.gpu-arrays.buffer-backed :cepl.vaos
	:cepl.measurements :cepl.memory)
  (:export :buffer-stream
	   :buffer-stream-p
	   :buffer-stream-vao
	   :buffer-stream-length
	   :buffer-stream-index-type
	   :buffer-stream-gpu-arrays
	   :free-buffer-stream
	   :make-buffer-stream))

(uiop:define-package :cepl.ubos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence :cepl.context
	:named-readtables :cepl.errors :cepl.c-arrays :cepl.memory
	:cepl.gpu-arrays.buffer-backed :cepl.internals :cepl.gpu-buffers)
  (:export :ubo
	   :make-ubo
	   :make-ubo-from-array
	   :ubo-id
	   :ubo-data
	   :ubo-data-type
	   :ubo-index
	   :ubo-owns-gpu-array))

(uiop:define-package :cepl.textures
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays :%cepl.types
	:cepl.gpu-arrays.buffer-backed :cepl.internals :cepl.pixel-formats
	:cepl.image-formats :cepl.gpu-buffers :cepl.measurements
	:cepl.memory)
  (:export :texture
	   :texture-p
	   :texture-id
	   :texture-base-dimensions
	   :texture-type
	   :texture-element-type
	   :texture-sampler-type
	   :texture-mipmap-levels
	   :texture-layer-count
	   :texture-cubes-p
	   :texture-mutable-p
	   :buffer-texture
	   :buffer-texture-p

	   :make-texture-from-id
	   :make-texture
	   :free-texture
	   :generate-mipmaps
	   :with-texture-bound
	   :texref
	   :*immutable-available*))

(uiop:define-package :cepl.gpu-arrays.texture-backed
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables :cepl.errors
	:cepl.internals :cepl.image-formats :cepl.c-arrays :cepl.gpu-buffers
	:cepl.textures :%cepl.types :cepl.memory)
  (:export :gpu-array-texture
	   :gpu-array-texture-type
	   :gpu-array-level-num
	   :gpu-array-layer-num
	   :gpu-array-face-num))

(uiop:define-package :cepl.gpu-arrays
  ;; a place to put things that cross both kinds of gpu-array
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables :cepl.errors
	:cepl.internals :cepl.image-formats :cepl.c-arrays :cepl.gpu-buffers
	:cepl.gpu-arrays.buffer-backed :cepl.gpu-arrays.texture-backed
	:cepl.textures :%cepl.types :cepl.measurements :cepl.memory)
  (:export :gpu-array
	   :gpu-array-p
	   :backed-by
	   :gpu-array-dimensions
	   :gpu-array-buffer
	   :gpu-array-format
	   :gpu-array-access-style
	   :gpu-array-texture-type
	   :gpu-array-level-num
	   :gpu-array-layer-num
	   :gpu-array-face-num
	   :gpu-array-element-type
	   :free-gpu-array
	   :make-gpu-array
	   :make-gpu-arrays
	   :subseq-g
	   :with-gpu-array-as-pointer
	   :with-gpu-array-as-c-array))

(uiop:define-package :cepl.samplers
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays
	:cepl.internals :%cepl.types :cepl.memory)
  (:export :sampler
	   :sampler-p
	   :sampler-texture
	   :sample
	   ;; :sampler-id
	   ;; :sampler-lod-bias
	   ;; :sampler-min-lod
	   ;; :sampler-max-lod
	   ;; :sampler-expects-mipmap
	   ;; :sampler-minify-filter
	   ;; :sampler-magnify-filter
	   ;; :sampler-wrap
	   ;; :sampler-expects-depth
	   ;; :sampler-compare
	   :lod-bias
	   :min-lod
	   :max-lod
	   :magnify-filter
	   :minify-filter
	   :set-minify-filter
	   :wrap
	   :compare
           :free-sampler
           :with-sampling))

(uiop:define-package :cepl.fbos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays :%cepl.types
	:cepl.internals :cepl.image-formats :cepl.textures
	:cepl.viewports :cepl.measurements :cepl.memory)
  (:export :fbo
	   :fbo-p
	   :fbo-blending-params
	   :attachment
	   :attachment-viewport
	   :attachment-blending
	   :per-attachment-blending-available-p
	   :attachment-tex
	   :make-fbo-from-id
	   :make-fbo
	   :check-framebuffer-status
	   :with-fbo-bound
	   :clear
	   :clear-fbo
	   :clear-attachment))

(uiop:define-package :cepl.blending
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays :%cepl.types
	:cepl.internals :cepl.fbos)
  (:export :blending-params
	   :blending-params-p
	   :copy-blending-params
	   :make-blending-params
	   :with-blending
	   ;;--
	   :mode-rgb
	   :mode-alpha
	   :source-rgb
	   :source-alpha
	   :destination-rgb
	   :destination-alpha))

(uiop:define-package :cepl.pipelines
  (:use :cl :cffi :varjo :varjo-lang :rtg-math :split-sequence :named-readtables
        :cepl-utils :cepl.errors :%cepl.types :cepl.types
	:cepl.internals :cepl.render-state :cepl.viewports :cepl.context
	:cepl.image-formats :cepl.pixel-formats :cepl.c-arrays :cepl.gpu-buffers
	:cepl.gpu-arrays.buffer-backed :cepl.vaos :cepl.streams :cepl.ubos
	:cepl.textures :cepl.gpu-arrays.texture-backed :cepl.gpu-arrays
	:cepl.samplers :cepl.fbos :cepl.blending :cepl.memory)
  (:export :defun-g
	   :def-glsl-stage
	   :defmacro-g
	   :define-compiler-macro-g
	   :with-instances
           :glambda
	   :def-g->
	   :g->
	   :map-g
	   :map-g-into
	   :gpu-function
	   :gpu-functions
	   :delete-gpu-function
           :bake
	   ;; :*verbose-compiles*
	   ;; :*warn-when-cant-test-compile*
	   ))

(uiop:define-package :cepl.space.routes
  (:use #:cl #:fn #:named-readtables #:cepl-utils
	:cepl.errors)
  (:export :id! :free-id :reset :get-route :map-route :reduce-route :add-id))

(uiop:define-package :cepl.space
  (:use :cl :cepl-utils :rtg-math.types :rtg-math :named-readtables
	:varjo :varjo-lang :cepl.types :cepl.errors
	:cepl.internals :cepl.pipelines :cepl.memory)
  (:shadowing-import-from :rtg-math :m! :v!)
  (:export :space :vec-space :make-space :make-space*
	   :parent-space :model-space-p :relational-space-p
	   :get-transform :get-transform-via
	   :with-space-routing-via :in
	   :*screen-space* :*ndc-space* :*clip-space* :*world-space*
	   :sv! :svec4))

(macrolet
    ((def-re-exporting-package (name &key use shadow export re-export
				     import-from export-from)
       (labels ((exported-symbols (package-name)
				  (let ((package (find-package package-name))
					result)
				    (do-external-symbols (x package)
							 (push (intern (symbol-name x) :keyword) result))
				    result))
		(calc-export-all (re)
				 (exported-symbols re))
		(calc-import-from (re)
				  (rest re))
		(calc-re-export (re)
				(typecase re
				  (list (calc-import-from re))
				  (symbol (calc-export-all re))))
		(calc-exports-from (ef)
				   (rest ef))
		(calc-exports ()
			      (append export (mapcan #'calc-re-export re-export)
				      (mapcan #'calc-exports-from export-from)))
		(calc-re-using (x)
			       (if (listp x) (first x) x)))
	 (let ((use (append use (mapcar #'calc-re-using re-export)))
	       (exports (calc-exports)))
	   `(uiop:define-package ,name
	      ,@(when use `((:use ,@use)))
	      ,@(when shadow `((:shadow ,@shadow)))
	      ,@(loop :for i :in import-from :collect (cons :import-from i))
	      ,@(loop :for i :in export-from :collect (cons :import-from i))
	      ,@(when exports `((:export ,@exports))))))))
  ;;
  (def-re-exporting-package :cepl
    :use (:cl
	  :rtg-math.base-maths
	  :cl-fad
	  :named-readtables
	  :cepl.errors
	  :cepl.internals)
    :shadow (:quit)
    :import-from ((:cepl-utils :deferror
			       :print-mem
			       :p->
			       :defvar*
			       :defparameter*))
    :export (:make-project
	     :quit
	     :repl
	     :step-host
	     :continuable
	     :cls
	     :swap
	     :print-mem
	     :defvar*
	     :defparameter*)
    :re-export (:cepl.render-state
		:cepl.viewports
		:cepl.types
		:cepl.memory
		:cepl.measurements
		:cepl.image-formats
		:cepl.pixel-formats
		:cepl.c-arrays
		:cepl.gpu-buffers
		:cepl.gpu-arrays.buffer-backed
		:cepl.gpu-arrays.texture-backed
		:cepl.gpu-arrays
		:cepl.streams
		:cepl.ubos
		:cepl.samplers
		:cepl.textures
		:cepl.fbos
		:cepl.blending
		:cepl.pipelines
		:cepl.space
		:cepl.types.predefined
		(:cepl.lifecycle :shutting-down-p)
		(:rtg-math :q! :m! :v! :v!byte :v!ubyte :v!int :s~
			   :radians :degrees))))

(uiop:define-package :cepl.misc
  (:use :cl :cepl :rtg-math.base-maths :cl-fad :named-readtables)
  (:export :draw-texture :draw-colored-quad))
