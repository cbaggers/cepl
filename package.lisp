;;;; package.lisp

(defpackage :cepl-utils
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
           :with-hash
           :with-hash*
           :last1
           :p->
           :split-string
           :ni-call
           :ni-val
           :n-of
           :n-of*
           :just-ignore))

(defpackage :cepl.errors
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math)
  (:export :invalid-stages
	   :gfun-invalid-arg-format
	   :gpu-func-spec-not-found
	   :dispatch-called-outside-of-map-g
	   :invalid-keywords-for-shader-gpipe-args
	   :invalid-context-for-assert-gpipe
	   :invalid-context-for-assert-options
	   :invalid-shader-gpipe-form
	   :not-enough-args-for-implicit-gpipe-stages
	   :invalid-shader-gpipe-stage-keys
	   :invalid-compose-gpipe-form
	   :invalid-defpipeline-options
	   :shader-pipeline-non-null-args
	   :make-tex-no-content-no-type
	   :make-tex-array-not-match-type
	   :make-tex-array-not-match-type2
	   :image-format->lisp-type-failed
	   :lisp-type->image-format-failed
	   :pixel-format->image-format-failed
	   :image-format->pixel-format-failed
	   :buffer-backed-texture-invalid-args
	   :buffer-backed-texture-invalid-samplers
	   :buffer-backed-texture-invalid-image-format
	   :buffer-backed-texture-establish-image-format
	   :failed-to-test-compile-gpu-func
	   :dont-define-space-to-self
	   :make-buffer-stream-with-no-gpu-arrays
	   :invalid-context-for-def-glsl-stage
	   :struct-in-glsl-stage-args
	   :make-gpu-array-from-c-array-mismatched-dimensions))

(defpackage :cepl.host
  (:use :cl)
  (:export :init
           :request-context
           :shutdown
           :set-primary-thread-and-run
           ;;---
           :set-step-func
           :set-swap-func))

(defpackage :cepl.lifecycle
  (:use :cl)
  (:export :shutting-down-p
           :listen-to-lifecycle-changes
           :stop-listening-to-lifecycle-changes))

(defpackage :cepl.measurements
  (:use :cl)
  (:export :dimensions
	   :resolution))

(defpackage :%cepl.types
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :split-sequence :named-readtables
	:cepl.errors)
  (:export :%make-gpu-array
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
	   :gpu-array-bb-format-index
	   :gpu-array-bb-start
	   :gpu-array-bb-access-style
	   :+null-buffer-backed-gpu-array+

	   :%make-gpu-buffer
	   :make-uninitialized-gpu-buffer
	   :gpu-buffer
	   :gpu-buffer-p
	   :gpu-buffer-id
	   :gpu-buffer-format
	   :gpu-buffer-managed
	   :+null-gpu-buffer+

	   :%%make-texture
	   :%%make-buffer-texture
	   :make-uninitialized-texture
	   :texture
	   :texture-p
	   :texture-id
	   :texture-base-dimensions
	   :texture-type
	   :texture-image-format
	   :texture-sampler-type
	   :texture-mipmap-levels
	   :texture-layer-count
	   :texture-cubes-p
	   :texture-mutable-p
	   :texture-allocated-p
	   :texture-sampler-object-id
	   :buffer-texture
	   :buffer-texture-p
	   :buffer-texture-backing-array
	   :buffer-texture-owns-array
	   :+null-texture+

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
	   :%sampler-lod-bias
	   :%sampler-min-lod
	   :%sampler-max-lod
	   :%sampler-expects-mipmap
	   :%sampler-minify-filter
	   :%sampler-magnify-filter
	   :%sampler-wrap
	   :%sampler-expects-depth
	   :%sampler-compare

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
	   :%fbo-attachment-color
	   :%fbo-draw-buffer-map
	   :%fbo-attachment-depth
	   :%fbo-clear-mask
	   :%fbo-is-default
	   :%fbo-blending-params

	   :%make-attachment
	   :attachment
	   :attachment-p
	   :%attachment-fbo
	   :%attachment-gpu-array
	   :%attachment-owns-gpu-array
	   :%attachment-blending-enabled
	   :%attachment-override-blending
	   :%attachment-blending-params

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
	   :pixel-format-normalise
	   :pixel-format-sizes
	   :pixel-format-reversed
	   :pixel-format-comp-length

	   :make-raw-buffer-stream
	   :make-uninitialized-buffer-stream
	   :buffer-stream
	   :buffer-stream-p
	   :buffer-stream-vao
	   :buffer-stream-start
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

(defpackage :cepl.memory
  (:use :cl :cffi :%cepl.types)
  (:export :free
	   :initialized-p
	   ;;---
	   :pull-g
           :pull1-g
           :push-g
	   ))

(defpackage :cepl.types
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
	   :element-byte-size))

(defpackage :cepl.types.predefined
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

(defpackage :cepl.internals
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
           :expand-gl-type-name
	   :color-attachment-enum
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
	   :%current-fbo
	   :*gl-window*
	   :*on-context*))

(defpackage :cepl.render-state
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors)
  (:export))

(defpackage :cepl.viewports
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

(defpackage :cepl.context
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors :cepl.internals)
  (:export :gl-context
           :*gl-context*
           :make-context
           :has-feature
           :major-version
           :minor-version
           :version-float
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
           ;; :%array-buffer-binding
           ;; :%read-buffer-binding
           ;; :%copy-write-buffer-binding
           ;; :%draw-indirect-buffer-binding
           ;; :%element-array-buffer-binding
           ;; :%query-buffer-binding
           ;; :%texture-buffer-binding
           ;; :%vertex-array-binding
           ))

(defpackage :cepl.image-formats
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

(defpackage :cepl.pixel-formats
  (:use #:cl #:fn #:named-readtables #:cepl-utils :%cepl.types
	:cepl.types :cepl.errors :cepl.internals)
  (:export :pixel-format
	   :pixel-format-p
	   :pixel-format-components
	   :pixel-format-type
	   :pixel-format-normalise
	   :pixel-format-sizes
	   :pixel-format-reversed
	   :pixel-format-comp-length))

(defpackage :cepl.c-arrays
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors :cepl.internals :cepl.image-formats
	:cepl.pixel-formats :cepl.memory :cepl.measurements)
  (:export :with-c-array
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
           :subseq-c))

(defpackage :cepl.gpu-buffers
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays :%cepl.types
	:cepl.internals :cepl.memory)
  (:export :with-buffer
           :gpu-buffer
	   :gpu-buffer-p
           :gpu-buffer-id
           :gpu-buffer-format
           :bind-buffer
           :buffer-data
           :buffer-data-raw
           :buffer-reserve-block
           :buffer-reserve-blocks
           :buffer-sub-data
           :free-buffer
           :free-buffers
           :make-gpu-buffer
           :make-gpu-buffer-from-id
           :multi-buffer-data
           :unbind-buffer))

(defpackage :cepl.gpu-arrays.buffer-backed
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
	   :subseq-g))

(defpackage :cepl.vaos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
	:named-readtables :cepl.errors :cepl.c-arrays :cepl.internals
	:cepl.gpu-buffers :cepl.gpu-arrays.buffer-backed)
  (:export :free-vao
	   :free-vaos
	   :bind-vao
	   :unbind-vao
	   :with-vao-bound
	   :make-vao
	   :make-vao-from-id))

(defpackage :cepl.streams
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

(defpackage :cepl.ubos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :%cepl.types :split-sequence
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

(defpackage :cepl.textures
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
	   :*immutable-available*

	   :lod-bias
	   :min-lod
	   :max-lod
	   :magnify-filter
	   :minify-filter
	   :set-minify-filter
	   :wrap
	   :compare))

(defpackage :cepl.gpu-arrays.texture-backed
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables :cepl.errors
	:cepl.internals :cepl.image-formats :cepl.c-arrays :cepl.gpu-buffers
	:cepl.textures :%cepl.types :cepl.memory)
  (:export :gpu-array-texture
	   :gpu-array-texture-type
	   :gpu-array-level-num
	   :gpu-array-layer-num
	   :gpu-array-face-num))

(defpackage :cepl.gpu-arrays
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

(defpackage :cepl.samplers
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays
	:cepl.internals :%cepl.types :cepl.memory)
  (:export :sampler
	   :sampler-p
	   :make-sampler
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
	   :with-sampling))

(defpackage :cepl.fbos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.types :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays :%cepl.types
	:cepl.internals :cepl.image-formats :cepl.textures
	:cepl.viewports :cepl.measurements :cepl.memory)
  (:export :fbo
	   :fbo-p
	   :fbo-blending-params
	   :attachment
	   :attachment-p
	   :attachment-viewport
	   :attachment-gpu-array
	   :per-attachment-blending-available-p
	   :attachment
	   :make-fbo-from-id
	   :make-fbo
	   :check-framebuffer-status
	   :with-fbo-bound
	   :clear
	   :clear-fbo
	   :clear-attachment))

(defpackage :cepl.blending
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

(defpackage :cepl.pipelines
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
	   :defpipeline
	   :with-instances
	   :g->
	   :map-g
	   ;; :*verbose-compiles*
	   ;; :*warn-when-cant-test-compile*
	   ))

(defpackage :cepl.space.routes
  (:use #:cl #:fn #:named-readtables #:cepl-utils
	:cepl.errors)
  (:export :id! :free-id :reset :get-route :map-route :reduce-route :add-id))

(defpackage :cepl.space
  (:use :cl :cepl-utils :rtg-math.types :rtg-math :named-readtables
	:varjo :varjo-lang :cepl.types :cepl.errors
	:cepl.internals :cepl.pipelines :cepl.memory)
  (:shadowing-import-from :rtg-math :m! :v!)
  (:export :vec-space :make-space :make-space*
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
	   `(defpackage ,name
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
			       :p->))
    :export (:make-project
	     :quit
	     :repl
	     :step-host
	     :continuable
	     :cls
	     :swap
	     :print-mem)
    :re-export (:cepl.render-state
		:cepl.viewports
		:cepl.types
		:cepl.memory
		:cepl.image-formats
		:cepl.pixel-formats
		:cepl.c-arrays
		:cepl.gpu-buffers
		:cepl.gpu-arrays.buffer-backed
		:cepl.gpu-arrays
		:cepl.streams
		:cepl.ubos
		:cepl.samplers
		:cepl.textures
		:cepl.fbos
		:cepl.blending
		:cepl.pipelines
		:cepl.space
		(:cepl.lifecycle :shutting-down-p)
		(:rtg-math :q! :m! :v! :v!byte :v!ubyte :v!int :s~
			   :radians :degrees))))




;; {TODO} read up on this:
;; com.informatimago.common-lisp.lisp-reader.package:defpackage
;;
;; re-evaluating defpackage has implementation specific effects,
;; advice I got from pjb was to implement the ugliness above with
;; 'export directly. I need to understand this approach but could
;; be nice.
