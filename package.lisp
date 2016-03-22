;;;; package.lisp

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

(defpackage :cepl.generics
  (:use :cl)
  (:export :pos
           :rot
           :dir
           :vec
           :size
           :norm
           :tex
           :col
           :tangent
           :bi-tangent
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
           :timestamp
           ;;--
           :backed-by
           :dimensions
           :free
           :free-gpu-array
           :free-texture
           :lisp-type->pixel-format
           :make-gpu-array
           :make-vao-from-id
           :populate
           :pull-g
           :pull1-g
           :push-g))

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
	   :internal-format->lisp-type-failed
	   :lisp-type->internal-format-failed
	   :pixel-format->internal-format-failed
	   :internal-format->pixel-format-failed
	   :buffer-backed-texture-invalid-args
	   :buffer-backed-texture-invalid-samplers
	   :buffer-backed-texture-invalid-internal-format
	   :buffer-backed-texture-establish-internal-format
	   :failed-to-test-compile-gpu-func
	   :dont-define-space-to-self
	   :make-buffer-stream-with-no-gpu-arrays
	   :invalid-context-for-def-glsl-stage
	   :struct-in-glsl-stage-args))

(defpackage :cepl.internals
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
	:cepl.errors)
  (:export :%collate-args
           :%get-pipeline-uniforms
           :1d-p
           :clear-gl-context-cache
           :gl-assign-attrib-pointers
           :s-arrayp
           :s-def
           :s-extra-prim-p
           :s-prim-p
           :symbol-names-cepl-structp
           :uploadable-lisp-seq
           :*expanded-gl-type-names*
           :expand-gl-type-name
	   :color-attachment-enum
	   :gl-type-size))

(defpackage :cepl.context
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
	:cepl.errors)
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

(defpackage :cepl.render-state
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
	:cepl.errors)
  (:export))

(defpackage :cepl.viewports
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
	:cepl.errors)
  (:export :current-viewport
	   :viewport
	   :viewport-p
	   :viewport-resolution-x
	   :viewport-resolution-y
	   :viewport-origin-x
	   :viewport-origin-y
	   :make-viewport
	   :viewport-resolution
	   :viewport-resolution-v!
	   :viewport-origin
	   :with-viewport
	   :with-fbo-viewport
	   :clone-viewport
	   :viewport-params-to-vec4))

(defpackage :cepl.types
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
	:cepl.errors)
  (:export :defstruct-g))

(defpackage :cepl.space.routes
  (:use #:cl #:fn #:named-readtables #:cepl-utils
	:cepl.errors)
  (:export :id! :free-id :reset :get-route :map-route :reduce-route :add-id))

(defpackage :cepl.image-formats
  (:use #:cl #:fn #:named-readtables #:cepl-utils
	:cepl.errors)
  (:export :internal-formatp
	   :valid-internal-format-for-buffer-backed-texturep
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
	   :*valid-internal-formats-for-buffer-backed-texture*
	   :*image-formats*))

(defpackage :cepl.pixel-formats
  (:use #:cl #:fn #:named-readtables #:cepl-utils
	:cepl.errors :cepl.internals)
  (:export :pixel-format
	   :pixel-format-p
	   :pixel-format-components
	   :pixel-format-type
	   :pixel-format-normalise
	   :pixel-format-sizes
	   :pixel-format-reversed
	   :pixel-format-comp-length
	   :compile-pixel-format
	   :describe-internal-format
	   :describe-pixel-format
	   :get-component-length
	   :internal-format->lisp-type
	   :internal-format->pixel-format
	   :lisp-type->internal-format
	   :lisp-type->pixel-format
	   :pixel-format->internal-format
	   :pixel-format->lisp-type
	   :valid-pixel-format-p))

(defpackage :cepl.c-arrays
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables :cepl.errors
	:cepl.internals :cepl.image-formats)
  (:export :with-c-array
           :with-c-arrays
           :element-byte-size
           :element-type
           :pointer
           :aref-c
           :%aref-c
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
        :cepl.generics :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays
	:cepl.internals)
  (:export :with-buffer
           :gpu-buffer
	   :gpu-buffer-p
           :gpu-buffer-id
           :gpu-buffer-format
           :bind-buffer
           :buffer-data
           :buffer-data-raw
           :buffer-reserve-block
           :buffer-reserve-block-raw
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
        :cepl.generics :split-sequence :named-readtables :cepl.errors
	:cepl.internals :cepl.image-formats :cepl.c-arrays :cepl.gpu-buffers)
  (:export :gpu-array
	   :gpu-array-p
	   :gpu-array-buffer
	   :gpu-array-format
	   :gpu-array-dimensions
	   :gpu-array-access-style
	   :free-gpu-array
	   :make-gpu-array
	   :make-gpu-arrays
	   :subseq-g
	   :with-gpu-array-as-pointer
	   :with-gpu-array-as-c-array))

(defpackage :cepl.streams
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
	:cepl.errors :cepl.c-arrays)
  (:export :free-vao
	   :free-vaos
	   :bind-vao
	   :force-bind-vao
	   :suitable-array-for-index-p
	   :make-vao
	   :make-vao-from-id
	   ;;---
	   :buffer-stream
	   :buffer-stream-p
	   :buffer-stream-vao
	   :buffer-stream-length
	   :buffer-stream-index-type
	   :buffer-stream-gpu-arrays
	   :buffer-stream-managed
	   :free-buffer-stream
	   :make-buffer-stream
	   :make-buffer-stream-from-id))

(defpackage :cepl.ubos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
	:cepl.errors :cepl.c-arrays)
  (:export :ubo
	   :make-ubo
	   :make-ubo-from-array
	   :ubo-id
	   :ubo-data
	   :ubo-data-type
	   :ubo-index
	   :ubo-owns-gpu-array))

(defpackage :cepl.samplers
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays
	:cepl.internals)
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
	   :with-sampling
	   :*sampler-types*
	   :sampler-typep))

(defpackage :cepl.textures
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays
	:cepl.internals :cepl.samplers :cepl.pixel-formats)
  (:export :*immutable-available*
	   :*cube-face-order*
	   :gl-texture
	   :immutable-texture
	   :mutable-texture
	   :buffer-texture
	   :make-texture-from-id
	   :make-texture
	   :mutable-texturep
	   :free-texture
	   :generate-mipmaps
	   :dimensions-at-mipmap-level
	   :bind-texture
	   :unbind-texture
	   :with-texture-bound
	   :texref
	   ;;---
	   :gpu-array-t
	   :free-gpu-array
	   :free-gpu-array-t
	   :upload-c-array-to-gpu-array-t
	   :upload-from-buffer-to-gpu-array-t))

(defpackage :cepl.fbos
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays
	:cepl.internals :cepl.image-formats :cepl.textures)
  (:export))

(defpackage :cepl.blending
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
        :cepl.context :cepl.errors :cepl.c-arrays
	:cepl.internals :cepl.fbos)
  (:export :blending-params
	   :make-blending-params
	   :blending-params-mode-rgb
	   :blending-params-mode-alpha
	   :blending-params-source-rgb
	   :blending-params-source-alpha
	   :blending-params-destination-rgb
	   :blending-params-destination-alpha
	   :with-blending
	   :constant-alpha
	   :constant-color
	   :current-blend-params
	   :blend-func-namep))

(defpackage :cepl.pipelines
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
        :cepl.internals :cepl.c-arrays :cepl.gpu-buffers
        :cepl.context :cepl.types :cepl.errors)
  (:export :*verbose-compiles*
	   :defun-g
	   :undefine-gpu-function
	   :def-glsl-stage
	   :defmacro-g
	   :define-compiler-macro-g
	   :defpipeline
	   :with-instances
	   :g->
	   :map-g))

(defpackage :jungl
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
        :cepl.generics :split-sequence :named-readtables
        :cepl.internals :cepl.c-arrays :cepl.gpu-buffers
        :cepl.context :cepl.types :cepl.errors)
  (:shadowing-import-from :rtg-math :v!)
  (:import-from :cepl-utils
                :deferror
                :print-mem
                :just-ignore)
  (:shadow :float :space)
  (:export ;;- - - - - - - -
           :timestamp
           ;;- - - - - - - -
           :current-viewport
           :viewport
           :make-viewport
           :clone-viewport
           :with-viewport
           :with-fbo-viewport
           :viewport-resolution
           :viewport-resolution-v!

           :valid-pixel-format-p
           :pixel-format
           :pixel-format->lisp-type
           :pixel-format->internal-format
           :internal-format->pixel-format
           :internal-format->lisp-type
           :lisp-type->internal-format
           :describe-pixel-format
           :defstruct-g
           :gl-calc-byte-size


           :subseq-g

           :make-gpu-arrays
           :with-gpu-array-as-c-array
           :with-gpu-array-as-pointer
           :suitable-array-for-index-p
           :bind-vao
           :bind-vertex-array
           :make-vao-from-formats
           :make-vao
           :make-raw-buffer-stream
           :make-buffer-stream
           :make-texture
           :bind-texture
           :with-texture-bound
           :mutable-texturep
           :upload-c-array-to-gpu-array-t ; this is a crap name
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
           :def-glsl-stage
           :g->
           :defun-g
           :defmacro-g
           :define-compiler-macro-g
           :with-instances
           :free-managed-resources
           :free-buffer-stream
           :free-vao
           ;;----------
           :map-g
           ;;----------
           :make-fbo
           :make-fbos
           :with-fbo-bound
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
           :set-arg-val))

(defpackage :cepl.space
  (:use :cl :cepl-utils :rtg-math.types :rtg-math :named-readtables
        :varjo :varjo-lang :cepl.generics :cepl.errors)
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

(macrolet
    ((def-re-exporting-package (name &key use shadow export re-export
                                     import-from export-from)
       (labels ((exported-symbols (package-name)
                  (let ((package (find-package package-name)))
                    (loop :for x :being :each external-symbol :of package
                       :when (eq (symbol-package x) package) :collect
                       (intern (symbol-name x) :keyword))))
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
	    :cepl.errors)
      :shadow (:quit)
      :import-from ((:cepl-utils :deferror
                                 :print-mem
                                 :p->))
      :export-from ((:cepl.space :p! :space-g :in))
      :export (:g-pc
               :g-pn
               :g-pnc
               :g-pnt
               :g-pntc
               :g-pt
               :make-project
               :quit
               :repl
               :step-host
               :continuable
	       :cls
	       :swap
	       :print-mem)
      :re-export (:cepl.generics
                  :cepl.c-arrays
                  :cepl.gpu-buffers
                  :jungl
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
