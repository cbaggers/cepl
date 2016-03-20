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
           :timestamp))

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

(defpackage :jungl.space.routes
  (:use #:cl #:fn #:named-readtables #:cepl-utils)
  (:export :id! :free-id :reset :get-route :map-route :reduce-route :add-id))

(defpackage :jungl
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :rtg-math
	:cepl-generics :split-sequence :named-readtables)
  (:shadowing-import-from :rtg-math :v!)
  (:import-from :cepl-utils
                :deferror
                :print-mem
		:just-ignore)
  (:shadow :float :space)
  (:export :cls
           :gl-context
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
           :swap
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
           :gl-calc-byte-size

	   :pointer
	   :dimensions ; [TODO] this isnt really inline with array-dimensions
	   :element-type

	   :c-array-pointer
	   :c-array-dimensions
	   :c-array-element-type

           :make-c-array-from-pointer
           :with-c-array
           :with-c-arrays
           :free-c-array
           :clone-c-array
           :make-c-array

	   :c-array-byte-size


           :aref-c
           :%aref-c
           :c-populate
           :subseq-g
           :subseq-c
           :pull-g
           :pull1-g
           :push-g

           :backed-by ; [TODO] is this the right name for the job?

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
	   :def-glsl-stage
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
	    :named-readtables)
      :shadow (:quit)
      :import-from ((:cepl-utils :deferror
				  :print-mem
				  :p->))
      :export-from ((:jungl.space :p! :space-g :in))
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
	       :continuable)
      :re-export (:cepl-generics
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
