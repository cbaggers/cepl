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
	   :with-hash
	   :with-hash*
           :last1
           :p->))

(defpackage :%jungl
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :cl-game-math.base-vectors :cepl-generics
        :split-sequence :named-readtables)
  (:shadowing-import-from :cl-game-math.base-vectors :v!))

(defpackage :jungl
  (:use :cl :cffi :cepl-utils :varjo :varjo-lang :cl-game-math.base-vectors :cepl-generics
        :split-sequence :%jungl :named-readtables)
  (:shadowing-import-from :cl-game-math.base-vectors :v!)
  (:import-from :utils
                :deferror
                :print-mem)
  (:shadow :float)
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
           :with-viewport
           :with-fbo-viewport
           :viewport-resolution
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
           :def-equivalent-type
           ;;----------
           :clear
	   ;;----------
	   :def-compile-pass
	   :set-uniform
	   :remove-uniform
	   :set-arg-val))

(defpackage :varjo-bridge-types
  (:use :cl))

(defpackage :cepl-camera
  (:nicknames :ccam)
  (:use :cl :cepl-generics :cl-game-math.base-vectors)
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
  (:import-from :cl-game-math.vector2
                :make-vector2)
  (:import-from :cl-game-math.vector3
                :make-vector3)
  (:import-from :cl-game-math.vector4
                :make-vector4))

(defpackage :tools
  (:use :cl
        :cl-game-math.base-vectors
        :cl-game-math.base-matrices
        :cl-game-math.base-maths)
  (:export :rqpos))

(defpackage :cepl.events
  (:use :cl :cepl-utils :cepl-generics :skitter :defstruct-plus-methods)
  (:nicknames :evt)
  (:export
   ;; from-skitter
   :subscribe
   :unsubscribe
   :unsubscribe-from-all
   :push-event
   :def-named-event-node
   :all-events
   :make-event-node

   ;; from-cepl
   :register-thunk-with-pump-events
   :pump-events
   :inject-backend-event

   :|sys|
   :|context|
   :|window|
   :|mouse|
   :|keyboard|
   :make-context-created
   :make-will-quit
   :make-win
   :make-cepl-mouse-event
   :make-mouse-scroll
   :make-mouse-button
   :make-mouse-motion
   :make-cepl-keyboard-event
   :make-key
   :event
   :mouse-button-state
   :key-state
   :will-quit
   :window
   :win
   :context-created
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

(defpackage :space
  (:use :cl :cl-game-math.base-vectors :cl-game-math.base-matrices :cepl-utils :cepl.events
	:named-readtables :varjo :varjo-lang)
  (:shadow :space)
  (:shadowing-import-from :cl-game-math.base-vectors :v!)
  (:shadowing-import-from :cl-game-math.base-matrices :m!)
  (:import-from :jungl :def-compile-pass :set-uniform :remove-uniform
		:set-arg-val)
  (:export :get-transform :p! :space-g :in :space!))

(defpackage :live
  (:use :cl :cepl-utils)
  (:export :main-loop
           :update-swank
           :peek
           :continuable))

(defpackage :cepl
  (:use :cl
        :cepl-generics
        :cl-game-math.base-vectors
        :cl-game-math.base-matrices
        :cl-game-math.base-maths
        :temporal-functions
        :cepl-camera
        :cl-fad
        :named-readtables
        :jungl)
  (:shadow :quit)
  (:import-from :space :p!)
  (:import-from :space :space-g :in)
  (:import-from :live
                :continuable
                :update-swank
                :peek)
  (:import-from :utils
                :deferror
                :print-mem
                :p->)
  (:import-from :cl-game-math.vectors
		:dvec
		:dvec*)
  (:import-from :cepl.events
                :def-named-event-node)
  (:export :repl
           :init
           :quit
           :make-project
           ;;----
	   :dvec
	   :dvec*
	   ;;----
           :def-named-event-node
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
           :with-viewport
           :with-fbo-viewport
           :def-equivalent-type
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
	   ))
