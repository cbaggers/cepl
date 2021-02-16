(in-package :%cepl.types)

;;------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +gl-id-bit-size+ 32))

(deftype gl-id ()
  '(unsigned-byte #.+gl-id-bit-size+))

(declaim (type gl-id +unknown-gl-id+))
(defconstant +unknown-gl-id+ #.(1- (expt 2 +gl-id-bit-size+)))
(defconstant +null-gl-id+ 0)

(defn-inline unknown-gl-id-p ((id gl-id)) boolean
  (declare (inline unknown-gl-id-p))
  (= id +unknown-gl-id+))

(deftype c-array-index ()
  '(unsigned-byte 32))

(deftype elem-byte-size ()
  '(unsigned-byte 32))

(deftype gl-enum-value ()
  '(unsigned-byte #.+gl-enum-size+))

(deftype gl-sizei ()
  ;; value has to be unsigned but the type is still signed
  '(signed-byte 32))

(deftype gbuf-byte-size ()
  '(unsigned-byte 32))

(deftype tex-unit ()
  '(unsigned-byte 8))

(deftype attachment-num ()
  '(unsigned-byte 16))

(deftype extended-attachment-num ()
  '(signed-byte 32))

(deftype attachment-name ()
  '(or attachment-num symbol))

(deftype stencil-mask ()
  '(unsigned-byte 8))

(defconstant +gl-color-mask-bit-size+
  #.(* 8 (cffi:foreign-type-size '%gl::clearbuffermask)))
(defconstant +unknown-clear-mask+
  #.(1- (expt 2 (* 8 (cffi:foreign-type-size '%gl::clearbuffermask)))))
(deftype clear-buffer-mask ()
  '(unsigned-byte #.(* 8 (cffi:foreign-type-size '%gl::clearbuffermask))))


(defun+ indexp (x)
  (typep x 'c-array-index))

;;------------------------------------------------------------

(defstruct (c-array (:constructor %make-c-array))
  (pointer
   (error "cepl: c-array must be created with a pointer")
   :type cffi-sys:foreign-pointer)
  (dimensions
   (error "cepl: c-array must be created with dimensions")
   :type list)
  (total-size
   (error "cepl: c-array must be created with total-size")
   :type c-array-index)
  (byte-size
   (error "cepl: c-array must be created with byte-size")
   :type c-array-index)
  (element-type
   (error "cepl: c-array must be created with an element-type")
   :type symbol)
  (sizes
   (error "CEPL (BUG): c-array created without internal sizes")
   :type (simple-array c-array-index (4)))
  (row-alignment
   (error "cepl: c-array must be created with a row-alignment")
   :type (integer 1 8))
  (struct-element-typep nil :type boolean)
  (element-pixel-format nil :type (or null pixel-format))
  (element-from-foreign
   (error "cepl: c-array must be created with a from-foreign function")
   :type (function (foreign-pointer) t))
  (element-to-foreign
   (error "cepl: c-array must be created with a to-foreign function")
   :type (function (foreign-pointer t) t))
  (free #'cffi:foreign-free
   :type function))

(defn-inline c-array-element-byte-size ((c-array c-array))
    c-array-index
  (aref (c-array-sizes c-array) 0))

;;------------------------------------------------------------

(defstruct (texture (:constructor %%make-texture))
  (id 0 :type gl-id)
  (cache-id 0 :type (signed-byte 32))
  (base-dimensions nil :type list)
  (type (error "") :type symbol)
  (image-format (error "") :type symbol)
  (mipmap-levels 0 :type (unsigned-byte 16))
  (layer-count 0 :type (unsigned-byte 16))
  (cubes-p nil :type boolean)
  (allocated-p nil :type boolean)
  (mutable-p nil :type boolean)
  (samples 0 :type (unsigned-byte 32))
  (fixed-sample-locations-p nil :type boolean)
  ;; last-sampler-id is used for perf optimizations
  ;; on gl v<3.3
  (last-sampler-id 0 :type (signed-byte 32)))

(defn-inline active-texture-num ((num (unsigned-byte 16))) (values)
  (declare (profile t))
  (%gl:active-texture (+ #.(gl-enum :texture0) num))
  (values))

;;------------------------------------------------------------

(defstruct (gpu-buffer (:constructor %make-gpu-buffer))
  (id 0 :type gl-id)
  (cache-id 0 :type (integer 0 13))
  (arrays (error "") :type (array gpu-array-bb (*))))

;;------------------------------------------------------------

(defstruct (gpu-array (:constructor %make-gpu-array))
  (dimensions nil :type list))

(defstruct (gpu-array-bb (:constructor %make-gpu-array-bb)
                         (:include gpu-array))
  (buffer (error "") :type gpu-buffer)
  (access-style :static-draw :type symbol)
  ;; buffer-data
  (element-type nil :type symbol) ;; data-type
  (byte-size 0 :type gbuf-byte-size) ;; data-index-length
  (element-byte-size 0 :type elem-byte-size)
  (offset-in-bytes-into-buffer 0 :type gbuf-byte-size)
  (element-pixel-format nil :type (or null pixel-format))
  ;; to match c-array and occasionaly used by pbos
  (row-alignment 1 :type (integer 1 8)))

(defstruct (gpu-array-t (:constructor %make-gpu-array-t)
                        (:include gpu-array))
  (texture (error "") :type texture)
  (texture-type (error "") :type symbol)
  (level-num 0 :type (unsigned-byte 16))
  (layer-num 0 :type (unsigned-byte 16))
  (face-num 0 :type (integer 0 5))
  (image-format nil :type symbol))

;;------------------------------------------------------------

(defstruct (buffer-texture
             (:include texture)
             (:constructor %%make-buffer-texture))
  (backing-array (error "") :type gpu-array-bb)
  (owns-array nil :type boolean))

;;------------------------------------------------------------

(defstruct sampler-id-box
  (id -1 :type (signed-byte 32))
  (shared-p nil :type boolean))

;; {TODO} border-color
(defstruct (sampler (:constructor %make-sampler)
                    (:conc-name %sampler-))
  (context-id 0 :type gl-id)
  (id-box (make-sampler-id-box) :type sampler-id-box)
  (type (error "") :type symbol)
  (texture (error "") :type texture)
  (lod-bias 0.0 :type single-float)
  (min-lod -1000.0 :type single-float)
  (max-lod 1000.0 :type single-float)
  (border-color (vec4 0f0 0f0 0f0 0f0) :type vec4)
  (expects-mipmap nil :type boolean)
  (minify-filter :linear :type keyword)
  (magnify-filter :linear :type keyword)
  (wrap #(:repeat :repeat :repeat) :type vector)
  (expects-depth nil :type boolean)
  (compare nil :type symbol)
  (anisotropy 1f0 :type single-float))

(defn-inline %sampler-id ((sampler sampler)) (signed-byte 32)
  (declare (profile t))
  (sampler-id-box-id (%sampler-id-box sampler)))

(defn (setf %sampler-id) ((value sampler-id-box) (sampler sampler))
    sampler-id-box
  (declare (profile t))
  (setf (%sampler-id-box sampler) value))

(defn-inline sampler-shared-p ((sampler sampler)) boolean
  (declare (profile t))
  (sampler-id-box-shared-p (%sampler-id-box sampler)))

;;------------------------------------------------------------

(defstruct (ubo (:constructor %make-ubo))
  (id 0 :type gl-id)
  (data (error "gpu-array must be provided when making ubo")
        :type gpu-array-bb)
  (index 0 :type c-array-index)
  (owns-gpu-array nil :type boolean))

;;------------------------------------------------------------

(defstruct (ssbo (:constructor %make-ssbo))
  (id 0 :type gl-id)
  (data (error "gpu-array must be provided when making ssbo")
        :type gpu-array-bb)
  (index 0 :type c-array-index)
  (owns-gpu-array nil :type boolean))

;;------------------------------------------------------------

;;{NOTE} if optimization called for it this could easily be an
;;       array of 16bit ints (or whatever works)
(defstruct (viewport (:conc-name %viewport-) (:constructor %make-viewport))
  (resolution-x 320 :type (unsigned-byte 16))
  (resolution-y 240 :type (unsigned-byte 16))
  (origin-x 0 :type (unsigned-byte 16))
  (origin-y 0 :type (unsigned-byte 16)))

(defgeneric viewport (obj))
(defgeneric (setf viewport) (value obj))

(defn make-viewport (&optional (resolution (or list vec2) '(320 240))
                               (origin (or list vec2) '(0 0)))
    viewport
  (%make-viewport :resolution-x (ceiling (elt resolution 0))
                  :resolution-y (ceiling (elt resolution 1))
                  :origin-x (ceiling (elt origin 0))
                  :origin-y (ceiling (elt origin 1))))

;;------------------------------------------------------------

(defstruct blending-params
  (mode-rgb :func-add :type keyword)
  (mode-alpha :func-add :type keyword)
  (source-rgb :src-alpha :type keyword)
  (source-alpha :src-alpha :type keyword)
  (destination-rgb :one-minus-src-alpha :type keyword)
  (destination-alpha :one-minus-src-alpha :type keyword))

;;------------------------------------------------------------

(defstruct (stencil-params (:constructor %make-stencil-params)
                           (:conc-name %stencil-params-))
  (test #.(gl-enum :never) :type (signed-byte 32) :read-only t)

  (value 0 :type stencil-mask :read-only t)

  (mask 0 :type stencil-mask :read-only t)

  (on-stencil-test-fail
   #.(gl-enum :keep) :type (signed-byte 32) :read-only t)

  (on-stencil-pass-depth-test-fail
   #.(gl-enum :keep) :type (signed-byte 32) :read-only t)

  (on-stencil-pass-depth-test-pass
   #.(gl-enum :keep) :type (signed-byte 32) :read-only t))

;;------------------------------------------------------------

(defstruct (render-buffer (:constructor %make-render-buffer)
                          (:conc-name %render-buffer-))
  (id 0 :type gl-id)
  (image-format (error "bug") :type symbol)
  (resolution (error "bug: render-buffer resolution not provided") :type vec2)
  (multisample-p nil :type boolean))

;;------------------------------------------------------------

(defstruct att
  (array nil :type (or null gpu-array-t render-buffer))
  (blend nil :type boolean)
  (bparams nil :type (or null blending-params))
  (owned-p nil :type boolean)
  (viewport nil :type (or null viewport)))

;;------------------------------------------------------------

(defstruct pixel-format
  (components (error "") :type symbol)
  (type (error "") :type symbol)
  (normalize t :type boolean)
  (sizes nil :type list)
  (reversed nil :type boolean)
  (comp-length 0 :type (unsigned-byte 8)))

;;------------------------------------------------------------

(deftype vao-id ()
  '(unsigned-byte 32))

;;------------------------------------------------------------

(defun+ draw-mode-group-id (x)
  (or (typecase x
        (varjo::points 0)
        (varjo::line-strip 1)
        (varjo::line-strip-adjacency 2)
        (varjo::line-loop 3)
        (varjo::lines 4)
        (varjo::lines-adjacency 5)
        (varjo::triangle-strip 6)
        (varjo::triangle-strip-adjacency 7)
        (varjo::triangle-fan 8)
        (varjo::triangles 9)
        (varjo::triangles-adjacency 10)
        (varjo::quads 11)
        (varjo::patches
         (let ((count (varjo::vertex-count x)))
           (case= count
             (1 0)
             (2 4)
             (3 9)
             (4 11)
             (otherwise (+ count 12))))))
      (error "draw-mode-group-id: ~a is not a draw-mode" x)))

(defn draw-mode-symbol-group-id ((x symbol)) (unsigned-byte 8)
  (declare (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))
  (or (case x
        (:triangles 9)
        (:points 0)
        (:lines 4)
        (:triangle-strip 6)
        (:triangle-fan 8)
        (:line-strip 1)
        (:line-loop 3)
        (:lines-adjacency 5)
        (:line-strip-adjacency 2)
        (:triangle-strip-adjacency 7)
        (:triangles-adjacency 10)
        (:quads 11)
        (:patches (error "Patches can not be used without a length")))
      (error "draw-mode-group-id: ~a is not a draw-mode" x)))

(defn primitive-keyword-to-enum-val ((kwd symbol)) (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)
                     (compilation-speed 0))
           (profile t))
  (ecase kwd
    (:triangles
     #.(gl-enum :triangles))
    (:points
     #.(gl-enum :points))
    (:lines
     #.(gl-enum :lines))
    (:patches
     #.(gl-enum :patches))
    (:triangle-strip
     #.(gl-enum :triangle-strip))
    (:triangle-fan
     #.(gl-enum :triangle-fan))
    (:line-strip
     #.(gl-enum :line-strip))
    (:line-loop
     #.(gl-enum :line-loop))
    (:triangle-strip-adjacency
     #.(gl-enum :triangle-strip-adjacency))
    (:triangles-adjacency
     #.(gl-enum :triangles-adjacency))
    (:line-strip-adjacency
     #.(gl-enum :line-strip-adjacency))
    (:lines-adjacency
     #.(gl-enum :lines-adjacency))))

;;------------------------------------------------------------

(defstruct (compute-space
             (:constructor make-compute-space
                           (&optional (size-x 1) (size-y 1) (size-z 1))))
  (size-x 1 :type (unsigned-byte 32))
  (size-y 1 :type (unsigned-byte 32))
  (size-z 1 :type (unsigned-byte 32)))

;;------------------------------------------------------------

(defstruct (buffer-stream (:constructor %make-buffer-stream))
  (vao 0 :type gl-id)
  (%start 0 :type (unsigned-byte 64))
  (%start-byte 0 :type (unsigned-byte 64))
  (base-vertex 0 :type (unsigned-byte 32))
  (length 1 :type (unsigned-byte 64))
  (%index-type-enum 0 :type gl-enum-value)
  (%index-type-size 0 :type (unsigned-byte 8))
  (gpu-arrays nil :type list)
  (%primitive nil :type symbol)
  (primitive-group-id 0 :type (unsigned-byte 8))
  (draw-mode-val 0 :type (unsigned-byte 32))
  (patch-length 0 :type (unsigned-byte 8))
  (managed nil :type boolean))

(defn buffer-stream-primitive ((stream buffer-stream)) t
  ;; a slow function, should be used sparingly
  (declare (profile t))
  (let ((prim (buffer-stream-%primitive stream)))
    (if (eq prim :patches)
        (list prim (buffer-stream-patch-length stream))
        prim)))

(defn (setf buffer-stream-primitive) ((primitive (or symbol list))
                                      (stream buffer-stream))
    (or symbol list)
  (declare (optimize (speed 3) (safety 1) (debug 1)
                     (compilation-speed 0))
           (profile t))
  (if (keywordp primitive)
      (unless (eq primitive (buffer-stream-%primitive stream))
        (assert (not (or (eq :patches primitive) (eq :patches primitive))))
        (setf (buffer-stream-%primitive stream) primitive)
        (setf (buffer-stream-primitive-group-id stream)
              (draw-mode-symbol-group-id primitive))
        (setf (buffer-stream-draw-mode-val stream)
              (primitive-keyword-to-enum-val primitive)))
      (set-patch-stream-primitive stream primitive))
  primitive)

(defun+ set-patch-stream-primitive (stream primitive)
  (let* ((prim (varjo.internals:primitive-name-to-instance primitive))
         (group-id (draw-mode-group-id prim))
         (enum-kwd (varjo::lisp-name prim))
         (enum-val (primitive-keyword-to-enum-val enum-kwd))
         (patch-length (primitive-vert-length prim)))
    (setf (buffer-stream-%primitive stream) enum-kwd
          (buffer-stream-primitive-group-id stream) group-id
          (buffer-stream-draw-mode-val stream) enum-val
          (buffer-stream-patch-length stream) patch-length)))

(defn primitive-vert-length ((prim varjo.internals:primitive))
    (unsigned-byte 8)
  (declare (profile t))
  (typecase prim
    (varjo::patches (coerce (varjo::vertex-count prim) '(unsigned-byte 8)))
    (varjo::triangles 3)
    (varjo::lines 2)
    (varjo::points 1)
    (otherwise 0)))

(defn-inline %valid-index-type-p ((x symbol)) boolean
  (declare (optimize (speed 3) (safety 1) (compilation-speed 0))
           (profile t))
  (and x (not (eq x :uninitialized))))

(defun+ make-raw-buffer-stream (&key vao start length
                                     index-type managed
                                     gpu-arrays (primitive :triangles))
  (let* ((prim (varjo.internals:primitive-name-to-instance primitive))
         (prim-group-id (draw-mode-group-id prim))
         (enum-kwd (varjo::lisp-name prim))
         (enum-val (primitive-keyword-to-enum-val enum-kwd))
         (patch-length (primitive-vert-length prim)))
    (%make-buffer-stream
     :vao (or vao 0)
     :%start (or start 0)
     :%start-byte (if (%valid-index-type-p index-type)
                      (* start (foreign-type-size index-type))
                      0)
     :length (or length 1)
     :%index-type-enum (%cffi-type->gl-enum index-type)
     :%index-type-size (if (%valid-index-type-p index-type)
                           (foreign-type-size index-type)
                           0)
     :managed managed
     :%primitive enum-kwd
     :primitive-group-id prim-group-id
     :draw-mode-val enum-val
     :patch-length patch-length
     :gpu-arrays gpu-arrays)))

(defn %cffi-type->gl-enum ((cffi-type-name symbol)) gl-enum-value
  (ecase cffi-type-name
    ((nil :uninitialized) 0)
    ((:char :signed-char) #.(gl-enum :byte))
    ((:uchar :unsigned-char) #.(gl-enum :unsigned-byte))
    ((:short :signed-short) #.(gl-enum :short))
    ((:ushort :unsigned-short) #.(gl-enum :unsigned-short))
    ((:int :signed-int) #.(gl-enum :int))
    ((:uint :unsigned-int) #.(gl-enum :unsigned-int))
    (:float #.(gl-enum :float))
    (:double #.(gl-enum :double))))

(defn-inline buffer-stream-index-type ((stream buffer-stream)) symbol
  (declare (optimize (speed 3) (safety 1) (debug 1)
                     (compilation-speed 0))
           (profile t))
  (ecase (buffer-stream-%index-type-enum stream)
    (0 nil)
    (#.(gl-enum :byte) :int8)
    (#.(gl-enum :unsigned-byte) :uint8)
    (#.(gl-enum :short) :short)
    (#.(gl-enum :unsigned-short) :ushort)
    (#.(gl-enum :int) :int)
    (#.(gl-enum :unsigned-int) :uint)
    (#.(gl-enum :float) :float)
    (#.(gl-enum :double) :double)))

(defn-inline buffer-stream-start ((stream buffer-stream))
    (unsigned-byte 64)
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (declare (optimize (speed 3) (safety 1) (debug 1)
                     (compilation-speed 0))
           (profile t))
  (buffer-stream-%start stream))

(defn (setf buffer-stream-start) ((value (unsigned-byte 64))
                                  (stream buffer-stream))
    (unsigned-byte 64)
  (declare (profile t) (inline buffer-stream-index-type))
  (setf (buffer-stream-%start stream) value)
  (let ((type-size (buffer-stream-%index-type-size stream)))
    (setf (buffer-stream-%start-byte stream) (* value type-size)))
  value)

(defn (setf buffer-stream-index-type)
    ((value symbol) (stream buffer-stream))
    symbol
  (declare (profile t))
  (setf (buffer-stream-%index-type-enum stream)
        (%cffi-type->gl-enum value))
  ;; doing this vv forces recalculation of start-byte

  (when (%valid-index-type-p value)
    (setf (buffer-stream-%index-type-size stream) (foreign-type-size value)
          (buffer-stream-start stream) (buffer-stream-start stream)))
  value)

(defn-inline buffer-stream-start-byte ((stream buffer-stream))
    (unsigned-byte 64)
  (declare (profile t))
  (buffer-stream-%start-byte stream))

(defun+ (setf buffer-stream-start-byte) (value stream)
  (declare (ignore value))
  (error "CEPL Internal Error: Do not set stream %start-byte directly~%~s"
         stream))

;;------------------------------------------------------------

(defstruct (transform-feedback-stream
             (:constructor %make-tfs)
             (:conc-name %tfs-))
  (arrays nil :type (or null (simple-array gpu-array-bb (*))))
  (pending-arrays nil :type (or null (simple-array gpu-array-bb (*))))
  (bound nil :type boolean)
  (current-prog-id +unknown-gl-id+ :type gl-id))

;;------------------------------------------------------------

(defstruct (empty-fbo-params
             (:conc-name %empty-fbo-params-))
  ;; setting dummy values here knowing that only internal code
  ;; should be creating this
  (fbo nil :type (or null fbo))
  (dimensions nil :type list)
  (viewport (%make-viewport) :type viewport)
  (layer-count 0 :type unsigned-byte)
  (samples 0 :type unsigned-byte)
  (fixed-sample-locations-p nil :type boolean))

(defstruct (fbo (:constructor %%make-fbo)
                (:conc-name %fbo-))
  (id 0 :type gl-id)
  ;; Once empty info is set we never remove it, we track emptiness with the
  ;; attachment-count slot
  (empty-params nil :type (or null empty-fbo-params))
  (color-arrays-fill-pointer 0 :type c-array-index)
  (color-arrays (make-array 0 :element-type 'att
                            :initial-element (symbol-value '+null-att+))
                :type (simple-array att (*)))
  (depth-array (make-att) :type att)
  (stencil-array (make-att) :type att)
  ;;
  (draw-buffer-map
   (error "draw-buffer array must be provided when initializing an fbo"))
  (clear-mask (cffi:foreign-bitfield-value
               '%gl::ClearBufferMask '(:color-buffer-bit))
              :type clear-buffer-mask)
  (is-default nil :type boolean)
  (attachment-count 0 :type (unsigned-byte 8))
  (blending-params (make-blending-params :mode-rgb :func-add
                                         :mode-alpha :func-add
                                         :source-rgb :one
                                         :source-alpha :one
                                         :destination-rgb :zero
                                         :destination-alpha :zero)
                   :type blending-params))

(defn-inline fbo-empty-p ((fbo fbo)) boolean
  (declare (speed 3) (debug 0) (safety 1))
  (= (%fbo-attachment-count fbo) 0))

;;------------------------------------------------------------

(defstruct (gpu-fence (:constructor %make-gpu-fence (obj))
                      (:conc-name %gpu-fence-))
  (obj (null-pointer) :type foreign-pointer))

;;------------------------------------------------------------

;; not normal to have the id generation here but then we can
;; use boa-constructors

(defn gen-query-id () gl-id
  (with-foreign-object (id '%gl:uint)
    (%gl:gen-queries 1 id)
    (mem-aref id '%gl:uint)))

(defstruct (gpu-query
             (:constructor make-gpu-query ()))
  (id (gen-query-id) :type gl-id :read-only t)
  (enum 0 :type (signed-byte 32) :read-only t)
  (cache-id 7 :type (integer 0 7) :read-only t))

(defstruct (timestamp-query
             (:constructor make-timestamp-query ())
             (:include gpu-query
                       (enum #.(gl-enum :timestamp)
                             :type (signed-byte 32)
                             :read-only t)
                       (cache-id 0 :type (integer 0 7)
                                 :read-only t))))

(defstruct (scoped-gpu-query
             (:include gpu-query))
  (active-p nil :type boolean))

(defstruct (samples-passed-query
             (:constructor make-samples-passed-query ())
             (:include scoped-gpu-query
                       (enum #.(gl-enum :samples-passed)
                             :type (signed-byte 32)
                             :read-only t)
                       (cache-id 1 :type (integer 0 7)
                                 :read-only t))))

(defstruct (any-samples-passed-query
             (:constructor make-any-samples-passed-query ())
             (:include scoped-gpu-query
                       (enum #.(gl-enum :any-samples-passed)
                             :type (signed-byte 32)
                             :read-only t)
                       (cache-id 2 :type (integer 0 7)
                                 :read-only t))))

(defstruct (any-samples-passed-conservative-query
             (:constructor make-any-samples-passed-conservative-query ())
             (:include scoped-gpu-query
                       (enum #.(gl-enum :any-samples-passed-conservative)
                             :type (signed-byte 32)
                             :read-only t)
                       (cache-id 3 :type (integer 0 7)
                                 :read-only t))))

(defstruct (primitives-generated-query
             (:constructor make-primitives-generated-query ())
             (:include scoped-gpu-query
                       (enum #.(gl-enum :primitives-generated)
                             :type (signed-byte 32)
                             :read-only t)
                       (cache-id 4 :type (integer 0 7)
                                 :read-only t))))

(defstruct (transform-feedback-primitives-written-query
             (:constructor make-transform-feedback-primitives-written-query ())
             (:include scoped-gpu-query
                       (enum #.(gl-enum :transform-feedback-primitives-written)
                             :type (signed-byte 32)
                             :read-only t)
                       (cache-id 5 :type (integer 0 7)
                                 :read-only t))))

(defstruct (time-elapsed-query
             (:constructor make-time-elapsed-query ())
             (:include scoped-gpu-query
                       (enum #.(gl-enum :time-elapsed)
                             :type (signed-byte 32)
                             :read-only t)
                       (cache-id 6 :type (integer 0 7)
                                 :read-only t))))

;;------------------------------------------------------------

(defun+ holds-gl-object-ref-p (object)
  (typecase object
    (texture t)
    (gpu-buffer t)
    (gpu-array t)
    (gpu-array-bb t)
    (gpu-array-t t)
    (buffer-texture t)
    (sampler t)
    (fbo t) ;; 20160402 - only one left to delay
    (buffer-stream t)))

(defgeneric can-be-shared-between-contexts-p (object)
  (:method ((object gpu-buffer)) t)
  (:method ((object gpu-array)) t)
  ;; (:method (object query) t)
  ;; (:method (object render-buffer) t)
  (:method ((object sampler)) t)
  (:method ((object texture)) t)
  (:method ((object fbo)) nil)
  ;;(:method (object pipeline) nil)
  (:method ((object transform-feedback-stream)) nil)
  (:method ((object buffer-stream)) nil))

;;------------------------------------------------------------

#+sbcl
(declaim (sb-ext:freeze-type fbo))

#+sbcl
(declaim (sb-ext:freeze-type ubo))

#+sbcl
(declaim (sb-ext:freeze-type ssbo))

#+sbcl
(declaim (sb-ext:freeze-type viewport))

#+sbcl
(declaim (sb-ext:freeze-type blending-params))

#+sbcl
(declaim (sb-ext:freeze-type stencil-params))

#+sbcl
(declaim (sb-ext:freeze-type render-buffer))

#+sbcl
(declaim (sb-ext:freeze-type att))

#+sbcl
(declaim (sb-ext:freeze-type pixel-format))

#+sbcl
(declaim (sb-ext:freeze-type compute-space))

;; Cant yet as is subclassed in nineveh
;; #+sbcl
;; (declaim (sb-ext:freeze-type buffer-stream))

#+sbcl
(declaim (sb-ext:freeze-type transform-feedback-stream))

#+sbcl
(declaim (sb-ext:freeze-type c-array))

#+sbcl
(declaim (sb-ext:freeze-type texture))

#+sbcl
(declaim (sb-ext:freeze-type gpu-buffer))

#+sbcl
(declaim (sb-ext:freeze-type gpu-array))

#+sbcl
(declaim (sb-ext:freeze-type gpu-array-bb))

#+sbcl
(declaim (sb-ext:freeze-type gpu-array-t))

#+sbcl
(declaim (sb-ext:freeze-type buffer-texture))

#+sbcl
(declaim (sb-ext:freeze-type sampler-id-box))

#+sbcl
(declaim (sb-ext:freeze-type sampler))

#+sbcl
(declaim (sb-ext:freeze-type gpu-fence))

#+sbcl
(declaim (sb-ext:freeze-type gpu-query))

#+sbcl
(declaim (sb-ext:freeze-type timestamp-query))

#+sbcl
(declaim (sb-ext:freeze-type scoped-gpu-query))

#+sbcl
(declaim (sb-ext:freeze-type samples-passed-query))

#+sbcl
(declaim (sb-ext:freeze-type any-samples-passed-query))

#+sbcl
(declaim (sb-ext:freeze-type any-samples-passed-conservative-query))

#+sbcl
(declaim (sb-ext:freeze-type primitives-generated-query))

#+sbcl
(declaim (sb-ext:freeze-type transform-feedback-primitives-written-query))

#+sbcl
(declaim (sb-ext:freeze-type time-elapsed-query))

;;------------------------------------------------------------

(defvar %cepl.types::*extra-primitive-types*
  '((:vec2 2 :float single-float)
    (:vec3 3 :float single-float)
    (:vec4 4 :float single-float)
    (:half-vec2 2 :half-float single-float)
    (:half-vec3 3 :half-float single-float)
    (:half-vec4 4 :half-float single-float)
    (:ivec2 2 :int (signed-byte 32))
    (:ivec3 3 :int (signed-byte 32))
    (:ivec4 4 :int (signed-byte 32))
    (:uvec2 2 :uint (unsigned-byte 32))
    (:uvec3 3 :uint (unsigned-byte 32))
    (:uvec4 4 :uint (unsigned-byte 32))
    (:mat2 4 :float single-float)
    (:mat3 9 :float single-float)
    (:mat4 16 :float single-float)
    (:mat2x2 4 :float single-float)
    (:mat2x3 6 :float single-float)
    (:mat2x4 8 :float single-float)
    (:mat3x2 6 :float single-float)
    (:mat3x3 9 :float single-float)
    (:mat3x4 12 :float single-float)
    (:mat4x2 8 :float single-float)
    (:mat4x3 12 :float single-float)
    (:mat4x4 16 :float single-float)
    (:uint8-vec2 2 :uint8 (unsigned-byte 8))
    (:uint8-vec3 3 :uint8 (unsigned-byte 8))
    (:uint8-vec4 4 :uint8 (unsigned-byte 8))
    (:int8-vec2 2 :int8 (signed-byte 8))
    (:int8-vec3 3 :int8 (signed-byte 8))
    (:int8-vec4 4 :int8 (signed-byte 8))))

;;------------------------------------------------------------

(declaim
 (ftype (function (single-float) (unsigned-byte 16)) %encode-half-float)
 (inline %encode-half-float)
 (ftype (function ((unsigned-byte 16)) single-float) %decode-half-float)
 (inline %decode-half-float))
(ieee-floats:make-float-converters %encode-half-float %decode-half-float
                                   5 10 nil)

(defn encode-half-float ((float single-float)) (unsigned-byte 16)
  (declare
   (optimize (speed 3) (safety 1) (debug 0))
   (inline %encode-half-float))
  (cond
    ((float-nan-p float)
     31745)
    ((eql float single-float-positive-infinity)
     31744)
    ((eql float single-float-negative-infinity)
     64512)
    (t (locally
           #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         (%encode-half-float float)))))

(defn decode-half-float ((bits (unsigned-byte 16))) single-float
  (declare
   (optimize (speed 3) (safety 1) (debug 0))
   (inline %decode-half-float))
  (let* ((sign (ldb (byte 1 15) bits))
         (exponent (ldb (byte 5 10) bits))
         (significand (ldb (byte 10 0) bits)))
    (if (= exponent 31)
        (cond ((not (zerop significand))
               (the single-float (float-features:bits-single-float #xFFC00000)))
              ((zerop sign) single-float-positive-infinity)
              (t single-float-negative-infinity))
        (%decode-half-float bits))))
