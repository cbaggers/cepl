(in-package :%cepl.types)

;;------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +gl-id-bit-size+ 16))

(deftype gl-id ()
  '(unsigned-byte #.+gl-id-bit-size+))

(declaim (type gl-id +unknown-gl-id+))
(defconstant +unknown-gl-id+ #.(1- (expt 2 +gl-id-bit-size+)))
(defconstant +null-gl-id+ 0)

(declaim (inline unknown-gl-id-p)
         (ftype (function (gl-id) boolean) unknown-gl-id-p))
(defun unknown-gl-id-p (id)
  (declare (gl-id id))
  (= id +unknown-gl-id+))

(deftype c-array-index ()
  '(unsigned-byte 32))

(defun indexp (x)
  (typep x 'c-array-index))

;;------------------------------------------------------------

(defstruct (c-array (:constructor %make-c-array))
  (pointer
   (error "cepl: c-array must be created with a pointer")
   :type cffi-sys:foreign-pointer)
  (dimensions
   (error "cepl: c-array must be created with dimensions")
   :type list)
  (element-type
   (error "cepl: c-array must be created with an element-type")
   :type symbol)
  (element-byte-size
   (error "cepl: c-array must be created with an element-byte-size")
   :type fixnum)
  (struct-element-typep nil :type boolean)
  (row-byte-size
   (error "cepl: c-array must be created with a pointer")
   :type fixnum)
  (element-pixel-format nil :type (or null pixel-format))
  (element-from-foreign
   (error "cepl: c-array must be created with a from-foreign function")
   :type function)
  (element-to-foreign
   (error "cepl: c-array must be created with a to-foreign function")
   :type (function (foreign-pointer t) t)))

;;------------------------------------------------------------

(defstruct (texture (:constructor %%make-texture))
  (id 0 :type real)
  (cache-id 0 :type (integer 0 11))
  (base-dimensions nil :type list)
  (type (error "") :type symbol)
  (image-format (error "") :type symbol)
  (mipmap-levels 0 :type fixnum)
  (layer-count 0 :type fixnum)
  (cubes-p nil :type boolean)
  (allocated-p nil :type boolean)
  (mutable-p nil :type boolean)
  ;; last-sampler-id is used for perf optimizations
  ;; on gl v<3.3
  (last-sampler-id 0 :type real))

(defvar +null-texture+
  (%%make-texture :type nil
                  :image-format nil))

;;------------------------------------------------------------

(defstruct (gpu-buffer (:constructor %make-gpu-buffer))
  (id 0 :type fixnum)
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
  (byte-size 0 :type (unsigned-byte 64)) ;; data-index-length
  (element-byte-size 0 :type (unsigned-byte 32))
  (offset-in-bytes-into-buffer 0 :type (unsigned-byte 64))) ;; offset-in-bytes-into-buffer

(defstruct (gpu-array-t (:constructor %make-gpu-array-t)
                        (:include gpu-array))
  (texture (error "") :type texture)
  (texture-type (error "") :type symbol)
  (level-num 0 :type fixnum)
  (layer-num 0 :type fixnum)
  (face-num 0 :type fixnum)
  (image-format nil :type symbol))

;;------------------------------------------------------------

(defvar +null-gpu-buffer+
  (%make-gpu-buffer :arrays (make-array 0 :element-type 'gpu-array-bb)))

;;------------------------------------------------------------

(defstruct (buffer-texture
             (:include texture)
             (:constructor %%make-buffer-texture))
  (backing-array (error "") :type gpu-array-bb)
  (owns-array nil :type boolean))

;;------------------------------------------------------------

(defstruct sampler-id-box
  (id -1 :type fixnum)
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
  (expects-mipmap nil :type boolean)
  (minify-filter :linear :type keyword)
  (magnify-filter :linear :type keyword)
  (wrap #(:repeat :repeat :repeat) :type vector)
  (expects-depth nil :type boolean)
  (compare nil :type symbol))

(defun %sampler-id (sampler)
  (sampler-id-box-id (%sampler-id-box sampler)))

(defun (setf %sampler-id) (value sampler)
  (setf (%sampler-id-box sampler) value))

(defun sampler-shared-p (sampler)
  (sampler-id-box-shared-p (%sampler-id-box sampler)))

;;------------------------------------------------------------

(defstruct (ubo (:constructor %make-ubo))
  (id 0 :type fixnum)
  (data (error "gpu-array must be provided when making ubo")
        :type gpu-array)
  (index 0 :type fixnum)
  (owns-gpu-array nil :type boolean))

;;------------------------------------------------------------

(defstruct blending-params
  (mode-rgb :func-add :type keyword)
  (mode-alpha :func-add :type keyword)
  (source-rgb :src-alpha :type keyword)
  (source-alpha :src-alpha :type keyword)
  (destination-rgb :one-minus-src-alpha :type keyword)
  (destination-alpha :one-minus-src-alpha :type keyword))

;;------------------------------------------------------------

(defstruct att
  (array nil :type (or null gpu-array-t))
  (blend nil :type boolean)
  (bparams nil :type (or null blending-params))
  (owned-p nil :type boolean))

(defstruct (fbo (:constructor %%make-fbo)
                (:conc-name %fbo-))
  (id -1 :type fixnum)
  ;;
  (color-arrays (make-array 0 :element-type 'att
                            :initial-element (make-att) :adjustable t
                            :fill-pointer 0)
                :type (array att *))
  (depth-array (make-att) :type att)
  ;;
  (draw-buffer-map
   (error "draw-buffer array must be provided when initializing an fbo"))
  (clear-mask (cffi:foreign-bitfield-value
               '%gl::ClearBufferMask '(:color-buffer-bit))
              :type fixnum)
  (is-default nil :type boolean)
  (blending-params (make-blending-params :mode-rgb :func-add
                                         :mode-alpha :func-add
                                         :source-rgb :one
                                         :source-alpha :one
                                         :destination-rgb :zero
                                         :destination-alpha :zero)
                   :type blending-params))

(defvar +null-fbo+
  (%%make-fbo :draw-buffer-map (cffi:null-pointer)))

;;------------------------------------------------------------

(defstruct pixel-format
  (components (error "") :type symbol)
  (type (error "") :type symbol)
  (normalize t :type boolean)
  (sizes nil :type list)
  (reversed nil :type boolean)
  (comp-length 0 :type fixnum))

;;------------------------------------------------------------

(deftype vao-id ()
  '(unsigned-byte 32))

(declaim (type vao-id +null-vao+))
(defvar +null-vao+ 0)

;;------------------------------------------------------------

(defun draw-mode-group-id (x)
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

;;------------------------------------------------------------

(defstruct (buffer-stream (:constructor %make-buffer-stream))
  vao
  (%start 0 :type (unsigned-byte 64))
  (%start-byte 0 :type (unsigned-byte 64))
  (length 1 :type unsigned-byte)
  (%index-type nil :type symbol)
  (%index-type-size 0 :type (unsigned-byte 8))
  (gpu-arrays nil :type list)
  (%primitive nil :type symbol)
  (primitive-group-id 0 :type (unsigned-byte 8))
  (draw-mode-val 0 :type (unsigned-byte 32))
  (patch-length 0 :type (unsigned-byte 8))
  (managed nil :type boolean))

(defun buffer-stream-primitive (stream)
  (buffer-stream-%primitive stream))

(defun (setf buffer-stream-primitive) (primitive stream)
  (let* ((prim (varjo.internals:primitive-name-to-instance primitive))
         (group-id (draw-mode-group-id prim))
         (enum-kwd (varjo::lisp-name prim))
         (enum-val (cffi:foreign-enum-value '%gl:enum enum-kwd :errorp t)))
    (setf (buffer-stream-%primitive stream) primitive
          (buffer-stream-primitive-group-id stream) group-id
          (buffer-stream-draw-mode-val stream) enum-val))
  primitive)

(defun primitive-vert-length (prim)
  (typecase prim
    (varjo::patches (varjo::vertex-count prim))
    (varjo::triangles 3)
    (varjo::lines 2)
    (varjo::points 1)
    (otherwise 0)))

(defun %valid-index-type-p (x)
  (and x (not (eq x :uninitialized))))

(defun make-raw-buffer-stream (&key vao start length
                                 index-type managed
                                 gpu-arrays (primitive :triangles))
  (let* ((prim (varjo.internals:primitive-name-to-instance primitive))
         (prim-group-id (draw-mode-group-id prim))
         (enum-kwd (varjo::lisp-name prim))
         (enum-val (cffi:foreign-enum-value '%gl:enum enum-kwd :errorp t))
         (patch-length (primitive-vert-length prim)))
    (%make-buffer-stream
     :vao vao
     :%start (or start 0)
     :%start-byte (if (%valid-index-type-p index-type)
                      (* start (foreign-type-size index-type))
                      0)
     :length (or length 1)
     :%index-type index-type
     :%index-type-size (if (%valid-index-type-p index-type)
                           (foreign-type-size index-type)
                           0)
     :managed managed
     :%primitive primitive
     :primitive-group-id prim-group-id
     :draw-mode-val enum-val
     :patch-length patch-length
     :gpu-arrays gpu-arrays)))

(defun buffer-stream-index-type (stream)
  (buffer-stream-%index-type stream))

(defun (setf buffer-stream-index-type) (value stream)
  (setf (buffer-stream-%index-type stream) value)
  ;; doing this vv forces recalculation of start-byte
  (when (%valid-index-type-p value)
    (setf (buffer-stream-%index-type-size stream) (foreign-type-size value)
          (buffer-stream-start stream) (buffer-stream-start stream)))
  value)

(declaim (ftype (function (buffer-stream) (unsigned-byte 64))
                buffer-stream-start))
(defun buffer-stream-start (stream)
  (buffer-stream-%start stream))

(defun (setf buffer-stream-start) (value stream)
  (setf (buffer-stream-%start stream) value)
  (let ((index-type (buffer-stream-index-type stream))
        (type-size (buffer-stream-%index-type-size stream)))
    (when (%valid-index-type-p index-type)
      (setf (buffer-stream-%start-byte stream)
            (* value type-size))))
  value)

(declaim (ftype (function (buffer-stream) (unsigned-byte 64))
                buffer-stream-start-byte))
(defun buffer-stream-start-byte (stream)
  (buffer-stream-%start-byte stream))

(defun (setf buffer-stream-start-byte) (value stream)
  (declare (ignore value))
  (error "CEPL Internal Error: Do not set stream %start-byte directly~%~s"
         stream))

;;------------------------------------------------------------

;;{NOTE} if optimization called for it this could easily be an
;;       array of 16bit ints (or whatever works)
(defstruct (viewport (:conc-name %viewport-) (:constructor %make-viewport))
  (resolution-x 320 :type fixnum)
  (resolution-y 240 :type fixnum)
  (origin-x 0 :type fixnum)
  (origin-y 0 :type fixnum))

(defgeneric viewport (obj))
(defgeneric (setf viewport) (value obj))

(defun make-viewport (&optional (resolution '(320 240)) (origin '(0 0)))
  (%make-viewport :resolution-x (ceiling (elt resolution 0))
                  :resolution-y (ceiling (elt resolution 1))
                  :origin-x (ceiling (elt origin 0))
                  :origin-y (ceiling (elt origin 1))))

;;------------------------------------------------------------

(defun holds-gl-object-ref-p (object)
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

;;------------------------------------------------------------

(defvar +null-gpu-buffer+
  (%make-gpu-buffer :arrays (make-array 0 :element-type 'gpu-array-bb)))

(defun make-uninitialized-texture (&optional buffer-backed-p)
  (if buffer-backed-p
      (%%make-buffer-texture
       :type :uninitialized
       :image-format :uninitialized
       :backing-array (make-uninitialized-gpu-array-bb))
      (%%make-texture
       :type :uninitialized :image-format :uninitialized)))

(defun make-uninitialized-gpu-array-bb (&optional buffer)
  (%make-gpu-array-bb
   :buffer (or buffer +null-gpu-buffer+)
   :access-style :uninitialized))

(defun make-uninitialized-gpu-array-t ()
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type :uninitialized))

(defun make-uninitialized-sampler (texture context-id)
  (%make-sampler
   :context-id context-id
   :texture texture
   :type :uninitialized))

(defun make-uninitialized-fbo ()
  (%%make-fbo
   :draw-buffer-map nil
   :clear-mask -13))

(defun make-uninitialized-buffer-stream (primitive)
  (make-raw-buffer-stream :index-type :uninitialized
                          :primitive primitive))

(defvar +null-texture-backed-gpu-array+
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type nil))

(defvar +null-buffer-backed-gpu-array+
  (%make-gpu-array-bb :buffer +null-gpu-buffer+
                      :access-style :invalid
                      :element-type nil
                      :byte-size 0
                      :offset-in-bytes-into-buffer 0))

(defvar +uninitialized-buffer-array+
  (make-array 0 :element-type 'gpu-array-bb
              :initial-element +null-buffer-backed-gpu-array+))

(defun make-uninitialized-gpu-buffer ()
  (%make-gpu-buffer :id 0 :arrays +uninitialized-buffer-array+))
