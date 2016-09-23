(in-package :%cepl.types)

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
  (arrays (error "") :type (array gpu-array-bb (*)))
  (managed nil :type boolean))

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

;;------------------------------------------------------------

(defstruct pixel-format
  (components (error "") :type symbol)
  (type (error "") :type symbol)
  (normalize t :type boolean)
  (sizes nil :type list)
  (reversed nil :type boolean)
  (comp-length 0 :type fixnum))

;;------------------------------------------------------------

(defstruct (buffer-stream (:constructor %make-buffer-stream))
  vao
  (%start 0 :type unsigned-byte) ;; unsigned-byte ≡ (integer 0 *)
  (%start-byte (null-pointer) :type foreign-pointer)
  (length 1 :type unsigned-byte)
  (%index-type nil :type symbol)
  (gpu-arrays nil :type list)
  (managed nil :type boolean))

(defun %valid-index-type-p (x)
  (and x (not (eq x :uninitialized))))

(defun make-raw-buffer-stream (&key vao start length
                                 index-type managed
                                 gpu-arrays)
  (%make-buffer-stream
   :vao vao
   :%start (or start 0)
   :%start-byte (if (%valid-index-type-p index-type)
                    (make-pointer (* start (foreign-type-size index-type)))
                    (null-pointer))
   :length (or length 1)
   :%index-type index-type
   :managed managed
   :gpu-arrays gpu-arrays))

(defun buffer-stream-index-type (stream)
  (buffer-stream-%index-type stream))

(defun (setf buffer-stream-index-type) (value stream)
  (setf (buffer-stream-%index-type stream) value)
  ;; doing this vv forces recalculation of start-byte
  (when (%valid-index-type-p value)
    (setf (buffer-stream-start stream) (buffer-stream-start stream)))
  value)

(defun buffer-stream-start (stream)
  (buffer-stream-%start stream))

(defun (setf buffer-stream-start) (value stream)
  (setf (buffer-stream-%start stream) value)
  (let ((index-type (buffer-stream-index-type stream)))
    (when (%valid-index-type-p index-type)
      (setf (buffer-stream-%start-byte stream)
            (make-pointer (* value (foreign-type-size index-type))))))
  value)

(defun buffer-stream-start-byte (stream)
  (buffer-stream-%start-byte stream))

(defun buffer-stream-%start-byte (stream)
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
       :type :uninitialized :image-format :uninitialized
       :backing-array buffer-backed-p)
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

(defun make-uninitialized-sampler (texture)
  (%make-sampler :texture texture :type :uninitialized))

(defun make-uninitialized-fbo ()
  (%%make-fbo
   :draw-buffer-map nil
   :clear-mask -13))

(defun make-uninitialized-buffer-stream ()
  (make-raw-buffer-stream :index-type :uninitialized))

(defvar +null-texture-backed-gpu-array+
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type nil))

(defvar +null-buffer-backed-gpu-array+
  (%make-gpu-array-bb
   :buffer +null-gpu-buffer+
   :access-style :invalid))

(defvar +uninitialized-buffer-array+
  (make-array 0 :element-type 'gpu-array-bb
	      :initial-element +null-buffer-backed-gpu-array+))

(defun make-uninitialized-gpu-buffer ()
  (%make-gpu-buffer :id 0 :arrays +uninitialized-buffer-array+ :managed nil))
