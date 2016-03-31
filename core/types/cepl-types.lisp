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
  (element-pixel-format nil :type (or null pixel-format)))

;;------------------------------------------------------------

;;{TODO} While I see why I started abstracting this using classes
;;       We cannot extend core functionality of gl, thus uses
;;       extensible constructs is optimizing for a case that can
;;       never happen. We should go for structs, ubyte and macros
;;       to hide the ugly, optimize for helping the compiler

(defstruct (texture (:constructor %%make-texture))
  (id 0 :type real)
  (base-dimensions nil :type list)
  (type (error "") :type symbol)
  (image-format (error "") :type symbol)
  (sampler-type nil :type symbol)
  (mipmap-levels 0 :type fixnum)
  (layer-count 0 :type fixnum)
  (cubes-p nil :type boolean)
  (allocated-p nil :type boolean)
  (sampler-object-id 0 :type real)
  (mutable-p nil :type boolean))

(defvar +null-texture+
  (%%make-texture :type nil
		  :image-format nil
		  :sampler-type nil))

;;------------------------------------------------------------

(defstruct (gpu-buffer (:constructor %make-gpu-buffer))
  (id 0 :type fixnum)
  (format nil :type list)
  (managed nil :type boolean))

(defvar +null-gpu-buffer+ (%make-gpu-buffer))

(defun make-uninitialized-gpu-buffer ()
  (%make-gpu-buffer :id 0 :format '(:uninitialized) :managed nil))

(defmethod cepl.memory::initialized-p ((object gpu-buffer))
  (not (equal (gpu-buffer-format object)
	      '(:uninitialized))))

;;------------------------------------------------------------

(defstruct (gpu-array (:constructor %make-gpu-array))
  (dimensions nil :type list))

(defstruct (gpu-array-bb (:constructor %make-gpu-array-bb)
			 (:include gpu-array))
  (buffer (error "") :type gpu-buffer)
  (format-index 0 :type fixnum)
  (start 0 :type fixnum)
  (access-style :static-draw :type symbol))

(defstruct (gpu-array-t (:constructor %make-gpu-array-t)
			(:include gpu-array))
  (texture (error "") :type texture)
  (texture-type (error "") :type symbol)
  (level-num 0 :type fixnum)
  (layer-num 0 :type fixnum)
  (face-num 0 :type fixnum)
  (image-format nil :type symbol))

(defvar +null-texture-backed-gpu-array+
  (%make-gpu-array-bb
   :buffer +null-gpu-buffer+
   :access-style :invalid))

(defvar +null-buffer-backed-gpu-array+
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type nil))

(defun make-uninitialized-gpu-array-bb ()
  (%make-gpu-array-bb
   :buffer +null-gpu-buffer+
   :access-style :uninitialized))

(defmethod cepl.memory::initialized-p ((object gpu-array-bb))
  (not (eq (gpu-array-bb-access-style object)
	   :uninitialized)))

(defun make-uninitialized-gpu-array-t ()
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type :uninitialized))

(defmethod cepl.memory::initialized-p ((object gpu-array-t))
  (not (eq (gpu-array-t-texture-type object)
	   :uninitialized)))

;;------------------------------------------------------------

(defstruct (buffer-texture
	     (:include texture)
	     (:constructor %%make-buffer-texture))
  (backing-array (error "") :type gpu-array-bb)
  (owns-array nil :type boolean))

;;------------------------------------------------------------

;; {TODO} border-color
(defstruct (sampler (:constructor %make-sampler)
                    (:conc-name %sampler-))
  (id -1 :type fixnum)
  (lod-bias 0.0 :type single-float)
  (min-lod -1000.0 :type single-float)
  (max-lod 1000.0 :type single-float)
  (expects-mipmap nil :type boolean)
  (minify-filter :linear :type keyword)
  (magnify-filter :linear :type keyword)
  (wrap #(:repeat :repeat :repeat) :type vector)
  (expects-depth nil :type boolean)
  (compare nil :type symbol))

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

(defstruct (fbo (:constructor %%make-fbo)
                (:conc-name %fbo-))
  (id -1 :type fixnum)
  (attachment-color (error "")
                    :type (array attachment *))
  (draw-buffer-map (error ""))
  (attachment-depth (%make-attachment) :type attachment)
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


(defstruct (attachment (:constructor %make-attachment)
                       (:conc-name %attachment-))
  (fbo nil :type (or null fbo))
  (gpu-array nil :type (or null gpu-array-t))
  (owns-gpu-array nil :type boolean)
  (blending-enabled nil :type boolean)
  (override-blending nil :type boolean)
  (blending-params (cepl.blending:make-blending-params
		    :mode-rgb :func-add
		    :mode-alpha :func-add
		    :source-rgb :one
		    :source-alpha :one
		    :destination-rgb :zero
		    :destination-alpha :zero) :type blending-params))

;;------------------------------------------------------------

(defstruct pixel-format
  (components (error "") :type symbol)
  (type (error "") :type symbol)
  (normalise t :type boolean)
  (sizes nil :type list)
  (reversed nil :type boolean)
  (comp-length 0 :type fixnum))

;;------------------------------------------------------------

(defstruct (buffer-stream (:constructor make-raw-buffer-stream
                                        (&key vao start length
                                              index-type managed
                                              gpu-arrays)))
  vao
  (start 0 :type unsigned-byte)
  (length 1 :type unsigned-byte)
  (index-type nil :type symbol)
  (gpu-arrays nil :type list)
  (managed nil :type boolean))

;;------------------------------------------------------------

;;{NOTE} if optimization called for it this could easily be an
;;       array of 16bit ints (or whatever works)
(defstruct (viewport (:conc-name %viewport-) (:constructor %make-viewport))
  (resolution-x 320 :type fixnum)
  (resolution-y 240 :type fixnum)
  (origin-x 0 :type fixnum)
  (origin-y 0 :type fixnum))
