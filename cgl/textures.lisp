;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;
(in-package :cepl-gl)

;;------------------------------------------------------------

(defclass gl-texture () ())

(defclass immutable-texture (gl-texture)
  ((texture-id :initarg :texture-id :reader texture-id)
   (base-dimensions :initarg :base-dimensions :accessor base-dimensions)
   (texture-type :initarg :texture-type :reader texture-type) ;the structure
   (internal-format :initarg :internal-format :reader internal-format) ;texels
   (sampler-type :initarg :sampler-type :reader sampler-type)
   (mipmap-levels :initarg :mipmap-levels)
   (layer-count :initarg :layer-count)
   (cubes :initarg :cubes)
   (allocated :initform nil :reader allocatedp)))

(defmethod print-object ((object immutable-texture) stream)
  (let ((m (slot-value object 'mipmap-levels))
        (l (slot-value object 'layer-count))
        (c (slot-value object 'cubes)))
    (format stream 
            "#<GL-~a (~{~a~^x~})~:[~; mip-levels:~a~]~:[~; layers:~a~]>"
            (slot-value object 'texture-type)
            (slot-value object 'base-dimensions)
            (when (> m 1) m) (when (> l 1) l) c)))

(defmethod gl-free ((object gl-texture))
  (free-texture object))

(defun blank-texture-object (texture)
  (with-slots (texture-id base-dimensions texture-type internal-format 
                          sampler-type mipmap-levels layer-count cubes
                          allocated) texture
    (setf texture-id -1
          base-dimensions nil
          texture-type nil
          internal-format nil
          sampler-type nil
          mipmap-levels nil
          layer-count nil
          cubes nil
          allocated nil)))

(defun free-texture (texture)
  (with-foreign-object (id :uint) 
    (setf (mem-ref id :uint) (texture-id texture))
    (setf (texture-id texture) -1)
    (%gl:delete-textures 1 id)))

;; [TODO] would a unboxed lisp array be faster?
(defun free-textures (textures)
  (with-foreign-object (id :uint (length textures))
    (loop :for texture :in textures :for i :from 0 :do
       (setf (mem-aref id :uint i) (texture-id texture)))
    (%gl:delete-textures 1 id)))

(defclass gpu-array-t ()
  ((texture :initarg :texture :reader texture)
   (texture-type :initarg :texture-type :reader texture-type)
   (dimensions :initform nil :initarg :dimensions :reader dimensions)
   (level-num :initarg :level-num)
   (layer-num :initarg :layer-num)
   (face-num :initarg :face-num)
   (internal-format :initform nil :initarg :internal-format :reader internal-format)))

(defmethod print-object ((object gpu-array-t) stream)
  (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by :TEXTURE>"
          (internal-format object)
          (dimensions object)))

(defmethod gl-free ((object gpu-array-t))
  (free-gpu-array-t object))

(defmethod free-gpu-array ((gpu-array gpu-array-t))
  (free-gpu-array-t gpu-array))

(defun free-gpu-array-t (texture)
  (if (typep texture 'immutable-texture)
      (error "Cannot free gpu-arrays that live inside an immutable texture")
      (error "Mutable textures not yet implmented...How did you get this error?")))

;;use with safe-exit thingy?
(defmacro with-texture-bound ((texture &optional type) &body body)
  (let ((tex (gensym "texture"))
        (res (gensym "result")))
    `(let ((,tex ,texture)) 
       (bind-texture ,tex ,type)
       (let ((,res (progn ,@body)))
         (unbind-texture (slot-value ,tex 'texture-type))
         ,res))))

(defun error-on-invalid-upload-formats (target internal-format pixel-format pixel-type)
  (unless (and internal-format pixel-type pixel-format)
    (error "Could not establish all the required formats for the pixel transfer"))
  (when (and (find internal-format '(:depth-component :depth-component16
                                        :depth-component24 :depth-component32f))
               (not (find target '(:texture_2d :proxy_texture_2d
                                   :texture_rectangle
                                   :proxy_texture_rectangle))))
    (error "Texture type is ~a. Cannot populate with ~a"
           target internal-format))
  (when (and (eq pixel-format :depth-component)
             (not (find internal-format 
                        '(:depth-component :depth-component16
                          :depth-component24 :depth-component32f))))
    (error "Pixel data is a depth format however the texture is not"))
  (when (and (not (eq pixel-format :depth-component))
             (find internal-format 
                   '(:depth-component :depth-component16
                     :depth-component24 :depth-component32f)))
    (error "Pixel data is a depth format however the texture is not"))
  t)

(defun upload-c-array-to-gpuarray-t (gpu-array c-array 
                                     &optional format type)
  ;; if no format or type
  (when (or (and format (not type)) (and type (not format)))
    (error "cannot only specify either format or type, must be both or neither"))
  (let* ((element-pf (pixel-format-of c-array))
         (compiled-pf (compile-pixel-format element-pf))
         (pix-format (or format (first compiled-pf)))
         (pix-type (or type (second compiled-pf))))
    (with-slots (texture dimensions level-num layer-num face-num 
                         internal-format texture-type) 
        gpu-array
      (error-on-invalid-upload-formats texture-type internal-format
                                       pix-format pix-type)
      (unless (equal (dimensions c-array) dimensions)
        (error "dimensions of c-array and gpu-array must match~%c-array:~a gpu-array:~a" (dimensions c-array) dimensions))
      (with-texture-bound ((texture gpu-array))
        (case (length dimensions)
          (1 (gl:tex-sub-image-1d texture-type level-num 0
                                  (first (dimensions c-array))
                                  format type (pointer c-array)))
          (2 (gl:tex-sub-image-2d texture-type level-num 0 0
                                  (first (dimensions c-array))
                                  (second (dimensions c-array))
                                  format type (pointer c-array)))
          (3 (gl:tex-sub-image-3d texture-type level-num 0 0 0
                                  (first (dimensions c-array))
                                  (second (dimensions c-array))
                                  (third (dimensions c-array))
                                  format type (pointer c-array)))
          (t (error "Cannot currently upload a c-array with more than 3 dimensions"))))))
  gpu-array)

(defun upload-from-buffer-to-gpuarray-t (&rest args)
  (declare (ignore args))
  (error "upload-from-buffer-to-gpuarray-t is not implemented yet"))

;;------------------------------------------------------------

(defparameter *mipmap-max-levels* 20)
(defparameter *valid-texture-storage-options* 
  '(((t nil nil 1 nil nil nil) :texture-1d)
    ((t nil nil 2 nil nil nil) :texture-2d)
    ((t nil nil 3 nil nil nil) :texture-3d)
    ((t t nil 1 nil nil nil) :texture-1d-array)
    ((t t nil 2 nil nil nil) :texture-2d-array)
    ((t nil t 2 nil nil nil) :texture-cube-map)
    ((t t t 2 nil nil nil) :texture-cube-map-array)
    ((nil nil nil 2 nil nil t) :texture-rectangle)
    ((nil nil nil 1 nil t nil) :texture-buffer)
    ((nil nil nil 2 t nil nil) :texture-2d-multisample)
    ((nil t nil 2 t nil nil) :texture-2d-multisample-array)))

;; [TODO] Add shadow samplers
;; [TODO] does cl-opengl use multisample instead of ms?
(defun calc-sampler-type (texture-type internal-format)
  (utils:kwd
   (case internal-format
     ((:r8 :r8-snorm :r16 :r16-snorm :rg8 :rg8-snorm :rg16 :rg16-snorm 
           :r3-g3-b2 :rgb4 :rgb5 :rgb8 :rgb8-snorm :rgb10 :rgb12 
           :rgb16-snorm :rgba2 :rgba4 
           :rgb5-a1 :rgba8 :rgba8-snorm :rgb10-a2 :rgba12 :rgba16 :srgb8
           :srgb8-alpha8 :r16f :rg16f :rgb16f :rgba16f :r32f :rg32f :rgb32f
           :rgba32f :r11f-g11f-b10f :rgb9-e5) "")
     ((:r8i :r16i :r32i :rg8i :rg16i :rg32i :rgb8i :rgb16i :rgb32i :rgba8i
            :rgba32i :rgba16i) :i)
     ((:rg8ui :rg16ui :rg32ui :rgb8ui :rgb16ui :rgb32ui :rgba8ui :rgba16ui
              :rgba32ui :rgb10-a2ui :r8ui :r16ui :r32ui) :ui)
     (t (error "internal-format unknown")))
   (case texture-type
     (:texture-1d :sampler-1d) (:texture-2d :sampler-2d) (:texture-3d :sampler-3d)
     (:texture-cube-map :sampler-cube) (:texture-rectangle :sampler-2drect)
     (:texture-1d-array :sampler-1d-array) (:texture-2d-array :sampler-2d-array)
     (:texture-cube-map-array :sampler-cube-array) (:texture-buffer :sampler-buffer)
     (:texture-2d-multisample :sampler-2d-ms)
     (:texture-2d-multisample-array :sampler-2d-ms-array) 
     (t (error "texture type not known")))))

;;------------------------------------------------------------

(defun gen-texture ()
  (first (gl:gen-textures 1)))

(defun po2p (x) (eql 0 (logand x (- x 1))))

(defun dimensions-at-mipmap-level (texture level)
  (if (= level 0)
      (base-dimensions texture)
      (let ((div (* 2 (1+ level))))
        (loop for i in (base-dimensions texture) collecting
             (/ i div)))))

;;------------------------------------------------------------

(defun establish-texture-type (dimensions mipmap layers cubes po2 multisample 
                               buffer rectangle)
  (declare (ignore po2))
  (cadr (assoc (list mipmap layers cubes dimensions multisample buffer rectangle)
               *valid-texture-storage-options*
               :test #'(lambda (a b)
                         (destructuring-bind
                               (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7)
                             (append a b)
                           (and (if b1 t (not a1)) (if b2 t (not a2))
                                (if b3 t (not a3)) (eql b4 a4)
                                (eql b5 a5) (eql b6 a6) (eql b7 a7)))))))

;;------------------------------------------------------------

;; [TODO] how does mipmap-max affect this
;; [TODO] si the max layercount?
;; [TODO] should fail on layer-count=0?
(defun make-texture (&key dimensions internal-format (mipmap nil) 
                       (layer-count 1) (cubes nil) (rectangle nil)
                       (multisample nil) (immutable t) (buffer-storage nil)
                       initial-contents)
  (let* ((pixel-format (when initial-contents 
                         (pixel-format-of initial-contents)))
         (internal-format 
         (if internal-format
             (if (pixel-format-p internal-format)
                 (internal-format-from-pixel-format internal-format)
                 internal-format)
             (when initial-contents
               (or (internal-format-from-pixel-format pixel-format)
                   (error "Could not infer the internal-format")))))
         (dimensions (if initial-contents
                         (if dimensions
                             (error "Cannot specify dimensions and have non nil initial-contents")
                             (dimensions initial-contents))
                         (if dimensions dimensions (error "must specify dimensions if no initial-contents provided")))))
    (if (and immutable (not buffer-storage))
        ;; check for power of two - handle or warn
        (let ((texture-type (establish-texture-type 
                             (if (listp dimensions) (length dimensions) 1)
                             mipmap (> layer-count 1) cubes 
                             (every #'po2p dimensions) multisample 
                             buffer-storage rectangle)))
          (if texture-type
              (if (and cubes (not (apply #'= dimensions)))
                  (error "Cube textures must be square")
                  (let ((texture (make-instance 
                                  'immutable-texture
                                  :texture-id (gen-texture)
                                  :base-dimensions dimensions
                                  :texture-type texture-type
                                  :mipmap-levels 
                                  (if mipmap
                                      (floor (log (apply #'max dimensions) 2))
                                      1)
                                  :layer-count layer-count
                                  :cubes cubes
                                  :internal-format internal-format
                                  :sampler-type (calc-sampler-type texture-type 
                                                                   internal-format))))
                    (with-texture-bound (texture)
                      (allocate-texture texture)
                      (when initial-contents
                        (destructuring-bind (pformat ptype)
                            (compile-pixel-format pixel-format)
                          (upload-c-array-to-gpuarray-t
                           (texref texture) initial-contents
                           pformat ptype))))
                    texture))
              (error "This combination of texture features is invalid")))
        (error "Textures with mutable storage and buffer backed 
              textures are not yet implemented"))))

(defun allocate-texture (texture)
  (if (allocatedp texture)
      (error "Attempting to reallocate a previously allocated texture")
      (let ((base-dimensions (base-dimensions texture))
            (texture-type (slot-value texture 'texture-type)))
        (case texture-type
          ((:texture-1d :proxy-texture-1d) 
           (tex-storage-1d texture-type
                           (slot-value texture 'mipmap-levels)
                           (slot-value texture 'internal-format)
                           (first base-dimensions)))
          ((:texture-2d :proxy-texture-2d :texture-1d-array :texture-rectangle
                        :proxy-texture-rectangle :texture-cube-map
                        :proxy-texture-cube-map :proxy-texture-1d-array)
           (tex-storage-2d texture-type
                           (slot-value texture 'mipmap-levels)
                           (slot-value texture 'internal-format)
                           (first base-dimensions)
                           (second base-dimensions)))
          ((:texture-3d :proxy-texture-3d :texture-2d-array :texture-cube-array 
                        :proxy-texture-cube-array :proxy-texture-2d-array)
           (tex-storage-3d texture-type
                           (slot-value texture 'mipmap-levels)
                           (slot-value texture 'internal-format)
                           (first base-dimensions)
                           (second base-dimensions)
                           (third base-dimensions))))
        (setf (slot-value texture 'allocated) t))))


(defun valid-index-p (texture mipmap-level layer cube-face)
  (with-slots (mipmap-levels layer-count cubes)
      texture
    (and (< mipmap-level mipmap-levels)
         (< layer layer-count)
         (if cubes
             (<= cube-face 6)
             (eql 0 cube-face)))))

(defun texref (texture &key (mipmap-level 0) (layer 0) (cube-face 0))
  (if (valid-index-p texture mipmap-level layer cube-face)
      (make-instance 'gpu-array-t
                     :texture texture
                     :texture-type (texture-type texture)
                     :level-num mipmap-level
                     :layer-num layer
                     :face-num cube-face
                     :dimensions (dimensions-at-mipmap-level
                                  texture mipmap-level)
                     :internal-format (internal-format texture))
      (error "Texture index out of range")))

;;------------------------------------------------------------


(defmethod gl-push ((object c-array) (destination gpu-array-t))
  (upload-c-array-to-gpuarray-t destination object))

(defmethod gl-pull-1 ((object gpu-array-t))
  (declare (ignore object))
  (print "Should now pull the gpu-array-t to a c-array"))

(defmethod backed-by ((object gpu-array-t))
  :texture)

(defun unbind-texture (type)
  (gl:bind-texture type 0))

(defun bind-texture (texture &optional type)
  (let ((texture-type (slot-value texture 'texture-type)))
    (if (or (null type) (eq type texture-type))
        (gl:bind-texture texture-type (texture-id texture))
        (if (eq :none texture-type)
            (progn (gl:bind-texture type (texture-id texture))
                   (setf (slot-value texture 'texture-type) type))
            (error "Texture has already been bound"))))
  texture)

;; copy data (from cpu to gpu) - texsubimage1d texsubimage2d texsubimage3d
;; copy data (from gpu to cpu) - get-tex-image
;; copy data (from frame-buffer to texture image) - leave for now
;; copy from buffer to texture glCopyTexSubImage2D
;; set texture params
;; get texture params
;; texture views
;; generate-mipmaps
;; texsubimage*d - pushing data
;; glPixelStore — set pixel storage modes

