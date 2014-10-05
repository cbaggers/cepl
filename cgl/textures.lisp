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
(defparameter *immutable-available* t)
(defparameter *cube-face-order* '(:texture-cube-map-positive-x​
                                  :texture-cube-map-negative-x​
                                  :texture-cube-map-positive-y​
                                  :texture-cube-map-negative-y​
                                  :texture-cube-map-positive-z​
                                  :texture-cube-map-negative-z​)) 

(defclass %gltexture ()
  ((id :type (unsigned-byte 16))
   (mutablep :type boolean)
   (allocatedp :type boolean)))

(defclass %glarraytexture (%gltexture)
  ((length :type (unsigned-byte 16))))

(defclass texture-1d (%gltexture) ())
(defclass texture-2d (%gltexture) ())
(defclass texture-3d (%gltexture) ())
(defclass texture-rectangle (%gltexture) ())
(defclass texture-cube-map (%gltexture) ())
(defclass texture-1d-array (%glarraytexture) ())
(defclass texture-2d-array (%glarraytexture) ())
(defclass texture-cube-map-array (%glarraytexture) ())
(defclass texture-buffer (%gltexture)
  ((backing-array :type gpuarray)
   (owns-array :type boolean)))

(defstruct array-cube
  (positive-x nil :type gpu-array-t)
  (negative-x nil :type gpu-array-t)
  (positive-y nil :type gpu-array-t)
  (negative-y nil :type gpu-array-t)
  (positive-z nil :type gpu-array-t)
  (negative-z nil :type gpu-array-t))

(defstruct (array-texture-mipmap-level
             (:constructor %make-array-tex-mipmap-level)
             (:conc-name %atml-))
  (texture nil :type %glarraytexture)
  (mipmap-level 0 :type (unsigned-byte 8)))

(defun arraytexref (level subscript)
  (let ((texture (%atml-texture level))
        (mipmap-level (%atml-mipmap-level level)))
    (if (< subscript (slot-value texture 'length))
        (if (typep texture 'texture-cube-map-array)
            (%make-tex-cube texture mipmap-level subscript)
            (make-instance 'gpu-array-t :texture texture :level-num mipmap-level
                           :layer-num subscript))
        (error "CEPL: Index out of bounds"))))

(defun mipref (texture subscript)
  (typecase texture
    (texture-1d 
     (make-instance 'gpu-array-t :texture texture :level-num subscript))
    (texture-2d
     (make-instance 'gpu-array-t :texture texture :level-num subscript))
    (texture-3d
     (make-instance 'gpu-array-t :texture texture :level-num subscript))
    (texture-rectangle (error "rectangle textures dont support mipmap levels"))
    (texture-buffer (error "buffer textures dont support mipmap levels"))
    (%glarraytexture
     (%make-array-tex-mipmap-level :texture texture :mipmap-level subscript))
    (texture-cube-map (%make-tex-cube texture subscript))
    (t (error "CEPL: Unknown texture type"))))

(defun %make-tex-cube (texture mipmap-level &optional (index 0))
  (make-array-cube 
   :positive-x (make-instance 'gpu-array-t :texture texture 
                              :level-num mipmap-level :face-num 0
                              :layer-num index)
   :negative-x (make-instance 'gpu-array-t :texture texture 
                              :level-num mipmap-level :face-num 1
                              :layer-num index)
   :positive-y (make-instance 'gpu-array-t :texture texture 
                              :level-num mipmap-level :face-num 2
                              :layer-num index)
   :negative-y (make-instance 'gpu-array-t :texture texture 
                              :level-num mipmap-level :face-num 3
                              :layer-num index)
   :positive-z (make-instance 'gpu-array-t :texture texture 
                              :level-num mipmap-level :face-num 4
                              :layer-num index)
   :negative-z (make-instance 'gpu-array-t :texture texture 
                              :level-num mipmap-level :face-num 5
                              :layer-num index)))

;;------------------------------------------------------------

(defun texref (texture &key (mipmap-level 0) (layer 0) (cube-face 0))
  (if (eq (slot-value texture 'texture-type) :texture-buffer)
      (if (> (+ mipmap-level layer cube-face) 0)
          (error "Texture index out of range")
          (slot-value texture 'backing-array))
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
          (error "Texture index out of range"))))

(defun valid-index-p (texture mipmap-level layer cube-face)
  (with-slots (mipmap-levels layer-count cubes)
      texture
    (and (< mipmap-level mipmap-levels)
         (< layer layer-count)
         (if cubes
             (<= cube-face 6)
             (eql 0 cube-face)))))

;;------------------------------------------------------------

(defclass gl-texture () 
  ((texture-id :initarg :texture-id :reader texture-id)
   (base-dimensions :initarg :base-dimensions :accessor base-dimensions)
   (texture-type :initarg :texture-type :reader texture-type)
   (internal-format :initarg :internal-format :reader internal-format)
   (sampler-type :initarg :sampler-type :reader sampler-type)
   (mipmap-levels :initarg :mipmap-levels)
   (layer-count :initarg :layer-count)
   (cubes :initarg :cubes)
   (allocated :initform nil :reader allocatedp)))

(defclass immutable-texture (gl-texture) ())
(defclass mutable-texture (gl-texture) ())
(defclass buffer-texture (gl-texture) 
  ((backing-array :initarg :backing-array)
   (owns-array :initarg :owns-array)))

(defgeneric mutable-texturep (texture))
(defmethod mutable-texturep ((texture mutable-texture)) t)
(defmethod mutable-texturep ((texture immutable-texture)) nil)

(defmethod print-object ((object mutable-texture) stream)
  (let ((m (slot-value object 'mipmap-levels))
        (l (slot-value object 'layer-count))
        (c (slot-value object 'cubes)))
    (format stream 
            "#<GL-~a (~{~a~^x~})~:[~; mip-levels:~a~]~:[~; layers:~a~]>"
            (slot-value object 'texture-type)
            (slot-value object 'base-dimensions)
            (when (> m 1) m) (when (> l 1) l) c)))

(defmethod print-object ((object immutable-texture) stream)
  (let ((m (slot-value object 'mipmap-levels))
        (l (slot-value object 'layer-count))
        (c (slot-value object 'cubes)))
    (format stream 
            "#<GL-~a (~{~a~^x~})~:[~; mip-levels:~a~]~:[~; layers:~a~]>"
            (slot-value object 'texture-type)
            (slot-value object 'base-dimensions)
            (when (> m 1) m) (when (> l 1) l) c)))

(defmethod print-object ((object buffer-texture) stream)
  (format stream 
          "#<GL-~a (~{~a~^x~})>"
          (slot-value object 'texture-type)
          (slot-value object 'base-dimensions)))

(defmethod gl-free ((object gl-texture))
  (free-texture object))

(defun blank-texture-object (texture)
  (with-slots (texture-id base-dimensions texture-type internal-format 
                          sampler-type mipmap-levels layer-count cubes
                          allocated) texture
    (setf (slot-value texture 'texture-id) -1
          (slot-value texture 'base-dimensions) nil
          (slot-value texture 'texture-type) nil
          (slot-value texture 'internal-format) nil
          (slot-value texture 'sampler-type) nil
          (slot-value texture 'mipmap-levels) nil
          (slot-value texture 'layer-count) nil
          (slot-value texture 'cubes) nil
          (slot-value texture 'allocated) nil)))

(defmethod free-texture ((texture gl-texture))
  (with-foreign-object (id :uint) 
    (setf (mem-ref id :uint) (texture-id texture))
    (setf (slot-value texture 'texture-id) -1)
    (%gl:delete-textures 1 id)))

(defmethod free-texture ((texture buffer-texture))
  (with-foreign-object (id :uint) 
    (with-slots (owns-array backing-array) texture
      (when owns-array
        (gl-free backing-array)
        (setf owns-array nil
              backing-array nil)))
    (setf (mem-ref id :uint) (texture-id texture))
    (setf (slot-value texture 'texture-id) -1)
    (%gl:delete-textures 1 id)))

(defclass gpu-array-t ()
  ((texture :initarg :texture :reader texture)
   (texture-type :initform nil :initarg :texture-type :reader texture-type)
   (dimensions :initform nil :initarg :dimensions :reader dimensions)
   (level-num :initform 0 :initarg :level-num)
   (layer-num :initform 0 :initarg :layer-num)
   (face-num :initform 0 :initarg :face-num)
   (internal-format :initform nil :initarg :internal-format :reader internal-format)))

(defmethod initialize-instance :after
    ((array gpu-array-t) &key texture texture-type internal-format)
  (when (null texture-type) 
    (setf (slot-value array 'texture-type) (texture-type texture)))
  (when (null internal-format) 
    (setf (slot-value array 'internal-format) (internal-format texture))))

(defmethod print-object ((object gpu-array-t) stream)
  (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by :TEXTURE>"
          (internal-format object)
          (dimensions object)))

(defmethod gl-free ((object gpu-array-t))
  (declare (ignore object))
  (free-gpu-array-t))

(defmethod free-gpu-array ((gpu-array gpu-array-t))
  (declare (ignore gpu-array))
  (free-gpu-array-t))

(defun free-gpu-array-t ()
  (error "Cannot free a texture backed gpu-array. gl-free the texture containing this array "))

;; [TODO] use with safe-exit thingy?
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
  (when (eq target :texture-buffer)
    (error "You should not have reached this function as buffer backed textures have buffer-gpu-arrays as their backing stores"))
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

(defun upload-c-array-to-gpuarray-t (gpu-array c-array &optional format type)
  ;; if no format or type
  (when (or (and format (not type)) (and type (not format)))
    (error "cannot only specify either format or type, must be both or neither"))
  (let* ((element-pf (pixel-format-of c-array))
         (compiled-pf (compile-pixel-format element-pf))
         (pix-format (or format (first compiled-pf)))
         (pix-type (or type (second compiled-pf))))
    (with-slots (texture dimensions level-num layer-num face-num internal-format
                         texture-type) gpu-array
      (error-on-invalid-upload-formats texture-type internal-format pix-format
                                       pix-type)
      (unless (equal (dimensions c-array) dimensions)
        (error "dimensions of c-array and gpu-array must match~%c-array:~a gpu-array:~a" (dimensions c-array) dimensions))
      (with-texture-bound ((texture gpu-array))
        (%upload-tex texture texture-type level-num (dimensions c-array) 
                     layer-num face-num pix-format pix-type (pointer c-array)))))
  gpu-array)

;; [TODO] add offsets
(defgeneric %upload-tex (tex tex-type level-num dimensions layer-num face-num
                         pix-format pix-type pointer))

(defmethod %upload-tex ((tex mutable-texture) tex-type level-num dimensions
                        layer-num face-num pix-format pix-type pointer)
  (case tex-type
    (:texture-1d (gl:tex-image-1d tex-type level-num (internal-format tex)
                                  (first dimensions) 0 pix-format pix-type
                                  pointer))
    (:texture-2d (gl:tex-image-2d tex-type level-num (internal-format tex)
                                  (first dimensions) (second dimensions) 0
                                  pix-format pix-type pointer))
    (:texture-3d (gl:tex-image-3d tex-type level-num (internal-format tex)
                                  (first dimensions) (second dimensions)
                                  (third dimensions) 0 pix-format pix-type
                                  pointer))
    (:texture-1d-array (gl:tex-image-2d tex-type level-num 
                                        (internal-format tex)
                                        (first dimensions) layer-num 0
                                        pix-format pix-type pointer))
    (:texture-2d-array (gl:tex-image-3d tex-type level-num 
                                        (internal-format tex)
                                        (first dimensions) (second dimensions)
                                        layer-num 0 pix-format pix-type pointer))
    (:texture-cube-map (gl:tex-image-2d (nth face-num *cube-face-order*)
                                        level-num (internal-format tex)
                                        (first dimensions) (second dimensions) 0
                                        pix-format pix-type pointer))
    (t (error "not currently supported for upload: ~a" tex-type))))


(defmethod %upload-tex ((tex immutable-texture) tex-type level-num dimensions
                        layer-num face-num pix-format pix-type pointer)
  (case tex-type
    (:texture-1d (gl:tex-sub-image-1d tex-type level-num 0 (first dimensions) 
                                      pix-format pix-type pointer))
    (:texture-2d (gl:tex-sub-image-2d tex-type level-num 0 0
                                      (first dimensions) (second dimensions)
                                      pix-format pix-type pointer))
    (:texture-1d-array (gl:tex-sub-image-2d tex-type level-num 0 0
                                            (first dimensions) layer-num
                                            pix-format pix-type pointer))
    (:texture-3d (gl:tex-sub-image-3d tex-type level-num 0 0 0
                                      (first dimensions) (second dimensions)
                                      (third dimensions) pix-format pix-type
                                      pointer))
    (:texture-2d-array (gl:tex-sub-image-3d tex-type level-num 0 0 0
                                            (first dimensions)
                                            (second dimensions) layer-num
                                            pix-format pix-type pointer))
    (:texture-cube-map (gl:tex-sub-image-2d (nth face-num *cube-face-order*)
                                            level-num 0 0 (first dimensions)
                                            (second dimensions) pix-format 
                                            pix-type pointer))
    (t (error "not currently supported for upload: ~a" tex-type))))

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
    ((nil nil nil 2 nil t nil) :texture-buffer)
    ((nil nil nil 3 nil t nil) :texture-buffer)
    ((nil nil nil 2 t nil nil) :texture-2d-multisample)
    ((nil t nil 2 t nil nil) :texture-2d-multisample-array)))

;; {TODO} Ok so the internal format bit is crap. Fix this
;; [TODO] Add shadow samplers
;; [TODO] does cl-opengl use multisample instead of ms?
(defun calc-sampler-type (texture-type internal-format)
  "Makes the keyword that names the sampler-type for the given texture-type and format"
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
             (floor (/ i div))))))

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
;; [TODO] what is the max layercount?
;; [TODO] should fail on layer-count=0?
(defun make-texture (&key dimensions internal-format (mipmap nil) 
                       (layer-count 1) (cubes nil) (rectangle nil)
                       (multisample nil) (immutable t) (buffer-storage nil)
                       initial-contents)
  (when multisample
    (error "cepl: Multisample textures are not supported"))
  (let* ((dimensions (if initial-contents
                         (if dimensions
                             (error "Cannot specify dimensions and have non nil initial-contents")
                             (dimensions initial-contents))
                         (if dimensions dimensions (error "must specify dimensions if no initial-contents provided")))))
    (if buffer-storage
        (%make-buffer-texture dimensions internal-format mipmap layer-count cubes
                              rectangle multisample immutable initial-contents)
        ;; check for power of two - handle or warn
        (let* ((pixel-format (when initial-contents 
                               (pixel-format-of initial-contents)))
               (internal-format 
                (if internal-format
                    (if (pixel-format-p internal-format)
                        (pixel-format->internal-format internal-format)
                        internal-format)
                    (when initial-contents
                      (or (pixel-format->internal-format pixel-format)
                          (error "Could not infer the internal-format")))))
               (texture-type (establish-texture-type 
                              (if (listp dimensions) (length dimensions) 1)
                              mipmap (> layer-count 1) cubes 
                              (every #'po2p dimensions) multisample 
                              buffer-storage rectangle)))
          (if texture-type
              (if (and cubes (not (apply #'= dimensions)))
                  (error "Cube textures must be square")
                  (let ((texture (make-instance 
                                  (if (and immutable *immutable-available*) 
                                      'immutable-texture
                                      'mutable-texture)
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
              (error "This combination of texture features is invalid"))))))

(defun %make-buffer-texture (dimensions element-format mipmap layer-count cubes
                             rectangle multisample immutable initial-contents)
  (declare (ignore immutable))
  (when (gpuarray-p initial-contents)
    (error "Cannot currently make a buffer-backed texture with an existing buffer-backed gpu-array"))
  (when (typep initial-contents 'gpu-array-t)
    (error "Cannot make a buffer-backed texture with a texture-backed gpu-array"))
  (when (or mipmap (not (= layer-count 1)) cubes rectangle multisample)
    (error "Buffer-backed textures cannot have mipmaps, multiple layers or be cube rectangle or multisample"))
  (unless (or (null initial-contents) (typep initial-contents 'c-array))
    (error "Invalid initial-contents for making a buffer-backed texture"))
  (let* ((internal-format (%find-tex-internal-format 
                           (if initial-contents
                               (element-type initial-contents)
                               element-format)))
         (element-type (if initial-contents
                           (element-type initial-contents)
                           (internal-format->element-type internal-format)))
         (texture-type (establish-texture-type 
                        (if (listp dimensions) (length dimensions) 1)
                        nil nil nil (every #'po2p dimensions) nil t nil)))
    (unless (valid-internal-format-for-buffer-backed-texturep internal-format)
      (error "Invalid internal format for use with buffer-backed-texture"))
    (unless (eq texture-type :texture-buffer)
      (error "Could not establish the correct texture type for a buffer texture: ~a"
             texture-type))
    (let* ((array (or (make-gpu-array initial-contents)
                      (make-gpu-array nil :dimensions dimensions 
                                      :element-type element-type)))
           (new-tex (make-instance 
                     'buffer-texture
                     :texture-id (gen-texture)
                     :base-dimensions dimensions
                     :texture-type texture-type
                     :mipmap-levels 1
                     :layer-count 1
                     :cubes nil
                     :internal-format internal-format
                     :sampler-type (calc-sampler-type
                                    texture-type internal-format)
                     :backing-array array
                     :owns-array t)))      
      (with-texture-bound (new-tex) 
        (%gl::tex-buffer :texture-buffer internal-format
                         (glbuffer-buffer-id (gpuarray-buffer array)))
        (setf (slot-value new-tex 'allocated) t)
        new-tex))))

(defun %find-tex-internal-format (element-format)
  (if (pixel-format-p element-format)
      (pixel-format->internal-format element-format)      
      (let ((pfo (pixel-format-of element-format)))
        (if pfo
            (pixel-format->internal-format pfo)
            element-format))))

(defgeneric allocate-texture (texture))

(defmethod allocate-texture ((texture buffer-texture))
  (error "This function should not have been called with a buffer backed texture"))

(defmethod allocate-texture ((texture mutable-texture))
  (gl:tex-parameter (texture-type texture) :texture-base-level 0)
  (gl:tex-parameter (texture-type texture) :texture-max-level 
                    (1- (slot-value texture 'mipmap-levels)))
  (setf (slot-value texture 'allocated) t))

(defmethod allocate-texture ((texture immutable-texture))
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

;;------------------------------------------------------------

(defmethod gl-push ((object c-array) (destination gl-texture))
  (gl-push object (texref destination)))

;; [TODO] gl-push taking lists

;; [TODO] This feels like could create non-optimal solutions
;;        So prehaps this should look at texture format, and
;;        find the most similar compatible format, with worst
;;        case being just do what we do below
(defmethod gl-push ((object c-array) (destination gpu-array-t))
  (destructuring-bind (pformat ptype)
      (compile-pixel-format (pixel-format-of object))
    (upload-c-array-to-gpuarray-t destination object
                                  pformat ptype)))

(defmethod gl-pull ((object gl-texture))
  (gl-pull (texref object)))

;; [TODO] implement gl-fill and fill arguments

;; [TODO] Alignment
;; [TODO] Does not respect GL_PIXEL_PACK/UNPACK_BUFFER
(defmethod gl-pull-1 ((object gpu-array-t))
  (with-slots (layer-num level-num texture-type face-num 
                         internal-format texture) object
    (let* ((p-format (internal-format->pixel-format 
                      (internal-format object)))
           (c-array (make-c-array (dimensions object) p-format)))
      (destructuring-bind (format type) (compile-pixel-format p-format)
        (with-texture-bound (texture)
          (%gl:get-tex-image texture-type level-num format type
                             (pointer c-array))))
      c-array)))

;; [TODO] With-c-array is wrong
(defmethod gl-pull ((object gpu-array-t))
  (with-c-array (c-array (gl-pull-1 object))
    (gl-pull-1 c-array)))

(defmethod backed-by ((object gpu-array-t))
  :texture)

(defun unbind-texture (type)
  (gl:bind-texture type 0))

;; [TODO] No keeping trackof anything
(defun bind-texture (texture &optional type)
  (let ((texture-type (slot-value texture 'texture-type)))
    (if (or (null type) (eq type texture-type))
        (gl:bind-texture texture-type (texture-id texture))
        (if (eq :none texture-type)
            (progn (gl:bind-texture type (texture-id texture))
                   (setf (slot-value texture 'texture-type) type))
            (error "Texture has already been bound"))))
  texture)

;; {TODO}
;; copy data (from gpu to cpu) - get-tex-image
;; copy data (from frame-buffer to texture image) - leave for now
;; copy from buffer to texture glCopyTexSubImage2D
;; set texture params
;; get texture params
;; texture views
;; generate-mipmaps
;; texsubimage*d - pushing data
;; glPixelStore — set pixel storage modes


