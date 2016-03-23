(in-package :cepl.textures)

(defclass gl-texture ()
  ((texture-id :initarg :texture-id :reader texture-id)
   (base-dimensions :initarg :base-dimensions :accessor base-dimensions)
   (texture-type :initarg :texture-type :reader texture-type)
   (internal-format :initarg :internal-format :reader internal-format)
   (sampler-type :initarg :sampler-type :reader sampler-type)
   (mipmap-levels :initarg :mipmap-levels)
   (layer-count :initarg :layer-count)
   (cubes :initarg :cubes)
   (allocated :initform nil :reader allocatedp)
   (sampler-object-id :initform 0)))

(defun texture-base-dimensions (texture)
  (slot-value texture 'base-dimensions))

(defun texture-internal-format (texture)
  (slot-value texture 'internal-format))

(defun texture-sampler-type (texture)
  (slot-value texture 'sampler-type))

(defun texture-mipmap-levels (texture)
  (slot-value texture 'mipmap-levels))

(defun texture-layer-count (texture)
  (slot-value texture 'layer-count))

(defun texture-cubes (texture)
  (slot-value texture 'cubes))

(defun texture-allocated (texture)
  (slot-value texture 'allocated))

(defun texture-sampler-object-id (texture)
  (slot-value texture 'sampler-object-id))


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
            "#<GL-~a (~{~a~^x~})~@[ mip-levels:~a~]~@[ layers:~a~]~@[ cubes:~a~]>"
            (slot-value object 'texture-type)
            (slot-value object 'base-dimensions)
            (when (> m 1) m) (when (> l 1) l)
	    c)))

(defmethod print-object ((object immutable-texture) stream)
  (let ((m (slot-value object 'mipmap-levels))
        (l (slot-value object 'layer-count))
        (c (slot-value object 'cubes)))
    (format stream
            "#<GL-~a (~{~a~^x~})~@[ mip-levels:~a~]~@[ layers:~a~]~@[ cubes:~a~]>"
            (slot-value object 'texture-type)
            (slot-value object 'base-dimensions)
            (when (> m 1) m)
	    (when (> l 1) l)
	    c)))

(defmethod print-object ((object buffer-texture) stream)
  (format stream
          "#<GL-~a (~{~a~^x~})>"
          (slot-value object 'texture-type)
          (slot-value object 'base-dimensions)))

(defmethod free ((object gl-texture))
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
    (%gl:delete-textures 1 id)
    t))

(defmethod free-texture ((texture buffer-texture))
  (with-foreign-object (id :uint)
    (with-slots (owns-array backing-array) texture
      (when owns-array
        (free backing-array)
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

(defmethod backed-by ((object gpu-array-t)) :texture)

(defmethod initialize-instance :after
    ((array gpu-array-t) &key texture texture-type internal-format)
  (when (null texture-type)
    (setf (slot-value array 'texture-type) (texture-type texture)))
  (when (null internal-format)
    (setf (slot-value array 'internal-format) (internal-format texture))))

(defmethod print-object ((object gpu-array-t) stream)
  (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by ~s>"
          (internal-format object)
          (dimensions object)
          (if (eq (internal-format object) :gl-internal)
              :internal
              :texture)))

(defmethod free ((object gpu-array-t))
  (declare (ignore object))
  (free-gpu-array-t))

(defmethod free-gpu-array ((gpu-array gpu-array-t))
  (declare (ignore gpu-array))
  (free-gpu-array-t))

(defun free-gpu-array-t ()
  (error "Cannot free a texture backed gpu-array. free the texture containing this array "))

;; [TODO] use with safe-exit thingy?
(defmacro with-texture-bound ((texture &optional type) &body body)
  (let ((tex (gensym "texture"))
        (res (gensym "result")))
    `(let ((,tex ,texture))
       (bind-texture ,tex ,type)
       (let ((,res (progn ,@body)))
         (unbind-texture (slot-value ,tex 'texture-type))
         ,res))))


(defun bind-texture (texture &optional type)
  (let ((texture-type (slot-value texture 'texture-type)))
    (if (or (null type) (eq type texture-type))
        (gl:bind-texture texture-type (texture-id texture))
        (if (eq :none texture-type)
            (progn (gl:bind-texture type (texture-id texture))
                   (setf (slot-value texture 'texture-type) type))
            (error "Texture has already been bound"))))
  texture)

(defun unbind-texture (type)
  (gl:bind-texture type 0))
