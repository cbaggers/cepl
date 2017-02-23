(in-package :cepl.textures)

;;------------------------------------------------------------

(defmethod print-object ((object texture) stream)
    (if (initialized-p object)
        (let ((m (texture-mipmap-levels object))
              (l (texture-layer-count object)))
          (format stream
                  "#<~a (~{~a~^x~})~@[ mip-levels:~a~]~@[ layers:~a~]>"
                  (texture-type object)
                  (texture-base-dimensions object)
                  (when (> m 1) m)
                  (when (> l 1) l)))
        (format stream "#<TEXTURE :UNINITIALIZED>")))

(defmethod print-object ((object buffer-texture) stream)
  (if (initialized-p object)
      (format stream
              "#<~a (~{~a~^x~})>"
              (texture-type object)
              (texture-base-dimensions object))
      (format stream "#<TEXTURE :UNINITIALIZED>")))

(defun texture-element-type (texture)
  (texture-image-format texture))

(defmethod element-type ((texture texture))
  (texture-image-format texture))

(defmethod free ((object texture))
  (free-texture object))

(defun blank-texture-object (texture)
  (setf (texture-id texture) -1
        (texture-base-dimensions texture) nil
        (texture-type texture) nil
        (texture-image-format texture) nil
        (texture-mipmap-levels texture) 0
        (texture-layer-count texture) 0
        (texture-cubes-p texture) nil
        (texture-allocated-p texture) nil))

(defgeneric free-texture (texture))

(defmethod free-texture ((texture texture))
  (with-foreign-object (id :uint)
    (setf (mem-ref id :uint) (texture-id texture))
    (setf (texture-id texture) -1)
    (%gl:delete-textures 1 id)
    t))

(defmethod free-texture ((texture buffer-texture))
  (with-foreign-object (id :uint)
    (when (buffer-texture-owns-array texture)
      (free (buffer-texture-backing-array texture))
      (setf (buffer-texture-owns-array texture) nil)
      (setf (buffer-texture-backing-array texture)
            +null-buffer-backed-gpu-array+))
    (setf (mem-ref id :uint) (texture-id texture))
    (setf (texture-id texture) -1)
    (%gl:delete-textures 1 id)))

;;------------------------------------------------------------

(defmethod print-object ((object gpu-array-t) stream)
  (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by ~s>"
          (gpu-array-t-image-format object)
          (gpu-array-dimensions object)
          (if (eq (gpu-array-t-image-format object) :gl-internal)
              :internal
              :texture)))

(defmethod free ((object gpu-array-t))
  (declare (ignore object))
  (free-gpu-array-t))

(defmethod element-type ((gpu-array gpu-array-t))
  (gpu-array-t-image-format gpu-array))

(defmethod cepl.gpu-arrays:free-gpu-array ((gpu-array gpu-array-t))
  (declare (ignore gpu-array))
  (free-gpu-array-t))

(defun free-gpu-array-t ()
  (error "Cannot free a texture backed gpu-array. free the texture containing this array "))

(defmacro with-gpu-array-t (gpu-array-t &body body)
  (let ((arr (gensym "gpu-array-t"))
        (texture (symb :texture))
        (texture-type (symb :texture-type))
        (dimensions (symb :dimensions))
        (level-num (symb :level-num))
        (layer-num (symb :layer-num))
        (face-num (symb :face-num))
        (image-format (symb :image-format)))
    `(let ((,arr ,gpu-array-t))
       (symbol-macrolet
           ((,texture (gpu-array-t-texture ,arr))
            (,texture-type (gpu-array-t-texture-type ,arr))
            (,dimensions (gpu-array-dimensions ,arr))
            (,level-num (gpu-array-t-level-num ,arr))
            (,layer-num (gpu-array-t-layer-num ,arr))
            (,face-num (gpu-array-t-face-num ,arr))
            (,image-format (gpu-array-t-image-format ,arr)))
         ,@body))))

;;------------------------------------------------------------

;; [TODO] use with safe-exit thingy?
(defmacro with-texture-bound (texture &body body)
  (alexandria:with-gensyms (tex old-id cache-id)
    `(let* ((,tex ,texture)
            (,cache-id (texture-cache-id ,tex))
            (,old-id (cepl.context::texture-bound-id
                      *cepl-context* ,cache-id)))
       (cepl.context::set-texture-bound-id
        *cepl-context* ,cache-id (texture-id ,tex))
       (unwind-protect (progn ,@body)
         (cepl.context::set-texture-bound-id
          *cepl-context* ,cache-id ,old-id)))))
