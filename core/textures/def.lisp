(in-package :cepl.textures)

;;------------------------------------------------------------

(defmethod print-object ((object texture) stream)
  (let ((m (texture-mipmap-levels object))
        (l (texture-layer-count object))
        (c (texture-cubes-p object)))
    (format stream
            "#<GL-~a (~{~a~^x~})~@[ mip-levels:~a~]~@[ layers:~a~]~@[ cubes:~a~]>"
            (texture-type object)
            (texture-base-dimensions object)
            (when (> m 1) m)
	    (when (> l 1) l)
	    c)))

(defmethod print-object ((object buffer-texture) stream)
  (format stream
          "#<GL-~a (~{~a~^x~})>"
          (texture-type object)
          (texture-base-dimensions object)))

(defmethod free ((object texture))
  (free-texture object))

(defun blank-texture-object (texture)
  (setf (texture-id texture) -1
	(texture-base-dimensions texture) nil
	(texture-type texture) nil
	(texture-internal-format texture) nil
	(texture-sampler-type texture) nil
	(texture-mipmap-levels texture) 0
	(texture-layer-count texture) 0
	(texture-cubes-p texture) nil
	(texture-allocated-p texture) nil))

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
          (gpu-array-t-internal-format object)
          (gpu-array-dimensions object)
          (if (eq (gpu-array-t-internal-format object) :gl-internal)
              :internal
              :texture)))

(defmethod free ((object gpu-array-t))
  (declare (ignore object))
  (free-gpu-array-t))

(defmethod element-type ((gpu-array gpu-array-t))
  (gpu-array-t-internal-format gpu-array))

(defmethod free-gpu-array ((gpu-array gpu-array-t))
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
	(internal-format (symb :internal-format)))
    `(let ((,arr ,gpu-array-t))
       (symbol-macrolet
	   ((,texture (gpu-array-t-texture ,arr))
	    (,texture-type (gpu-array-t-texture-type ,arr))
	    (,dimensions (gpu-array-dimensions ,arr))
	    (,level-num (gpu-array-t-level-num ,arr))
	    (,layer-num (gpu-array-t-layer-num ,arr))
	    (,face-num (gpu-array-t-face-num ,arr))
	    (,internal-format (gpu-array-t-internal-format ,arr)))
	 ,@body))))

;;------------------------------------------------------------

;; [TODO] use with safe-exit thingy?
(defmacro with-texture-bound ((texture &optional type) &body body)
  (let ((tex (gensym "texture"))
        (res (gensym "result")))
    `(let ((,tex ,texture))
       (bind-texture ,tex ,type)
       (let ((,res (progn ,@body)))
         (unbind-texture (texture-type ,tex))
         ,res))))


(defun bind-texture (texture &optional type)
  (let ((texture-type (texture-type texture)))
    (if (or (null type) (eq type texture-type))
        (gl:bind-texture texture-type (texture-id texture))
        (if (eq :none texture-type)
            (progn (gl:bind-texture type (texture-id texture))
                   (setf (texture-type texture) type))
            (error "Texture has already been bound"))))
  texture)

(defun unbind-texture (type)
  (gl:bind-texture type 0))
