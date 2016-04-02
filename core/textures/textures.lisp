(in-package :cepl.textures)

;;------------------------------------------------------------

(defvar *immutable-available* t)
(defvar *cube-face-order* '(:texture-cube-map-positive-x
                            :texture-cube-map-negative-x
                            :texture-cube-map-positive-y
                            :texture-cube-map-negative-y
                            :texture-cube-map-positive-z
                            :texture-cube-map-negative-z))

(defun check-immutable-feature ()
  (unless (has-feature "GL_ARB_texture_storage")
    (setf *immutable-available* nil)))

(push #'check-immutable-feature *on-context*)

;;------------------------------------------------------------

(defun texref (texture &key (mipmap-level 0) (layer 0) (cube-face 0))
  (when (and (> cube-face 0) (not (texture-cubes-p texture)))
    (error "Cannot get the cube-face from a texture that wasnt made with :cubes-p t:~%~a" texture))
  (if (eq (texture-type texture) :texture-buffer)
      (if (> (+ mipmap-level layer cube-face) 0)
          (error "Texture index out of range")
          (buffer-texture-backing-array texture))
      (if (valid-index-p texture mipmap-level layer cube-face)
          (%make-gpu-array-t
	   :texture texture
	   :texture-type (texture-type texture)
	   :level-num mipmap-level
	   :layer-num layer
	   :face-num cube-face
	   :dimensions (dimensions-at-mipmap-level
			texture mipmap-level)
	   :image-format (texture-image-format texture))
          (error "Texture index out of range"))))

(defun valid-index-p (texture mipmap-level layer cube-face)
  (and (< mipmap-level (texture-mipmap-levels texture))
       (< layer (texture-layer-count texture))
       (if (texture-cubes-p texture)
	   (<= cube-face 6)
	   (= 0 cube-face))))

;;------------------------------------------------------------

(defun generate-mipmaps (texture)
  (let ((type (texture-type texture)))
    (with-texture-bound texture
      (gl:generate-mipmap type))))

(defun error-on-invalid-upload-formats (target image-format pixel-format pixel-type)
  (unless (and image-format pixel-type pixel-format)
    (error "Could not establish all the required formats for the pixel transfer"))
  (when (eq target :texture-buffer)
    (error "You should not have reached this function as buffer backed textures have buffer-gpu-arrays as their backing stores"))
  (when (and (find image-format '(:depth-component :depth-component16
                                     :depth-component24 :depth-component32f))
             (not (find target '(:texture_2d :proxy_texture_2d
                                 :texture_rectangle
                                 :proxy_texture_rectangle))))
    (error "Texture type is ~a. Cannot populate with ~a"
           target image-format))
  (when (and (eq pixel-format :depth-component)
             (not (find image-format
                        '(:depth-component :depth-component16
                          :depth-component24 :depth-component32f))))
    (error "Pixel data is a depth format however the texture is not"))
  (when (and (not (eq pixel-format :depth-component))
             (find image-format
                   '(:depth-component :depth-component16
                     :depth-component24 :depth-component32f)))
    (error "Pixel data is a depth format however the texture is not"))
  t)

(defun upload-c-array-to-gpu-array-t (gpu-array c-array &optional format type)
  ;; if no format or type
  (when (or (and format (not type)) (and type (not format)))
    (error "cannot only specify either format or type, must be both or neither"))
  (let* ((element-pf (lisp-type->pixel-format c-array))
         (compiled-pf (cepl.pixel-formats::compile-pixel-format element-pf))
         (pix-format (or format (first compiled-pf)))
         (pix-type (or type (second compiled-pf))))
    (with-gpu-array-t gpu-array
      (error-on-invalid-upload-formats texture-type image-format pix-format
				       pix-type)
      (unless (equal (dimensions c-array) dimensions)
	(error "dimensions of c-array and gpu-array must match~%c-array:~a gpu-array:~a" (dimensions c-array) dimensions))
      (with-texture-bound (gpu-array-t-texture gpu-array)
	(%upload-tex texture texture-type level-num (dimensions c-array)
		     layer-num face-num pix-format pix-type (pointer c-array)))))
  gpu-array)

;; [TODO] add offsets
(defun %upload-tex (tex tex-type level-num dimensions layer-num face-num
		    pix-format pix-type pointer)
  (if (texture-mutable-p tex)
      (%upload-to-mutable-tex tex tex-type level-num dimensions layer-num
			      face-num pix-format pix-type pointer)
      (%upload-to-immutable-tex tex tex-type level-num dimensions layer-num
				face-num pix-format pix-type pointer)))

(defun %upload-to-mutable-tex (tex tex-type level-num dimensions layer-num
			       face-num pix-format pix-type pointer)
  (case tex-type
    (:texture-1d (gl:tex-image-1d
		  tex-type level-num (texture-image-format tex)
		  (first dimensions) 0 pix-format pix-type
		  pointer))
    (:texture-2d (gl:tex-image-2d
		  tex-type level-num (texture-image-format tex)
		  (first dimensions) (second dimensions) 0
		  pix-format pix-type pointer))
    (:texture-3d (gl:tex-image-3d
		  tex-type level-num (texture-image-format tex)
		  (first dimensions) (second dimensions)
		  (third dimensions) 0 pix-format pix-type
		  pointer))
    (:texture-1d-array (gl:tex-image-2d
			tex-type level-num
			(texture-image-format tex)
			(first dimensions) layer-num 0
			pix-format pix-type pointer))
    (:texture-2d-array (gl:tex-image-3d
			tex-type level-num
			(texture-image-format tex)
			(first dimensions) (second dimensions)
			layer-num 0 pix-format pix-type pointer))
    (:texture-cube-map (gl:tex-image-2d
			(nth face-num *cube-face-order*)
			level-num (texture-image-format tex)
			(first dimensions) (second dimensions) 0
			pix-format pix-type pointer))
    (t (error "not currently supported for upload: ~a" tex-type))))


(defun %upload-to-immutable-tex (tex tex-type level-num dimensions layer-num
				 face-num pix-format pix-type pointer)
  (declare (ignore tex))
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

(defun upload-from-buffer-to-gpu-array-t (&rest args)
  (declare (ignore args))
  (error "upload-from-buffer-to-gpu-array-t is not implemented yet"))

;;------------------------------------------------------------

(defvar *mipmap-max-levels* 20)
(defvar *valid-texture-storage-options*
  ;; mipmap layers cubes dimensions multisample buffer rectangle
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

;;------------------------------------------------------------

(defun gen-texture ()
  (first (gl:gen-textures 1)))

(defun po2p (x) (eql 0 (logand x (- x 1))))

(defun dimensions-at-mipmap-level (texture level)
  (if (= level 0)
      (texture-base-dimensions texture)
      (let ((div (* 2 (1+ level))))
        (loop for i in (texture-base-dimensions texture) collecting
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
;; {TODO} Texture sizes have a limit based on the GL
;;        implementation. For 1D and 2D textures (and any texture types that
;;        use similar dimensionality, like cubemaps) the max size of either
;;        dimension is GL_MAX_TEXTURE_SIZE​. For array textures, the maximum
;;        array length is GL_MAX_ARRAY_TEXTURE_LAYERS​. For 3D textures, no
;;        dimension can be greater than GL_MAX_3D_TEXTURE_SIZE​ in size.


;; [TODO] how does mipmap-max affect this
;; [TODO] what is the max layercount?
;; [TODO] should fail on layer-count=0?
(defun %texture-dimensions (initial-contents dimensions)
  (if initial-contents
      (if dimensions
          (error "Cannot specify dimensions and initial-contents")
          (dimensions initial-contents))
      (if dimensions dimensions (error "must specify dimensions if no initial-contents provided"))))

(defun make-texture-from-id (gl-object &key base-dimensions texture-type
                                         element-type sampler-type
                                         mipmap-levels layer-count cubes
                                         allocated mutable-p)
  (%%make-texture
   :id gl-object
   :base-dimensions base-dimensions
   :type texture-type
   :image-format element-type
   :sampler-type sampler-type
   :mipmap-levels mipmap-levels
   :layer-count layer-count
   :cubes-p cubes
   :allocated-p allocated
   :mutable-p mutable-p))


(defun make-texture (initial-contents
                     &key dimensions element-type (mipmap nil)
                       (layer-count 1) (cubes nil) (rectangle nil)
                       (multisample nil) (immutable t) (buffer-storage nil)
                       (generate-mipmaps t))
  (cepl.memory::if-context
   (make-texture-now %pre% initial-contents dimensions element-type mipmap
		     layer-count cubes rectangle multisample immutable
		     buffer-storage generate-mipmaps)
   (make-uninitialized-texture)
   (when (typep initial-contents 'gpu-array-bb)
     (list initial-contents))))

(defun make-texture-now (tex-obj initial-contents dimensions element-type mipmap
			 layer-count cubes rectangle multisample immutable
			 buffer-storage generate-mipmaps)
  ;;
  (let ((element-type (expand-gl-type-name element-type))
	(image-format (calc-image-format element-type initial-contents)))
    (cond
      ;; ms
      (multisample (error "cepl: Multisample textures are not supported"))
      ;; cube textures
      ((and initial-contents cubes)
       (%make-cube-texture tex-obj dimensions mipmap layer-count cubes
			   buffer-storage rectangle multisample immutable
			   initial-contents image-format generate-mipmaps))
      ;; initialize content needs to be turned into c-array
      ((and initial-contents (typep initial-contents 'uploadable-lisp-seq))
       (%make-texture-with-lisp-data tex-obj dimensions mipmap layer-count cubes
                                     buffer-storage rectangle multisample
                                     immutable initial-contents generate-mipmaps
				     element-type image-format))
      ;; buffer backed - note that by now if there were intitial contents, they
      ;;                 are now a c-array
      (buffer-storage
       (%make-buffer-texture tex-obj
			     (%texture-dimensions initial-contents dimensions)
                             image-format mipmap layer-count cubes
                             rectangle multisample immutable initial-contents))
      ;; all other cases
      (t (%make-texture tex-obj dimensions mipmap layer-count cubes
			buffer-storage rectangle multisample immutable
			initial-contents image-format generate-mipmaps)))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun calc-dimensions (image-format dimensions cube-tex)
  (declare (ignore image-format dimensions cube-tex)))

;; other
;; (listify dimensions)
;;cube
;; (let* ((target-dim (or dimensions (dimensions (first initial-contents))))
;;        (dim (if (every (lambda (_) (equal target-dim (dimensions _)))
;;                        initial-contents)
;;                 target-dim
;;                 (error "Conflicting dimensions of c-arrays passed to make-texture with :cube t:~%~a"
;;                        initial-contents)))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun calc-image-format (element-type initial-contents)
  (cond
    ;; need to infer the type
    ((null element-type)
     (%calc-image-format-without-declared-format initial-contents))
    ;; need to ensure nothing conflicts with declaration
    ((image-formatp element-type)
     (%calc-image-format-with-declared-format element-type
                                                 element-type
                                                 initial-contents))
    ;; need to ensure nothing conflicts with declaration
    ((pixel-format-p element-type)
     (%calc-image-format-with-pixel-format element-type
                                              initial-contents))
    ;;
    (t (%calc-image-format-with-lisp-type element-type
                                             initial-contents))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun %calc-image-format-without-declared-format (initial-contents)
  ;; The user didnt declare an element-type so try and infer one
  (typecase initial-contents
    (null (error 'make-tex-no-content-no-type))
    (c-array (lisp-type->image-format
              (element-type initial-contents)))
    (uploadable-lisp-seq (lisp-type->image-format
			  (cepl.c-arrays::lisp->gl-type
			   (cepl.c-arrays::scan-for-type initial-contents))))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun %calc-image-format-with-declared-format
    (element-type image-format initial-contents)
  ;; Here the users declared the internal format in :element-type so
  ;; we need to make sure that no other arguments conflict with this
  (typecase initial-contents
    (null image-format)
    (c-array (if (equal (lisp-type->image-format
                         (element-type initial-contents))
                        image-format)
                 image-format
                 (error 'make-tex-array-not-match-type
                        :element-type element-type
                        :image-format image-format
                        :array-type (element-type initial-contents))))
    (uploadable-lisp-seq image-format) ;; we cant infer all types so we
    ;; have to trust and then the
    ;; c-array code handle it
    (t (error 'make-tex-array-not-match-type2
              :element-type element-type
              :initial-contents initial-contents))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun %calc-image-format-with-pixel-format (pixel-format initial-contents)
  "Convert the pixel-format to an internal format and delegate to
   %calc-image-format"
  (%calc-image-format-with-declared-format
   pixel-format
   (pixel-format->image-format pixel-format)
   initial-contents))

(defun %calc-image-format-with-lisp-type (element-type initial-contents)
  "Convert the lisp type to an internal format and delegate to
   %calc-image-format"
  (%calc-image-format-with-declared-format
   element-type
   (lisp-type->image-format element-type)
   initial-contents))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun %make-texture-with-lisp-data
    (tex-obj dimensions mipmap layer-count cubes buffer-storage
     rectangle multisample immutable initial-contents
     generate-mipmaps element-type image-format)
  (declare (ignore element-type))
  (let ((element-type (image-format->lisp-type image-format)))
    (with-c-array (tmp (make-c-array initial-contents :dimensions dimensions
				     :element-type element-type))
      (make-texture-now tex-obj tmp nil nil mipmap layer-count cubes rectangle
			multisample immutable buffer-storage
			generate-mipmaps))))

(defun %make-cube-texture (tex-obj dimensions mipmap layer-count cubes buffer-storage
                           rectangle multisample immutable initial-contents
                           image-format generate-mipmaps)
  (assert (= 6 (length initial-contents)))
  (let* ((target-dim (or dimensions (dimensions (first initial-contents))))
         (dim (if (every (lambda (_) (equal target-dim (dimensions _)))
                         initial-contents)
                  target-dim
                  (error "Conflicting dimensions of c-arrays passed to make-texture with :cube t:~%~a"
                         initial-contents)))
         (result (%make-texture tex-obj dim mipmap layer-count cubes
				buffer-storage rectangle multisample immutable
				nil image-format generate-mipmaps)))
    (loop :for data :in initial-contents :for i :from 0 :do
       (push-g data (texref result :cube-face i)))
    result))

(defun %make-texture (tex-obj dimensions mipmap layer-count cubes buffer-storage
                      rectangle multisample immutable initial-contents
                      image-format generate-mipmaps)
  (let* ((dimensions (listify dimensions))
	 (dimensions (%texture-dimensions initial-contents dimensions)))
    ;; check for power of two - handle or warn
    (let* ((pixel-format (when initial-contents
                           (lisp-type->pixel-format initial-contents)))
           (texture-type (establish-texture-type
                          (if (listp dimensions) (length dimensions) 1)
                          (not (null mipmap)) (> layer-count 1) cubes
                          (every #'po2p dimensions) multisample
                          buffer-storage rectangle))
           (mipmap-levels (let ((max-levels (floor (log (apply #'max dimensions) 2))))
                            (if (typep mipmap 'integer)
                                (if (<= mipmap max-levels)
                                    mipmap
                                    (error "Invalid number of mipmap levels specified (~a) for dimensions ~a"
                                           mipmap dimensions))
                                (if mipmap
                                    max-levels
                                    1)))))
      (if texture-type
          (if (and cubes (not (apply #'= dimensions)))
              (error "Cube textures must be square")
              (progn
		(setf (texture-id tex-obj) (gen-texture)
		      (texture-base-dimensions tex-obj) dimensions
		      (texture-type tex-obj) texture-type
		      (texture-mipmap-levels tex-obj) mipmap-levels
		      (texture-layer-count tex-obj) layer-count
		      (texture-cubes-p tex-obj) cubes
		      (texture-image-format tex-obj) image-format
		      (texture-mutable-p tex-obj) (not (and immutable *immutable-available*))
		      (texture-sampler-type tex-obj) (cepl.samplers::calc-sampler-type
						      texture-type
						      image-format))
                (with-texture-bound tex-obj
                  (allocate-texture tex-obj)
                  (when initial-contents
                    (destructuring-bind (pformat ptype)
                        (cepl.pixel-formats::compile-pixel-format pixel-format)
                      (upload-c-array-to-gpu-array-t
                       (texref tex-obj) initial-contents
                       pformat ptype)))
                  (when (and generate-mipmaps (> mipmap-levels 1))
                    (generate-mipmaps tex-obj)))
                tex-obj))
          (error "This combination of texture features is invalid")))))

;; (when (gpu-array-p initial-contents)
;;     (error "Cannot currently make a buffer-backed texture with an existing buffer-backed gpu-array"))
;;   (when (typep initial-contents 'gpu-array-t)
;;     (error "Cannot make a buffer-backed texture with a texture-backed gpu-array"))

(defun %make-buffer-texture (tex-obj dimensions image-format mipmap layer-count
			     cubes rectangle multisample immutable
			     initial-contents)
  (declare (ignore immutable))
  (let* ((dimensions (listify dimensions))
         (element-type (if initial-contents
                           (element-type initial-contents)
                           (image-format->lisp-type image-format)))
         (texture-type (establish-texture-type
                        (length dimensions)
                        nil nil nil (every #'po2p dimensions) nil t nil)))
    ;; validation
    (assert-valid-args-for-buffer-backend-texture
     image-format cubes rectangle multisample mipmap layer-count
     texture-type)
    ;; creation
    (let* ((array (if initial-contents
                      (make-gpu-array initial-contents)
                      (make-gpu-array nil :dimensions dimensions
                                      :element-type element-type))))
      (setf (texture-id tex-obj) (gen-texture)
	    (texture-base-dimensions tex-obj) dimensions
	    (texture-type tex-obj) texture-type
	    (texture-mipmap-levels tex-obj) 1
	    (texture-layer-count tex-obj) 1
	    (texture-cubes-p tex-obj) nil
	    (texture-image-format tex-obj) image-format
	    (texture-sampler-type tex-obj) (cepl.samplers::calc-sampler-type
					    texture-type image-format)
	    (buffer-texture-backing-array tex-obj) array
	    (buffer-texture-owns-array tex-obj) t)
      ;; upload
      (with-texture-bound tex-obj
        (%gl::tex-buffer :texture-buffer image-format
                         (gpu-buffer-id
			  (cepl.gpu-arrays::gpu-array-buffer array)))
        (setf (texture-allocated-p tex-obj) t)
        tex-obj))))

(defun assert-valid-args-for-buffer-backend-texture
    (image-format cubes rectangle multisample mipmap layer-count
     texture-type)
  (when (or mipmap (not (= layer-count 1)) cubes rectangle multisample)
    (error 'buffer-backed-texture-invalid-args))
  (unless (valid-image-format-for-buffer-backed-texturep image-format)
    (error 'buffer-backed-texture-invalid-image-format
           :type-name image-format))
  (unless (eq texture-type :texture-buffer)
    (error 'buffer-backed-texture-establish-image-format
           :type-name texture-type)))

(defun %find-tex-image-format (element-format)
  (if (pixel-format-p element-format)
      (pixel-format->image-format element-format)
      (let ((pfo (lisp-type->pixel-format element-format)))
        (if pfo
            (pixel-format->image-format pfo)
            element-format))))

(defun allocate-texture (texture)
  (when (buffer-texture-p texture)
    (error "This function should not have been called with a buffer backed texture"))
  (if (texture-mutable-p texture)
      (allocate-mutable-texture texture)
      (allocate-immutable-texture texture)))

(defun allocate-mutable-texture (texture)
  (gl:tex-parameter (texture-type texture) :texture-base-level 0)
  (gl:tex-parameter (texture-type texture) :texture-max-level
                    (1- (texture-mipmap-levels texture)))
  (setf (texture-allocated-p texture) t))

(defun allocate-immutable-texture (texture)
  (if (texture-allocated-p texture)
      (error "Attempting to reallocate a previously allocated texture")
      (let ((base-dimensions (texture-base-dimensions texture))
            (texture-type (texture-type texture)))
        (case texture-type
          ((:texture-1d :proxy-texture-1d)
           (tex-storage-1d texture-type
                           (texture-mipmap-levels texture)
                           (texture-image-format texture)
                           (first base-dimensions)))
          ((:texture-2d :proxy-texture-2d :texture-1d-array :texture-rectangle
                        :proxy-texture-rectangle :texture-cube-map
                        :proxy-texture-cube-map :proxy-texture-1d-array)
           (tex-storage-2d texture-type
                           (texture-mipmap-levels texture)
                           (texture-image-format texture)
                           (first base-dimensions)
                           (or (second base-dimensions) 0)))
          ((:texture-3d :proxy-texture-3d :texture-2d-array :texture-cube-array
                        :proxy-texture-cube-array :proxy-texture-2d-array)
           (tex-storage-3d texture-type
                           (texture-mipmap-levels texture)
                           (texture-image-format texture)
                           (first base-dimensions)
                           (or (second base-dimensions) 0)
                           (or (third base-dimensions) 0))))
        (setf (texture-allocated-p texture) t))))

(defun tex-storage-1d (target levels image-format width)
  (%gl:tex-storage-1d target levels (gl::internal-format->int image-format)
                      width))

(defun tex-storage-2d (target levels image-format width height)
  (%gl:tex-storage-2d target levels (gl::internal-format->int image-format)
                      width height))

(defun tex-storage-3d (target levels image-format width height depth)
  (%gl:tex-storage-3d target levels (gl::internal-format->int image-format)
                      width height depth))

;;------------------------------------------------------------

(defmethod push-g ((object c-array) (destination texture))
  (push-g object (texref destination)))
(defmethod push-g ((object list) (destination texture))
  (push-g object (texref destination)))
(defmethod push-g ((object array) (destination texture))
  (push-g object (texref destination)))

(defmethod push-g ((object list) (destination gpu-array-t))
  (with-c-array (c-a (make-c-array object
                                   :dimensions (dimensions destination)
                                   :element-type (image-format->pixel-format
                                                  (gpu-array-t-image-format destination))))
    (push-g c-a destination)))

(defmethod push-g ((object array) (destination gpu-array-t))
  (with-c-array (c-a (make-c-array object
                                   :dimensions (dimensions destination)
                                   :element-type (image-format->pixel-format
                                                  (gpu-array-t-image-format destination))))
    (push-g c-a destination)))

;; [TODO] This feels like could create non-optimal solutions
;;        So prehaps this should look at texture format, and
;;        find the most similar compatible format, with worst
;;        case being just do what we do below
(defmethod push-g ((object c-array) (destination gpu-array-t))
  (destructuring-bind (pformat ptype)
      (cepl.pixel-formats::compile-pixel-format (lisp-type->pixel-format object))
    (upload-c-array-to-gpu-array-t destination object
                                  pformat ptype)))

(defmethod pull-g ((object texture))
  (pull-g (texref object)))

;; [TODO] implement gl-fill and fill arguments
;; [TODO] Does not respect GL_PIXEL_PACK/UNPACK_BUFFER
(defmethod pull1-g ((object gpu-array-t))
  (with-gpu-array-t object
    (let* ((p-format (image-format->pixel-format
                      (gpu-array-t-image-format object)))
           (c-array (make-c-array nil :dimensions (dimensions object)
                                  :element-type p-format)))
      (destructuring-bind (format type)
	  (cepl.pixel-formats::compile-pixel-format p-format)
        (with-texture-bound texture
          (%gl:get-tex-image (foreign-enum-value '%gl:enum texture-type)
			     (coerce level-num 'real)
			     format
			     type
                             (pointer c-array))))
      c-array)))

(defmethod pull1-g ((object texture))
  (pull1-g (texref object)))

;; [TODO] With-c-array is wrong
(defmethod pull-g ((object gpu-array-t))
  (with-c-array (c-array (pull1-g object))
    (pull1-g c-array)))



(defun active-texture-num (num)
  (gl:active-texture (+ #x84C0 num)))

;; {TODO}
;; copy data (from frame-buffer to texture image) - leave for now
;; copy from buffer to texture glCopyTexSubImage2D
;; set texture params
;; get texture params
;; texture views
;; generate-mipmaps
;; texsubimage*d - pushing data
;; glPixelStore — set pixel storage modes
