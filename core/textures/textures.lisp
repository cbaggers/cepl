(in-package :cepl.textures)

;;------------------------------------------------------------

(defvar *immutable-available* t)
(defvar *cube-face-order* '(:texture-cube-map-positive-x
                            :texture-cube-map-negative-x
                            :texture-cube-map-positive-y
                            :texture-cube-map-negative-y
                            :texture-cube-map-positive-z
                            :texture-cube-map-negative-z))

(defun2 check-immutable-feature ()
  (unless (has-feature "GL_ARB_texture_storage")
    (setf *immutable-available* nil)))

(push #'check-immutable-feature *on-context*)

;;------------------------------------------------------------

(defun2 texref (texture &key (mipmap-level 0) (layer 0) (cube-face 0))
  (when (and (> cube-face 0) (not (texture-cubes-p texture)))
    (error "Cannot get the cube-face from a texture that wasnt made with :cubes-p t:~%~a" texture))
  (if (eq (texture-type texture) :texture-buffer)
      (if (> (+ mipmap-level layer cube-face) 0)
          (error "Texture index out of range")
          (buffer-texture-backing-array texture))
      (if (valid-index-p texture mipmap-level layer cube-face)
          (let ((result
                 (%make-gpu-array-t
                  :texture texture
                  :texture-type (texture-type texture)
                  :level-num mipmap-level
                  :layer-num layer
                  :face-num cube-face
                  :dimensions (dimensions-at-mipmap-level
                               texture mipmap-level)
                  :image-format (texture-image-format texture))))

            (when (not cepl.context:*gl-context*)
              (cepl.context::delay-initialization
               (cepl-context)
               (lambda () (reinit-on-context result))
               (list texture)))
            result)
          (error "Texture index out of range"))))

(defun2 reinit-on-context (gpu-array)
  (let ((texture (gpu-array-t-texture gpu-array)))
    (setf (gpu-array-t-texture-type gpu-array) (texture-type texture)
          (gpu-array-dimensions gpu-array) (dimensions-at-mipmap-level
                                            texture (gpu-array-t-level-num
                                                     gpu-array))
          (gpu-array-t-image-format gpu-array) (texture-image-format texture))))

(defun2 valid-index-p (texture mipmap-level layer cube-face)
  (or (= 0 mipmap-level layer cube-face)
      (and (< mipmap-level (texture-mipmap-levels texture))
           (< layer (texture-layer-count texture))
           (if (texture-cubes-p texture)
               (<= cube-face 6)
               (= 0 cube-face)))))

;;------------------------------------------------------------

(defun2 generate-mipmaps (texture)
  (let ((type (texture-type texture)))
    (with-texture-bound texture
      (%gl:generate-mipmap type))))

(defun2 error-on-invalid-upload-formats (target image-format pixel-format pixel-type)
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

(defun2 upload-c-array-to-gpu-array-t (gpu-array c-array &optional pixel-format)
  (let* ((element-pf (lisp-type->pixel-format c-array))
         (compiled-pf (or pixel-format (cepl.pixel-formats::compile-pixel-format
                                        element-pf)))
         (pix-format (first compiled-pf))
         (pix-type (second compiled-pf)))
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
(defun2 %upload-tex (tex tex-type level-num dimensions layer-num face-num
                         pix-format pix-type pointer)
  (if (texture-mutable-p tex)
      (%upload-to-mutable-tex tex tex-type level-num dimensions layer-num
                              face-num pix-format pix-type pointer)
      (%upload-to-immutable-tex tex tex-type level-num dimensions layer-num
                                face-num pix-format pix-type pointer)))

(defun2 %upload-to-mutable-tex (tex tex-type level-num dimensions layer-num
                                    face-num pix-format pix-type pointer)
  ;; border is an old (now unsupported) parameter and so is always be set to 0
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


(defun2 %upload-to-immutable-tex (tex tex-type level-num dimensions layer-num
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

(defun2 upload-from-buffer-to-gpu-array-t (&rest args)
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

(defun2 gen-texture ()
  (first (gl:gen-textures 1)))

(defun2 po2p (x) (eql 0 (logand x (- x 1))))

(defun2 dimensions-at-mipmap-level (texture level)
  (if (= level 0)
      (texture-base-dimensions texture)
      (loop :for i :in (texture-base-dimensions texture) :collect
         (/ i (expt 2 level)))))

;;------------------------------------------------------------

(defun2 establish-texture-type (dimensions mipmap layers cubes po2 multisample
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
(defun2 %texture-dimensions (initial-contents dimensions)
  (if initial-contents
      (if dimensions
          (error "Cannot specify dimensions and initial-contents")
          (dimensions initial-contents))
      (if dimensions dimensions (error "must specify dimensions if no initial-contents provided"))))

(defun2 make-texture-from-id (gl-object &key base-dimensions texture-type
                                         element-type mipmap-levels
                                         layer-count cubes allocated mutable-p)
  (assert (typep gl-object 'gl-id))
  (cepl.context::register-texture
   (cepl-context)
   (%%make-texture
    :id gl-object
    :base-dimensions base-dimensions
    :type texture-type
    :image-format element-type
    :mipmap-levels mipmap-levels
    :layer-count layer-count
    :cubes-p cubes
    :allocated-p allocated
    :mutable-p mutable-p)))


(defun2 make-texture (initial-contents
                     &key dimensions element-type (mipmap nil)
                       (layer-count 1) (cubes nil) (rectangle nil)
                       (multisample nil) (immutable t) (buffer-storage nil)
                       (generate-mipmaps t) pixel-format)
  (let ((dimensions (listify dimensions)))
    (cepl.context::if-gl-context
     (make-texture-now %pre% initial-contents dimensions element-type mipmap
                       layer-count cubes rectangle multisample immutable
                       buffer-storage generate-mipmaps pixel-format)
     (make-uninitialized-texture buffer-storage)
     (when (typep initial-contents 'gpu-array-bb)
       (list initial-contents)))))

(defun2 make-texture-now (tex-obj initial-contents dimensions element-type mipmap
                         layer-count cubes rectangle multisample immutable
                         buffer-storage generate-mipmaps pixel-format)
  ;;
  (let ((element-type (cffi-type->gl-type element-type))
        (image-format (calc-image-format element-type initial-contents)))
    (cond
      ;; ms
      (multisample (error "cepl: Multisample textures are not supported"))
      ;; cube textures
      ((and initial-contents cubes)
       (%make-cube-texture tex-obj dimensions mipmap layer-count cubes
                           buffer-storage rectangle multisample immutable
                           initial-contents image-format pixel-format
                           generate-mipmaps))
      ;; initialize content needs to be turned into c-array
      ((and initial-contents (typep initial-contents 'uploadable-lisp-seq))
       (%make-texture-with-lisp-data tex-obj dimensions mipmap layer-count cubes
                                     buffer-storage rectangle multisample
                                     immutable initial-contents generate-mipmaps
                                     element-type image-format pixel-format))
      ;; buffer backed - note that by now if there were intitial contents, they
      ;;                 are now a c-array
      (buffer-storage
       (%make-buffer-texture tex-obj
                             (%texture-dimensions initial-contents dimensions)
                             image-format mipmap layer-count cubes
                             rectangle multisample immutable initial-contents
                             pixel-format))
      ;; all other cases
      (t (%make-texture tex-obj dimensions mipmap layer-count cubes
                        buffer-storage rectangle multisample immutable
                        initial-contents image-format pixel-format
                        generate-mipmaps)))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun2 calc-dimensions (image-format dimensions cube-tex)
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

(defun2 calc-image-format (element-type initial-contents)
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

(defun2 %calc-image-format-without-declared-format (initial-contents)
  ;; The user didnt declare an element-type so try and infer one
  (typecase initial-contents
    (null (error 'make-tex-no-content-no-type))
    (c-array (lisp-type->image-format
              (element-type initial-contents)))
    (uploadable-lisp-seq (lisp-type->image-format
                          (cepl.c-arrays::lisp->gl-type
                           (cepl.c-arrays::scan-for-type initial-contents))))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun2 %calc-image-format-with-declared-format
    (element-type image-format initial-contents)
  ;; Here the users declared the internal format in :element-type so
  ;; we need to make sure that no other arguments conflict with this
  (typecase initial-contents
    (null image-format)
    (c-array image-format)
    (uploadable-lisp-seq image-format)
    (t (error 'make-tex-array-not-match-type2
              :element-type element-type
              :initial-contents initial-contents))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun2 %calc-image-format-with-pixel-format (pixel-format initial-contents)
  "Convert the pixel-format to an internal format and delegate to
   %calc-image-format"
  (%calc-image-format-with-declared-format
   pixel-format
   (pixel-format->image-format pixel-format)
   initial-contents))

(defun2 %calc-image-format-with-lisp-type (element-type initial-contents)
  "Convert the lisp type to an internal format and delegate to
   %calc-image-format"
  (%calc-image-format-with-declared-format
   element-type
   (lisp-type->image-format element-type)
   initial-contents))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun2 %make-texture-with-lisp-data
    (tex-obj dimensions mipmap layer-count cubes buffer-storage
     rectangle multisample immutable initial-contents
     generate-mipmaps element-type image-format pixel-format)
  (declare (ignore element-type))
  (let ((element-type (image-format->lisp-type image-format)))
    (with-c-array (tmp (make-c-array initial-contents :dimensions dimensions
                                     :element-type element-type))
      (make-texture-now tex-obj tmp nil nil mipmap layer-count cubes rectangle
                        multisample immutable buffer-storage
                        generate-mipmaps pixel-format))))

(defun2 %make-cube-texture (tex-obj dimensions mipmap layer-count cubes buffer-storage
                           rectangle multisample immutable initial-contents
                           image-format pixel-format generate-mipmaps)
  (assert (= 6 (length initial-contents)))
  (let* ((target-dim (or dimensions (dimensions (first initial-contents))))
         (dim (if (every (lambda (_) (equal target-dim (dimensions _)))
                         initial-contents)
                  target-dim
                  (error "Conflicting dimensions of c-arrays passed to make-texture with :cube t:~%~a"
                         initial-contents)))
         (result (%make-texture tex-obj dim mipmap layer-count cubes
                                buffer-storage rectangle multisample immutable
                                nil image-format pixel-format generate-mipmaps)))
    (loop :for data :in initial-contents :for i :from 0 :do
       (push-g data (texref result :cube-face i)))
    result))


(defun2 validate-pixel-format (initial-contents pixel-format)
  (let ((element-type (element-type initial-contents))
        (supposed-type (pixel-format->lisp-type pixel-format)))
    (if (equal element-type supposed-type)
        pixel-format
        (error 'make-tex-array-not-match-type
               :element-type element-type
               :pixel-format pixel-format
               :supposed-type supposed-type
               :array-type (element-type initial-contents)))))

(defgeneric check-mipmap-level-count-valid (level-count dimensions))

(defmethod check-mipmap-level-count-valid ((level-count integer)
                                           (dimensions list))
  (let ((max-levels (calc-max-num-mipmap-levels dimensions)))
    (if (<= level-count max-levels)
        level-count
        (error "Invalid number of mipmap levels specified (~a) for dimensions ~a
Max is: ~s"
               level-count dimensions max-levels))))

(defun2 calc-max-num-mipmap-levels (dimensions)
  (floor (log (apply #'max dimensions) 2)))

(defun2 slow-query-mipmap-count (texture)
  "This is a hack, never use it in production code.

GL has no function for querying the number of mipmap levels
so what we do is get the maxiumum possible count and iterate through checking
the width to see at what point the width reaches 0 or GL throws an error."
  (cepl.textures::with-texture-bound texture
    (let* ((count -1)
           (tex-type (texture-type texture))
           (max-level (gl:get-tex-parameter tex-type :texture-max-level)))
      (handler-case
          (loop :for i :below max-level
             :for width = (gl:get-tex-level-parameter tex-type i :texture-width)
             :do (setf count i)
             :when (= width 0) :return nil)
        (cl-opengl-bindings:opengl-error () nil))
      count)))

(defun2 %make-texture (tex-obj dimensions mipmap layer-count cubes buffer-storage
                      rectangle multisample immutable initial-contents
                      image-format pixel-format generate-mipmaps)
  (let* ((dimensions (listify dimensions))
         (dimensions (%texture-dimensions initial-contents dimensions)))
    ;; check for power of two - handle or warn
    (let* ((pixel-format (when initial-contents
                           (validate-pixel-format
                            initial-contents
                            (or pixel-format
                                (lisp-type->pixel-format initial-contents)))))
           (texture-type (establish-texture-type
                          (if (listp dimensions) (length dimensions) 1)
                          (not (null mipmap)) (> layer-count 1) cubes
                          (every #'po2p dimensions) multisample
                          buffer-storage rectangle))
           (mipmap-levels (if (typep mipmap 'integer)
                              (check-mipmap-level-count-valid mipmap dimensions)
                              (if mipmap
                                  (calc-max-num-mipmap-levels dimensions)
                                  1))))
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
                      (texture-mutable-p tex-obj) (not (and immutable *immutable-available*)))
                (setf (texture-cache-id tex-obj)
                      (cepl.context::tex-kind->cache-index texture-type))
                (cepl.context::register-texture (cepl-context) tex-obj)
                (with-texture-bound tex-obj
                  (allocate-texture tex-obj)
                  (when initial-contents
                    (upload-c-array-to-gpu-array-t
                     (texref tex-obj) initial-contents
                     (cepl.pixel-formats::compile-pixel-format pixel-format)))
                  (when (and generate-mipmaps (> mipmap-levels 1))
                    ;; It may look like we are just going to generate as many
                    ;; mipmap levels as possible here. But #'allocate-texture
                    ;; has used tex-storage, which has specified the number of
                    ;; levels
                    (generate-mipmaps tex-obj)))
                tex-obj))
          (error "This combination of texture features is invalid")))))


(defun2 gen-buffer-tex-initial-contents (initial-contents dimensions image-format
                                        cubes rectangle multisample mipmap
                                        layer-count pixel-format)
  (when pixel-format
    (error 'pixel-format-in-bb-texture :pixel-format pixel-format))
  (let* ((element-type (when image-format
                         (image-format->lisp-type image-format))))
    (etypecase initial-contents
      (null
       (assert element-type)
       (let* ((dimensions (listify dimensions))
              (po2p (every #'po2p dimensions))
              (texture-type (or (establish-texture-type (length dimensions) nil nil nil po2p nil t nil)
                                (error "Could not extablish a buffer-texture-type with dimensions ~a"
                                       dimensions))))
         (assert-valid-args-for-buffer-backend-texture
          image-format cubes rectangle multisample mipmap layer-count
          texture-type)
         (values (make-gpu-array nil :dimensions dimensions :element-type element-type)
                 texture-type)))
      (c-array
       (when element-type
         (assert (eq element-type (element-type initial-contents))))
       (let* ((dimensions (or (listify dimensions) (dimensions initial-contents)))
              (po2p (every #'po2p dimensions))
              (texture-type (or (establish-texture-type (length dimensions) nil nil nil po2p nil t nil)
                                (error "Could not extablish a buffer-texture-type with dimensions ~a"
                                       dimensions))))
         (assert (every #'= dimensions (dimensions initial-contents)))
         (assert-valid-args-for-buffer-backend-texture
          image-format cubes rectangle multisample mipmap layer-count
          texture-type)
         (values (make-gpu-array initial-contents) texture-type)))
      (gpu-array-bb
       (when element-type
         (assert (eq element-type (element-type initial-contents))))
       (let* ((dimensions (or (listify dimensions) (dimensions initial-contents)))
              (po2p (every #'po2p dimensions))
              (texture-type (or (establish-texture-type (length dimensions) nil nil nil po2p nil t nil)
                                (error "Could not extablish a buffer-texture-type with dimensions ~a"
                                       dimensions))))
         (assert (every #'= dimensions (dimensions initial-contents)))
         (assert-valid-args-for-buffer-backend-texture
          image-format cubes rectangle multisample mipmap layer-count
          texture-type)
         (values initial-contents texture-type)))
      (t (with-c-array (carr (make-c-array initial-contents
                                           :element-type element-type
                                           :dimensions dimensions))
           (when element-type
             (assert (eq element-type (element-type carr))))
           (let* ((dimensions (or (listify dimensions) (dimensions carr)))
                  (po2p (every #'po2p dimensions))
                  (texture-type (or (establish-texture-type (length dimensions) nil nil nil po2p nil t nil)
                                    (error "Could not extablish a buffer-texture-type with dimensions ~a"
                                           dimensions))))
             (assert (every #'= dimensions (dimensions carr)))
             (assert-valid-args-for-buffer-backend-texture
              image-format cubes rectangle multisample mipmap layer-count
              texture-type)
             (values (make-gpu-array carr) texture-type)))))))

(defun2 %make-buffer-texture (tex-obj dimensions image-format mipmap layer-count
                             cubes rectangle multisample immutable
                             initial-contents pixel-format)
  (declare (ignore immutable))

  (multiple-value-bind (array type)
      (gen-buffer-tex-initial-contents
       initial-contents dimensions
       image-format cubes rectangle multisample mipmap layer-count pixel-format)
    ;; creation
    (assert (typep tex-obj 'buffer-texture))
    (setf (texture-id tex-obj) (gen-texture)
          (buffer-texture-backing-array tex-obj) array
          (texture-type tex-obj) type
          (texture-base-dimensions tex-obj) dimensions
          (texture-mipmap-levels tex-obj) 1
          (texture-layer-count tex-obj) 1
          (texture-cubes-p tex-obj) nil
          (texture-image-format tex-obj) image-format
          (buffer-texture-owns-array tex-obj) t)
    (setf (texture-cache-id tex-obj)
          (cepl.context::tex-kind->cache-index :texture-buffer))
    (cepl.context::register-texture (cepl-context) tex-obj)
    ;; upload
    (with-texture-bound tex-obj
      (%gl::tex-buffer :texture-buffer image-format
                       (gpu-buffer-id
                        (cepl.gpu-arrays::gpu-array-buffer
                         (buffer-texture-backing-array tex-obj))))
      (setf (texture-allocated-p tex-obj) t)
      tex-obj)))

(defun2 assert-valid-args-for-buffer-backend-texture
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

(defun2 %find-tex-image-format (element-format)
  (if (pixel-format-p element-format)
      (pixel-format->image-format element-format)
      (let ((pfo (lisp-type->pixel-format element-format)))
        (if pfo
            (pixel-format->image-format pfo)
            element-format))))

(defun2 allocate-texture (texture)
  (when (buffer-texture-p texture)
    (error "This function should not have been called with a buffer backed texture"))
  (if (texture-mutable-p texture)
      (allocate-mutable-texture texture)
      (allocate-immutable-texture texture)))

(defun2 allocate-mutable-texture (texture)
  (gl:tex-parameter (texture-type texture) :texture-base-level 0)
  (gl:tex-parameter (texture-type texture) :texture-max-level
                    (1- (texture-mipmap-levels texture)))
  (setf (texture-allocated-p texture) t))

(defun2 allocate-immutable-texture (texture)
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
          ((:texture-2d :proxy-texture-2d :texture-rectangle
                        :proxy-texture-rectangle :texture-cube-map
                        :proxy-texture-cube-map :proxy-texture-1d-array)
           (tex-storage-2d texture-type
                           (texture-mipmap-levels texture)
                           (texture-image-format texture)
                           (first base-dimensions)
                           (or (second base-dimensions) 1)))
          (:texture-1d-array
           (tex-storage-2d texture-type
                           (texture-mipmap-levels texture)
                           (texture-image-format texture)
                           (first base-dimensions)
                           (texture-layer-count texture)))
          ((:texture-3d :proxy-texture-3d :texture-cube-array
                        :proxy-texture-cube-array :proxy-texture-2d-array)
           (tex-storage-3d texture-type
                           (texture-mipmap-levels texture)
                           (texture-image-format texture)
                           (first base-dimensions)
                           (or (second base-dimensions) 1)
                           (or (third base-dimensions) 1)))
          (:texture-2d-array
           (tex-storage-3d texture-type
                           (texture-mipmap-levels texture)
                           (texture-image-format texture)
                           (first base-dimensions)
                           (or (second base-dimensions) 1)
                           (texture-layer-count texture))))
        (setf (texture-allocated-p texture) t))))

(defun2 tex-storage-1d (target levels image-format width)
  (%gl:tex-storage-1d target levels (gl::internal-format->int image-format)
                      width))

(defun2 tex-storage-2d (target levels image-format width height)
  (%gl:tex-storage-2d target levels (gl::internal-format->int image-format)
                      width height))

(defun2 tex-storage-3d (target levels image-format width height depth)
  (%gl:tex-storage-3d target levels (gl::internal-format->int image-format)
                      width height depth))

;;------------------------------------------------------------

(defmethod dimensions ((texture texture))
  (dimensions (texref texture)))

(defmethod resolution ((texture texture))
  (resolution (texref texture)))

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
  (upload-c-array-to-gpu-array-t
   destination object
   (cepl.pixel-formats::compile-pixel-format (lisp-type->pixel-format object))))

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

(defn-inline active-texture-num ((num (unsigned-byte 16))) (values)
  (declare (profile t))
  (%gl:active-texture (+ #x84C0 num))
  (values))

;; {TODO}
;; copy data (from frame-buffer to texture image) - leave for now
;; copy from buffer to texture glCopyTexSubImage2D
;; set texture params
;; get texture params
;; texture views
;; generate-mipmaps
;; texsubimage*d - pushing data
;; glPixelStore — set pixel storage modes
