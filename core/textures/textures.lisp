(in-package :cepl.textures)

;;------------------------------------------------------------

(defvar *immutable-available* t)
(define-const +cube-face-order+
    '(:texture-cube-map-positive-x
      :texture-cube-map-negative-x
      :texture-cube-map-positive-y
      :texture-cube-map-negative-y
      :texture-cube-map-positive-z
      :texture-cube-map-negative-z)
  :type list)

(defun+ check-immutable-feature ()
  (unless (has-feature "GL_ARB_texture_storage")
    (setf *immutable-available* nil)))

;;------------------------------------------------------------

(defun+ texref (texture &key (mipmap-level 0) (layer 0) (cube-face 0))
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

            (when (or (null (cepl-context))
                      (null (cepl.context::%cepl-context-gl-context
                             (cepl-context))))
              (cepl.context::delay-initialization
               (cepl-context)
               (lambda () (reinit-on-context result))
               (list texture)))
            result)
          (error "Texture index out of range"))))

(defun+ reinit-on-context (gpu-array)
  (let ((texture (gpu-array-t-texture gpu-array))
        (level (gpu-array-t-level-num gpu-array)))
    (setf (gpu-array-t-texture-type gpu-array)
          (texture-type texture))
    (setf (gpu-array-dimensions gpu-array)
          (dimensions-at-mipmap-level texture level))
    (setf (gpu-array-t-image-format gpu-array)
          (texture-image-format texture))))

(defun+ valid-index-p (texture mipmap-level layer cube-face)
  (or (= 0 mipmap-level layer cube-face)
      (and (< mipmap-level (texture-mipmap-levels texture))
           (< layer (texture-layer-count texture))
           (if (texture-cubes-p texture)
               (<= cube-face 6)
               (= 0 cube-face)))))

;;------------------------------------------------------------

(defun+ generate-mipmaps (texture)
  (let ((type (texture-type texture)))
    (%with-scratch-texture-bound texture
      (%gl:generate-mipmap type))))

(defun+ error-on-invalid-upload-formats (target image-format pixel-format pixel-type)
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

(defun+ multisample-texture-p (texture)
  (not (= 0 (texture-samples texture))))

(defun+ upload-c-array-to-gpu-array-t (gpu-array c-array &optional pixel-format)
  (let* ((element-pf (lisp-type->pixel-format c-array))
         (compiled-pf (or pixel-format (cepl.pixel-formats::compile-pixel-format
                                        element-pf)))
         (pix-format (first compiled-pf))
         (pix-type (second compiled-pf))
         (c-array-dims (c-array-dimensions c-array)))
    (with-gpu-array-t gpu-array
      (error-on-invalid-upload-formats texture-type image-format pix-format
                                       pix-type)
      (unless (equal c-array-dims dimensions)
        (error "dimensions of c-array and gpu-array must match~%c-array:~a gpu-array:~a"
               c-array-dims dimensions))
      (%with-scratch-texture-bound (gpu-array-t-texture gpu-array)
        (%upload-tex texture texture-type level-num c-array-dims
                     layer-num face-num pix-format pix-type (pointer c-array)
                     (c-array-row-alignment c-array)))))
  gpu-array)

;; [TODO] add offsets
(defun+ %upload-tex (tex tex-type level-num dimensions layer-num face-num
                         pix-format pix-type pointer row-alignment)
  (assert
   (not (multisample-texture-p tex)) ()
   "CEPL: Sorry can not yet upload data to a multisample texture in this fashion:~%~a"
   tex)
  (if (texture-mutable-p tex)
      (%upload-to-mutable-tex tex tex-type level-num dimensions layer-num
                              face-num pix-format pix-type pointer
                              row-alignment)
      (%upload-to-immutable-tex tex tex-type level-num dimensions layer-num
                                face-num pix-format pix-type pointer
                                row-alignment)))

(defun+ %upload-to-mutable-tex (tex tex-type level-num dimensions layer-num
                                    face-num pix-format pix-type pointer
                                    row-alignment)
  ;; border is an old (now unsupported) parameter and so is always be set to 0
  (destructuring-bind (&optional (width 1) (height 1) (depth 1)) dimensions
    (setf (unpack-alignment) row-alignment)
    (case tex-type
      (:texture-1d (gl:tex-image-1d
                    tex-type level-num (texture-image-format tex)
                    width 0 pix-format pix-type
                    pointer))
      (:texture-2d (gl:tex-image-2d
                    tex-type level-num (texture-image-format tex)
                    width height 0
                    pix-format pix-type pointer))
      (:texture-3d (gl:tex-image-3d
                    tex-type level-num (texture-image-format tex)
                    width height
                    depth 0 pix-format pix-type
                    pointer))
      (:texture-1d-array (gl:tex-image-2d
                          tex-type level-num
                          (texture-image-format tex)
                          width layer-num 0
                          pix-format pix-type pointer))
      (:texture-2d-array (gl:tex-image-3d
                          tex-type level-num
                          (texture-image-format tex)
                          width height
                          layer-num 0 pix-format pix-type pointer))
      (:texture-cube-map (gl:tex-image-2d
                          (nth face-num +cube-face-order+)
                          level-num (texture-image-format tex)
                          width height 0
                          pix-format pix-type pointer))
      (t (error "not currently supported for upload: ~a" tex-type)))))


(defun+ %upload-to-immutable-tex (tex tex-type level-num dimensions layer-num
                                      face-num pix-format pix-type pointer
                                      row-alignment)
  (declare (ignore tex))
  (destructuring-bind (&optional (width 1) (height 1) (depth 1)) dimensions
    (setf (unpack-alignment) row-alignment)
    (case tex-type
      (:texture-1d (gl:tex-sub-image-1d tex-type level-num 0 width
                                        pix-format pix-type pointer))
      (:texture-2d (gl:tex-sub-image-2d tex-type level-num 0 0
                                        width height
                                        pix-format pix-type pointer))
      (:texture-1d-array (gl:tex-sub-image-2d tex-type level-num 0 0
                                              width layer-num
                                              pix-format pix-type pointer))
      (:texture-3d (gl:tex-sub-image-3d tex-type level-num 0 0 0
                                        width height
                                        depth pix-format pix-type
                                        pointer))
      (:texture-2d-array (gl:tex-sub-image-3d tex-type level-num 0 0 0
                                              width
                                              height layer-num
                                              pix-format pix-type pointer))
      (:texture-cube-map (gl:tex-sub-image-2d (nth face-num +cube-face-order+)
                                              level-num 0 0 width
                                              height pix-format
                                              pix-type pointer))
      (t (error "not currently supported for upload: ~a" tex-type)))))

(defun+ upload-from-buffer-to-gpu-array-t (&rest args)
  (declare (ignore args))
  (error "upload-from-buffer-to-gpu-array-t is not implemented yet"))

;;------------------------------------------------------------

(define-const +valid-texture-storage-options+
    ;; 0.mipmap 1.layers 2.cubes 3.dimensions 4.multisample 5.buffer 6.rectangle
    ;;
    ;;  0    1    2    3    4    5    6
    '(((t    nil  nil  1    nil  nil  nil) :texture-1d)
      ((t    nil  nil  2    nil  nil  nil) :texture-2d)
      ((t    nil  nil  3    nil  nil  nil) :texture-3d)
      ((t    t    nil  1    nil  nil  nil) :texture-1d-array)
      ((t    t    nil  2    nil  nil  nil) :texture-2d-array)
      ((t    nil  t    2    nil  nil  nil) :texture-cube-map)
      ((t    t    t    2    nil  nil  nil) :texture-cube-map-array)
      ((nil  nil  nil  2    nil  nil  t  ) :texture-rectangle)
      ((nil  nil  nil  1    nil  t    nil) :texture-buffer)
      ((nil  nil  nil  2    nil  t    nil) :texture-buffer)
      ((nil  nil  nil  3    nil  t    nil) :texture-buffer)
      ((nil  nil  nil  2    t    nil  nil) :texture-2d-multisample)
      ((nil  t    nil  2    t    nil  nil) :texture-2d-multisample-array))
  :type list)

;;------------------------------------------------------------

(defun+ establish-texture-type (dimensions mipmap layers cubes po2 multisample
                                           buffer rectangle)
  ;; Dont strain too hard trying to work out this function, see the table
  ;; above to see what input result in what texture types
  (declare (ignore po2))
  (cadr (assoc
         (list mipmap layers cubes dimensions multisample buffer rectangle)
         +valid-texture-storage-options+
         :test #'(lambda (a b)
                   (destructuring-bind
                         (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7)
                       (append a b)
                     (and (if b1 t (not a1)) (if b2 t (not a2))
                          (if b3 t (not a3)) (eql b4 a4)
                          (eql b5 a5) (eql b6 a6) (eql b7 a7)))))))

;;------------------------------------------------------------

(defun+ gen-texture ()
  (first (gl:gen-textures 1)))

(defun+ po2p (x) (eql 0 (logand x (- x 1))))

(defun+ dimensions-at-mipmap-level (texture level)
  (if (= level 0)
      (texture-base-dimensions texture)
      (loop :for i :in (texture-base-dimensions texture) :collect
         (/ i (expt 2 level)))))

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
(defun+ %texture-dimensions (initial-contents dimensions)
  (let ((dims
         (if initial-contents
             (if dimensions
                 (error "Cannot specify dimensions and initial-contents")
                 (dimensions initial-contents))
             (if dimensions dimensions (error "must specify dimensions if no initial-contents provided")))))
    ;; Early checks, move to own function if gets more extensive
    (assert (every (lambda (x) (> x 0)) dims) ()
            'texture-dimensions-lequal-zero
            :dimensions dims)
    dims))

(defun+ make-texture-from-id (gl-object
                              &key
                              base-dimensions
                              texture-type
                              element-type
                              (mipmap-levels 1)
                              (layer-count 1)
                              cubes
                              allocated
                              mutable-p
                              (samples 0)
                              fixed-sample-locations)
  (assert (or (and (symbolp base-dimensions)
                   (string= base-dimensions "?"))
              (numberp base-dimensions)
              (and (listp base-dimensions)
                   (every (lambda (x)
                            (or (numberp x)
                                (and (symbolp x)
                                     (string= x "?"))))
                          base-dimensions))))
  (let* ((size-unknown nil)
         (base-dimensions
          (loop :for dim :in (listify base-dimensions)
             :collect (if (and (symbolp dim) (string= dim "?"))
                          (progn
                            (setf size-unknown t)
                            0)
                          dim)))
         (fixed-sample-locations (not (null fixed-sample-locations)))
         (texture-type (or texture-type
                           (establish-texture-type
                            (if (listp base-dimensions)
                                (length base-dimensions)
                                1)
                            (> mipmap-levels 1)
                            (> layer-count 1)
                            cubes
                            (and (not size-unknown)
                                 (every #'po2p base-dimensions))
                            (> samples 0)
                            nil
                            nil))))
    (assert (typep gl-object 'gl-id))
    (cepl.context::register-texture
     (cepl-context)
     (%%make-texture
      :id gl-object
      :cache-id (tex-kind->cache-index texture-type)
      :base-dimensions base-dimensions
      :type texture-type
      :image-format element-type
      :mipmap-levels mipmap-levels
      :layer-count layer-count
      :cubes-p cubes
      :allocated-p allocated
      :mutable-p mutable-p
      :samples samples
      :fixed-sample-locations-p fixed-sample-locations))))


(defun+ make-texture (initial-contents
                     &key dimensions element-type (mipmap nil)
                       (layer-count 1) (cubes nil) (rectangle nil)
                       (immutable t) (buffer-storage nil)
                       (generate-mipmaps t) pixel-format
                       (samples nil) (fixed-sample-locations nil))
  (let ((dimensions (listify dimensions)))
    (cepl.context::if-gl-context
     (make-texture-now %pre% initial-contents dimensions element-type mipmap
                       layer-count cubes rectangle immutable
                       buffer-storage generate-mipmaps pixel-format
                       samples fixed-sample-locations)
     (make-uninitialized-texture buffer-storage)
     (when (typep initial-contents 'gpu-array-bb)
       (list initial-contents)))))

(defun+ make-texture-now (tex-obj initial-contents dimensions element-type mipmap
                         layer-count cubes rectangle immutable
                         buffer-storage generate-mipmaps pixel-format
                         samples fixed-sample-locations)
  ;;
  (let ((element-type (cffi-type->gl-type element-type))
        (image-format (calc-image-format element-type initial-contents))
        (fixed-sample-locations (not (null fixed-sample-locations))))
    (cond
      ;; cube textures
      ((and initial-contents cubes)
       (%make-cube-texture tex-obj dimensions mipmap layer-count cubes
                           buffer-storage rectangle  immutable
                           initial-contents image-format pixel-format
                           generate-mipmaps samples fixed-sample-locations))
      ;; initialize content needs to be turned into c-array
      ((and initial-contents (typep initial-contents 'uploadable-lisp-seq))
       (%make-texture-with-lisp-data tex-obj dimensions mipmap layer-count
                                     cubes buffer-storage rectangle immutable
                                     initial-contents generate-mipmaps
                                     element-type image-format pixel-format
                                     samples fixed-sample-locations))
      ;; buffer backed - note that by now if there were intitial contents, they
      ;;                 are now a c-array
      (buffer-storage
       (%make-buffer-texture tex-obj
                             (%texture-dimensions initial-contents dimensions)
                             image-format mipmap layer-count cubes
                             rectangle (or samples fixed-sample-locations)
                             immutable initial-contents pixel-format))
      ;; all other cases
      (t (%make-texture tex-obj dimensions mipmap layer-count cubes
                        buffer-storage rectangle immutable
                        initial-contents image-format pixel-format
                        generate-mipmaps samples fixed-sample-locations)))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun+ calc-image-format (element-type initial-contents)
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

(defun+ %calc-image-format-without-declared-format (initial-contents)
  ;; The user didnt declare an element-type so try and infer one
  (typecase initial-contents
    (null (error 'make-tex-no-content-no-type))
    (c-array (lisp-type->image-format
              (element-type initial-contents)))
    (uploadable-lisp-seq (lisp-type->image-format
                          (cepl.c-arrays::lisp->gl-type
                           (cepl.c-arrays::scan-for-type initial-contents))))))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun+ %calc-image-format-with-declared-format
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

(defun+ %calc-image-format-with-pixel-format (pixel-format initial-contents)
  "Convert the pixel-format to an internal format and delegate to
   %calc-image-format"
  (%calc-image-format-with-declared-format
   pixel-format
   (pixel-format->image-format pixel-format)
   initial-contents))

(defun+ %calc-image-format-with-lisp-type (element-type initial-contents)
  "Convert the lisp type to an internal format and delegate to
   %calc-image-format"
  (%calc-image-format-with-declared-format
   element-type
   (lisp-type->image-format element-type)
   initial-contents))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun+ %make-texture-with-lisp-data
    (tex-obj dimensions mipmap layer-count cubes buffer-storage
     rectangle immutable initial-contents
     generate-mipmaps element-type image-format pixel-format
     samples fixed-sample-locations)
  (declare (ignore element-type))
  (let ((element-type (image-format->lisp-type image-format)))
    (with-c-array-freed (tmp (make-c-array initial-contents :dimensions dimensions
                                     :element-type element-type))
      (make-texture-now tex-obj tmp nil nil mipmap layer-count cubes rectangle
                        immutable buffer-storage
                        generate-mipmaps pixel-format
                        samples fixed-sample-locations))))

(defun+ %make-cube-texture (tex-obj dimensions mipmap layer-count cubes
                                    buffer-storage rectangle immutable
                                    initial-contents image-format pixel-format
                                    generate-mipmaps samples
                                    fixed-sample-locations)
  (assert (= 6 (length initial-contents)))
  (let* ((target-dim (or dimensions (dimensions (first initial-contents))))
         (dim (if (every (lambda (_) (equal target-dim (dimensions _)))
                         initial-contents)
                  target-dim
                  (error "Conflicting dimensions of c-arrays passed to make-texture with :cube t:~%~a"
                         initial-contents)))
         (result (%make-texture tex-obj dim mipmap layer-count cubes
                                buffer-storage rectangle immutable
                                nil image-format pixel-format generate-mipmaps
                                samples fixed-sample-locations)))
    (loop :for data :in initial-contents :for i :from 0 :do
       (push-g data (texref result :cube-face i)))
    result))


(defun+ validate-pixel-format (initial-contents pixel-format)
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

(defun+ calc-max-num-mipmap-levels (dimensions)
  (floor (log (apply #'max dimensions) 2)))

(defun+ slow-query-mipmap-count (texture)
  "This is a hack, never use it in production code.

GL has no function for querying the number of mipmap levels
so what we do is get the maxiumum possible count and iterate through checking
the width to see at what point the width reaches 0 or GL throws an error."
  (%with-scratch-texture-bound texture
    (let* ((count -1)
           (tex-type (texture-type texture))
           (max-level (gl:get-tex-parameter tex-type :texture-max-level)))
      (handler-case
          (loop :for i :below max-level
             :for width = (gl:get-tex-level-parameter tex-type i :texture-width)
             :do (setf count i)
             :when (= width 0) :return nil)
        (%gl:opengl-error () nil))
      count)))

(defn tex-kind->cache-index ((kind symbol)) (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (ecase kind
    (:texture-1d #.(gl-enum :texture-1d))
    (:texture-2d #.(gl-enum :texture-2d))
    (:texture-3d #.(gl-enum :texture-3d))
    (:texture-1d-array #.(gl-enum :texture-1d-array))
    (:texture-2d-array #.(gl-enum :texture-2d-array))
    (:texture-rectangle #.(gl-enum :texture-rectangle))
    (:texture-cube-map #.(gl-enum :texture-cube-map))
    (:texture-cube-map-array #.(gl-enum :texture-cube-map-array))
    (:texture-buffer #.(gl-enum :texture-buffer))
    (:texture-2d-multisample #.(gl-enum :texture-2d-multisample))
    (:texture-2d-multisample-array #.(gl-enum :texture-2d-multisample-array))))

(defun+ %make-texture (tex-obj dimensions mipmap layer-count cubes
                               buffer-storage rectangle immutable
                               initial-contents image-format
                               pixel-format generate-mipmaps
                               samples fixed-sample-locations)
  (let* ((dimensions (listify dimensions))
         (dimensions (%texture-dimensions initial-contents dimensions))
         (multisample (not (null samples))))
    (when fixed-sample-locations
      (assert samples))
    ;; check for power of two - handle or warn
    (let* ((pixel-format (when initial-contents
                           (validate-pixel-format
                            initial-contents
                            (or pixel-format
                                (lisp-type->pixel-format initial-contents)))))
           (texture-type (establish-texture-type
                          (length dimensions)
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
              (error "CEPL: Attept to make cube texture with invalid dimensions:~%~s"
                     dimensions)
              (progn
                (setf (texture-id tex-obj) (gen-texture)
                      (texture-base-dimensions tex-obj) dimensions
                      (texture-type tex-obj) texture-type
                      (texture-mipmap-levels tex-obj) mipmap-levels
                      (texture-layer-count tex-obj) layer-count
                      (texture-cubes-p tex-obj) cubes
                      (texture-image-format tex-obj) image-format
                      (texture-samples tex-obj) (or samples 0)
                      (texture-fixed-sample-locations-p tex-obj) fixed-sample-locations
                      (texture-mutable-p tex-obj) (not (and immutable *immutable-available*)))
                (setf (texture-cache-id tex-obj)
                      (tex-kind->cache-index texture-type))
                (cepl.context::register-texture (cepl-context) tex-obj)
                (%with-scratch-texture-bound tex-obj
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
          (error 'invalid-options-for-texture
                 :buffer-storage buffer-storage
                 :cubes cubes
                 :dimensions dimensions
                 :layer-count layer-count
                 :mipmap mipmap
                 :multisample multisample
                 :rectangle rectangle)))))


(defun+ gen-buffer-tex-initial-contents (initial-contents dimensions image-format
                                        cubes rectangle multisample mipmap
                                        layer-count pixel-format)
  (when pixel-format
    (error 'pixel-format-in-bb-texture :pixel-format pixel-format))
  (let* ((element-type (when image-format
                         (image-format->lisp-type image-format)))
         (dimensions (listify dimensions))
         (rank (length dimensions)))
    (etypecase initial-contents
      (null
       (assert element-type)
       (let* ((po2p (every #'po2p dimensions))
              (texture-type (or (establish-texture-type rank nil nil nil po2p nil t nil)
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
       (let* ((dimensions (or dimensions (dimensions initial-contents)))
              (po2p (every #'po2p dimensions))
              (texture-type (or (establish-texture-type rank nil nil nil po2p nil t nil)
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
       (let* ((dimensions (or dimensions (dimensions initial-contents)))
              (po2p (every #'po2p dimensions))
              (texture-type (or (establish-texture-type rank nil nil nil po2p nil t nil)
                                (error "Could not extablish a buffer-texture-type with dimensions ~a"
                                       dimensions))))
         (assert (every #'= dimensions (dimensions initial-contents)))
         (assert-valid-args-for-buffer-backend-texture
          image-format cubes rectangle multisample mipmap layer-count
          texture-type)
         (values initial-contents texture-type)))
      (t (with-c-array-freed (carr (make-c-array initial-contents
                                           :element-type element-type
                                           :dimensions dimensions))
           (gen-buffer-tex-initial-contents carr dimensions image-format
                                            cubes rectangle multisample mipmap
                                            layer-count pixel-format))))))

(defun+ %make-buffer-texture (tex-obj dimensions image-format mipmap layer-count
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
          (tex-kind->cache-index :texture-buffer))
    (cepl.context::register-texture (cepl-context) tex-obj)
    ;; upload
    (%with-scratch-texture-bound tex-obj
      (%gl::tex-buffer :texture-buffer image-format
                       (gpu-buffer-id
                        (cepl.gpu-arrays::gpu-array-buffer
                         (buffer-texture-backing-array tex-obj))))
      (setf (texture-allocated-p tex-obj) t)
      tex-obj)))

(defun+ assert-valid-args-for-buffer-backend-texture
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

(defun+ %find-tex-image-format (element-format)
  (if (pixel-format-p element-format)
      (pixel-format->image-format element-format)
      (let ((pfo (lisp-type->pixel-format element-format)))
        (if pfo
            (pixel-format->image-format pfo)
            element-format))))

(defun+ allocate-texture (texture)
  (when (buffer-texture-p texture)
    (error "This function should not have been called with a buffer backed texture"))
  (if (texture-mutable-p texture)
      (allocate-mutable-texture texture)
      (allocate-immutable-texture texture)))

(defun+ allocate-mutable-texture (texture)
  ;; {TODO} Well this is clearly missing a lot :p
  (let ((base-dimensions (texture-base-dimensions texture))
        (texture-type (texture-type texture)))
    (destructuring-bind (&optional (width 1) (height 1) (depth 1))
        base-dimensions
      (declare (ignore depth))
      (case texture-type
        (:texture-2d-multisample
         (%gl:tex-image-2d-multisample
          :texture-2d-multisample
          (texture-samples texture)
          (texture-image-format texture)
          width height
          (texture-fixed-sample-locations-p texture)))
        (t (gl:tex-parameter (texture-type texture) :texture-base-level 0)
           (gl:tex-parameter (texture-type texture) :texture-max-level
                             (1- (texture-mipmap-levels texture)))))))
  (setf (texture-allocated-p texture) t))

(defun+ allocate-immutable-texture (texture)
  (if (texture-allocated-p texture)
      (error "Attempting to reallocate a previously allocated texture")
      (let ((base-dimensions (texture-base-dimensions texture))
            (texture-type (texture-type texture)))
        (destructuring-bind (&optional (width 1) (height 1) (depth 1))
            base-dimensions
          (case texture-type
            ((:texture-1d :proxy-texture-1d)
             (tex-storage-1d texture-type
                             (texture-mipmap-levels texture)
                             (texture-image-format texture)
                             width))
            ((:texture-2d :proxy-texture-2d :texture-rectangle
                          :proxy-texture-rectangle :texture-cube-map
                          :proxy-texture-cube-map :proxy-texture-1d-array)
             (tex-storage-2d texture-type
                             (texture-mipmap-levels texture)
                             (texture-image-format texture)
                             width
                             height))
            (:texture-2d-multisample
             (tex-storage-2d-multisample
              texture-type
              (texture-image-format texture)
              width
              height
              (texture-samples texture)
              (texture-fixed-sample-locations-p texture)))
            (:texture-2d-multisample-array
             (tex-storage-3d-multisample
              texture-type
              (texture-image-format texture)
              width
              height
              depth
              (texture-samples texture)
              (texture-fixed-sample-locations-p texture)))
            (:texture-1d-array
             (tex-storage-2d texture-type
                             (texture-mipmap-levels texture)
                             (texture-image-format texture)
                             width
                             (texture-layer-count texture)))
            ((:texture-3d :proxy-texture-3d :texture-cube-array
                          :proxy-texture-cube-array :proxy-texture-2d-array)
             (tex-storage-3d texture-type
                             (texture-mipmap-levels texture)
                             (texture-image-format texture)
                             width
                             height
                             depth))
            (:texture-2d-array
             (tex-storage-3d texture-type
                             (texture-mipmap-levels texture)
                             (texture-image-format texture)
                             width
                             height
                             (texture-layer-count texture)))))
        (setf (texture-allocated-p texture) t))))

(defun+ tex-storage-1d (target levels image-format width)
  (%gl:tex-storage-1d target levels (gl::internal-format->int image-format)
                      width))

(defun+ tex-storage-2d (target levels image-format width height)
  (%gl:tex-storage-2d target levels (gl::internal-format->int image-format)
                      width height))

(defun+ tex-storage-2d-multisample (target image-format width height
                                           samples fixed-sample-locations)
  (%gl:tex-storage-2d-multisample
   target samples (gl::internal-format->int image-format)
   width height fixed-sample-locations))

(defun+ tex-storage-3d-multisample (target image-format width height depth
                                           samples fixed-sample-locations)
  (%gl:tex-storage-3d-multisample
   target samples (gl::internal-format->int image-format)
   width height depth fixed-sample-locations))

(defun+ tex-storage-3d (target levels image-format width height depth)
  (%gl:tex-storage-3d target levels (gl::internal-format->int image-format)
                      width height depth))

;;------------------------------------------------------------

(defmethod dimensions ((texture texture))
  (texture-base-dimensions texture))

(defmethod resolution ((texture texture))
  (let ((dim (texture-base-dimensions texture)))
    (make-array (length dim) :element-type 'single-float
                :initial-contents (mapcar (lambda (i) (coerce i 'single-float))
                                          dim))))

;;------------------------------------------------------------

;; [TODO] This feels like could create non-optimal solutions
;;        So prehaps this should look at texture format, and
;;        find the most similar compatible format, with worst
;;        case being just do what we do below
(defn copy-c-array-to-texture-backed-gpu-array ((src c-array)
                                                (dst gpu-array-t))
    gpu-array-t
  (upload-c-array-to-gpu-array-t
   dst src
   (cepl.pixel-formats::compile-pixel-format (lisp-type->pixel-format src)))
  dst)

(defn copy-lisp-data-to-texture-backed-gpu-array ((src (or list array))
                                                  (dst gpu-array-t))
    gpu-array-t
  (with-c-array-freed
      (c-a (make-c-array src
                         :dimensions (gpu-array-dimensions dst)
                         :element-type (image-format->pixel-format
                                        (gpu-array-t-image-format dst))))
    (copy-c-array-to-texture-backed-gpu-array c-a dst))
  dst)

;; [TODO] implement gl-fill and fill arguments
;; [TODO] Does not respect UNPACK_BUFFER
(defn copy-texture-backed-gpu-array-to-new-c-array ((src gpu-array-t))
    c-array
  (with-gpu-array-t src
    (let* ((p-format (image-format->pixel-format
                      (gpu-array-t-image-format src)))
           (c-array (make-c-array nil :dimensions (gpu-array-dimensions src)
                                  :element-type p-format)))
      (destructuring-bind (format type)
          (cepl.pixel-formats::compile-pixel-format p-format)
        (%with-scratch-texture-bound texture
          (setf (pack-alignment) (c-array-row-alignment c-array))
          (%gl:get-tex-image (foreign-enum-value '%gl:enum texture-type)
                             (coerce level-num 'real)
                             format
                             type
                             (pointer c-array))))
      c-array)))

(defn copy-texture-backed-gpu-array-to-new-lisp-data ((src gpu-array-t))
    t
  (with-c-array-freed
      (c-array (copy-texture-backed-gpu-array-to-new-c-array src))
    (pull1-g c-array)))


(defmethod push-g ((object c-array) (destination texture))
  (copy-c-array-to-texture-backed-gpu-array object (texref destination)))
(defmethod push-g ((object c-array) (destination gpu-array-t))
  (copy-c-array-to-texture-backed-gpu-array object destination))
(defmethod push-g ((object list) (destination texture))
  (copy-lisp-data-to-texture-backed-gpu-array object (texref destination)))
(defmethod push-g ((object array) (destination texture))
  (copy-lisp-data-to-texture-backed-gpu-array object (texref destination)))
(defmethod push-g ((object list) (destination gpu-array-t))
  (copy-lisp-data-to-texture-backed-gpu-array object destination))
(defmethod push-g ((object array) (destination gpu-array-t))
  (copy-lisp-data-to-texture-backed-gpu-array object destination))

(defmethod pull-g ((object gpu-array-t))
  (copy-texture-backed-gpu-array-to-new-lisp-data object))
(defmethod pull-g ((object texture))
  (copy-texture-backed-gpu-array-to-new-lisp-data (texref object)))

(defmethod pull1-g ((object gpu-array-t))
  (copy-texture-backed-gpu-array-to-new-c-array object))
(defmethod pull1-g ((object texture))
  (copy-texture-backed-gpu-array-to-new-c-array (texref object)))

(defmethod copy-g ((source c-array) (destination gpu-array-t))
  (copy-c-array-to-texture-backed-gpu-array source destination))
(defmethod copy-g ((source list) (destination gpu-array-t))
  (copy-lisp-data-to-texture-backed-gpu-array source destination))
(defmethod copy-g ((source array) (destination gpu-array-t))
  (copy-lisp-data-to-texture-backed-gpu-array source destination))
(defmethod copy-g ((source gpu-array-t) (destination (eql :c-array)))
  (declare (ignore destination))
  (copy-texture-backed-gpu-array-to-new-c-array source))
(defmethod copy-g ((source gpu-array-t) (destination (eql :lisp)))
  (declare (ignore destination))
  (copy-texture-backed-gpu-array-to-new-lisp-data source))

;; {TODO}
;; copy data (from frame-buffer to texture image) - (glCopyTexSubImage2D)
;; copy to tex from buffer (same as above but with GL_PIXEL_UNPACK_BUFFER)
;; copy to buffer from tex (same as above but with GL_PIXEL_PACK_BUFFER)
;; set texture params
;; get texture params
;; texture views
;; dedicated generate-mipmaps
