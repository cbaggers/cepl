(in-package :cgl)

;; :color-attachmenti :depth-attachment :stencil-attachment :depth-stencil-attachment

;; NOTE: The second parameter implies that you can have multiple color
;; attachments. A fragment shader can output different data to any of these by
;; linking out variables to attachments with the glBindFragDataLocation
;; function

;; Framebuffer Object Structure

(defstruct (fbo (:constructor %make-fbo)
                (:conc-name %fbo-))
  (id -1 :type fixnum)
  (attachment-color (make-array 16 :element-type 'attachment
                                :initial-element (make-attachment))
                    :type (array attachment *))
  (draw-buffer-map (foreign-alloc 'cl-opengl-bindings:enum :count 16
                                  :initial-element :none))
  (attachment-depth nil :type (or null attachment)))

(defstruct attachment
  (gpu-array nil :type (or null gpu-array-t))
  (override-blending nil :type boolean)
  (source-rgb :one :type keyword)
  (source-alpha :one :type keyword)
  (destination-rgb :zero :type keyword)
  (destination-alpha :zero :type keyword))

(defun update-draw-buffer-map (fbo)
  (let ((ptr (%fbo-draw-buffer-map fbo)))
    (loop :for i :from 0 :for attachment :across (%fbo-attachment-color fbo) :do
       (setf (mem-ref ptr 'cl-opengl-bindings:enum i)
             (if (attachment-gpu-array attachment)
                 (+ i 36064)
                 :none)))))
;;{TODO} magic-num is gl enum for color-attachment0
;;       needs an assert somewhere

(defmethod print-object ((object fbo) stream)
  (format stream "#<FBO~@[ COLOR-ATTACHMENTS ~a~]~@[ DEPTH-ATTACHMENTS ~a~]>"
          (loop :for i :from 0 :for j :across (%fbo-attachment-color object)
             :when (attachment-gpu-array j) :collect i)
          (not (null (%fbo-attachment-depth object)))))

;;--------------------------------------------------------------
;; Macro to write the helper func and compiler macro

(defun replace-attachment-array (gpu-array attachment)
  (make-attachment
   :gpu-array gpu-array
   :override-blending (attachment-override-blending attachment)
   :source-rgb (attachment-source-rgb attachment)
   :source-alpha (attachment-source-alpha attachment)
   :destination-rgb (attachment-destination-rgb attachment)
   :destination-alpha (attachment-destination-alpha attachment)))

(defun attachment (fbo attachment-name)
  (case attachment-name
    (:d (%fbo-attachment-depth fbo))
    ;; (:s (%fbo-attachment-depth fbo))
    ;; (:ds (%fbo-attachment-depth fbo))
    (otherwise (aref (%fbo-attachment-color fbo) attachment-name))))

(defun (setf attachment) (value fbo attachment-name)
  ;;{TODO} add support for :S and :ds
  (typecase value
    (gpu-array-t
     (if (eq :d attachment-name)
         (setf (%fbo-attachment-depth fbo)
               (replace-attachment-array
                value
                (%fbo-attachment-depth fbo)))
         (setf (aref (%fbo-attachment-color fbo) attachment-name)
               (replace-attachment-array
                value
                (aref (%fbo-attachment-color fbo) attachment-name)))))
    (attachment
     (if (eq :d attachment-name)
         (setf (%fbo-attachment-depth fbo) value)
         (setf (aref (%fbo-attachment-color fbo) attachment-name)
               value)))
    (otherwise (error "invalid value from attachment ~a" value)))
  (update-draw-buffer-map fbo)
  value)

(defparameter *attachments*
  '(:color-attachment0 :color-attachment1 :color-attachment2 :color-attachment3
    :color-attachment4 :color-attachment5 :color-attachment6 :color-attachment7
    :color-attachment8 :color-attachment9 :color-attachment10
    :color-attachment11 :color-attachment12 :color-attachment13
    :color-attachment14 :color-attachment15))

(let ((attachments (make-array 16 :element-type 'keyword
                               :initial-contents *attachments*)))
  (defun get-gl-attachment-keyword (x)
    (cond ((eq x :c) :color-attachment0)
          ((eq x :d) :depth-attachment)
          ((eq x :s) :stencil-attachment)
          ((eq x :ds) :depth-stencil-attachment)
          (t (aref attachments x)))))

(define-compiler-macro get-gl-attachment-keyword (&whole whole x)
  (cond ((numberp x) (nth x *attachments*))
        ((eq x :c) :color-attachment0)
        ((eq x :d) :depth-attachment)
        ((eq x :s) :stencil-attachment)
        ((eq x :ds) :depth-stencil-attachment)
        (t whole)))

(defun binding-shorthand (x)
  (when (symbolp x)
    (cond ((string-equal x "C") 0)
          ((string-equal x "D") :d)
          ((string-equal x "S") :s)
          ((string-equal x "ds") :ds)
          (t (let ((name (symbol-name x)))
               (when (and (>= (length name) 2) (char= #\C (aref name 0)))
                 (let ((num (parse-integer name :start 1)))
                   (when (and (>= num 0) (< num 16)) num))))))))

;;--------------------------------------------------------------

(defun %fbo-draw-buffers (fbo)
  (cl-opengl-bindings:draw-buffers 16 (%fbo-draw-buffer-map fbo)))

;;--------------------------------------------------------------

(defmacro with-fbo-slots (attachment-bindings expression &body body)
  (let ((expr (gensym "expression")))
    `(let* ((,expr ,expression)
            ,@(loop :for var-form :in attachment-bindings :collect
                 (if (listp var-form)
                     `(,(second var-form) (attachment ,expr
                                                      ,(kwd (first var-form))))
                     `(,var-form (attachment ,expr
                                             ,(binding-shorthand var-form))))))
       ,@body)))


;; (with-fbo-slots (c0 d)
;;     (with-bind-fbo (fbo :framebuffer)
;;       (map-g #'prog-1 stream :tex tx))
;;   (print c0)
;;   (print d))

;;--------------------------------------------------------------

(defun make-fbo (&rest fuzzy-attach-args)
  "Will create an fbo and optionally attach the arguments using
   #'fbo-gen-attach"
  (let ((fbo (%make-fbo :id (first (gl:gen-framebuffers 1)))))
    (when fuzzy-attach-args (apply #'fbo-gen-attach fbo fuzzy-attach-args))
    fbo))

(defun make-fbos (&optional (count 1))
  (unless (> count 0)
    (error "Attempting to create invalid number of framebuffers: ~s" count))
  (mapcar (lambda (x) (%make-fbo :id x))
          (gl:gen-framebuffers count)))

(defun %delete-fbo (fbo)
  (gl:delete-framebuffers (listify (%fbo-id fbo))))

(defun %delete-fbos (&rest fbos)
  (gl:delete-framebuffers (mapcar #'%fbo-id fbos)))

(defun %bind-fbo (fbo target)
  ;; The target parameter for this object can take one of 3 values:
  ;; GL_FRAMEBUFFER, GL_READ_FRAMEBUFFER, or GL_DRAW_FRAMEBUFFER.
  ;; The last two allow you to bind an FBO so that reading commands
  ;; (glReadPixels, etc) and writing commands (any command of the form glDraw*)
  ;; can happen to two different buffers.
  ;; The GL_FRAMEBUFFER target simply sets both the read and the write to the
  ;; same FBO.
  ;; When an FBO is bound to a target, the available surfaces change.
  ;; The default framebuffer has buffers like GL_FRONT, GL_BACK, GL_AUXi,
  ;; GL_ACCUM, and so forth. FBOs do not have these.
  ;; Instead, FBOs have a different set of images. Each FBO image represents an
  ;; attachment point, a location in the FBO where an image can be attached.
  (gl:bind-framebuffer target (%fbo-id fbo)))

(defun %unbind-fbo ()
  (gl:bind-framebuffer :framebuffer 0))


;; NOTE: The with-bind-fbo macro lives in the gmap.lisp file, this is because
;;       of the crazy macro relationship they have for performance reasons
;;
;; (defmacro with-bind-fbo ((fbo target &optional (unbind t)) &body body)
;;   ... Sorry mario, the macro you are looking for is in another file ...
;;   )

;; Attaching Images

;; Remember that textures are a set of images. Textures can have mipmaps; thus,
;; each individual mipmap level can contain one or more images.

(defun fbo-attach (fbo tex-array attachment &optional (target :draw-framebuffer))
  ;; To attach images to an FBO, we must first bind the FBO to the context.
  ;; target can be '(:framebuffer :read-framebuffer :draw-framebuffer)
  (with-bind-fbo (fbo target t :c0 nil)
    ;; FBOs have the following attachment points:
    ;; GL_COLOR_ATTACHMENTi: These are an implementation-dependent number of
    ;; attachment points. You can query GL_MAX_COLOR_ATTACHMENTS to determine the
    ;; number of color attachments that an implementation will allow. The minimum
    ;; value for this is 1, so you are guaranteed to be able to have at least
    ;; color attachment 0. These attachment points can only have images bound to
    ;; them with color-renderable formats. All compressed image formats are not
    ;; color-renderable, and thus cannot be attached to an FBO.
    ;;
    ;; GL_DEPTH_ATTACHMENT: This attachment point can only have images with depth
    ;; formats bound to it. The image attached becomes the Depth Buffer for
    ;; the FBO.
    ;;
    ;; GL_STENCIL_ATTACHMENT: This attachment point can only have images with
    ;; stencil formats bound to it. The image attached becomes the stencil buffer
    ;; for the FBO.
    ;;
    ;; GL_DEPTH_STENCIL_ATTACHMENT: This is shorthand for "both depth and stencil"
    ;; The image attached becomes both the depth and stencil buffers.
    ;; Note: If you use GL_DEPTH_STENCIL_ATTACHMENT, you should use a packed
    ;; depth-stencil internal format for the texture or renderbuffer you are
    ;; attaching.
    ;;
    ;; When attaching a non-cubemap, textarget should be the proper
    ;; texture-type: GL_TEXTURE_1D, GL_TEXTURE_2D_MULTISAMPLE, etc.
    (with-slots (texture-type dimensions (mipmap-level level-num) layer-num
                              face-num internal-format texture) tex-array
      (unless (attachment-compatible attachment internal-format)
        (error "attachment is not compatible with this array"))
      (let ((tex-id (slot-value texture 'texture-id)))
        (case (texture-type tex-array)
          ;; A 1D texture contains 2D images that have the vertical height of 1.
          ;; Each individual image can be uniquely identified by a mipmap level.
          (:texture-1d (progn
                         (setf (attachment fbo attachment) tex-array)
                         (gl:framebuffer-texture-1d target attachment :texture-1d
                                                    tex-id mipmap-level)))
          ;; A 2D texture contains 2D images. Each individual image can be
          ;; uniquely identified by a mipmap level.
          (:texture-2d (progn
                         (setf (attachment fbo attachment) tex-array)
                         (gl:framebuffer-texture-2d target attachment :texture-2d
                                                    tex-id mipmap-level)))
          ;; Each mipmap level of a 3D texture is considered a set of 2D images,
          ;; with the number of these being the extent of the Z coordinate.
          ;; Each integer value for the depth of a 3D texture mipmap level is a
          ;; layer. So each image in a 3D texture is uniquely identified by a
          ;; layer and a mipmap level.
          ;; A single mipmap level of a 3D texture is a layered image, where the
          ;; number of layers is the depth of that particular mipmap level.
          (:texture-3d (progn
                         (setf (attachment fbo attachment) tex-array)
                         (%gl:framebuffer-texture-layer target attachment tex-id
                                                        mipmap-level layer-num)))
          ;; Each mipmap level of a 1D Array Textures contains a number of images,
          ;; equal to the count images in the array. While these images are
          ;; technically one-dimensional, they are promoted to 2D status for FBO
          ;; purposes in the same way as a non-array 1D texture: by using a height
          ;; of 1. Thus, each individual image is uniquely identified by a layer
          ;; (the array index) and a mipmap level.
          ;; A single mipmap level of a 1D Array Texture is a layered image, where
          ;; the number of layers is the array size.
          (:texture-1d-array (progn
                               (setf (attachment fbo attachment) tex-array)
                               (%gl:framebuffer-texture-layer
                                target attachment tex-id mipmap-level layer-num)))
          ;; 2D Array textures are much like 3D textures, except instead of the
          ;; number of Z slices, it is the array count. Each 2D image in an array
          ;; texture can be uniquely identified by a layer (the array index) and a
          ;; mipmap level. Unlike 3D textures, the array count doesn't change when
          ;; going down the mipmap hierarchy.
          ;; A single mipmap level of a 2D Array Texture is a layered image, where
          ;; the number of layers is the array size.
          (:texture-2d-array (progn
                               (setf (attachment fbo attachment) tex-array)
                               (%gl:framebuffer-texture-layer
                                target attachment tex-id mipmap-level layer-num)))
          ;; A Rectangle Texture has a single 2D image, and thus is identified by
          ;; mipmap level 0.
          (:texture-rectangle
           (progn
             (setf (attachment fbo attachment) tex-array)
             (gl:framebuffer-texture-2d target attachment :texture-2d
                                        tex-id 0)))
          ;; When attaching a cubemap, you must use the Texture2D function, and
          ;; the textarget must be one of the 6 targets for cubemap binding.
          ;; Cubemaps contain 6 targets, each of which is a 2D image. Thus, each
          ;; image in a cubemap texture can be uniquely identified by a target
          ;; and a mipmap level.
          ;; Also, a mipmap level of a Cubemap Texture is a layered image. For
          ;; cubemaps, you get exactly 6 layers, one for each face. And the order
          ;; of the faces is the same as the order of the enumerators:
          ;; Layer number 	Cubemap face
          ;; 0 	GL_TEXTURE_CUBE_MAP_POSITIVE_X
          ;; 1 	GL_TEXTURE_CUBE_MAP_NEGATIVE_X
          ;; 2 	GL_TEXTURE_CUBE_MAP_POSITIVE_Y
          ;; 3 	GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
          ;; 4 	GL_TEXTURE_CUBE_MAP_POSITIVE_Z
          ;; 5 	GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
          (:texture-cube-map
           (progn
             (setf (attachment fbo attachment) tex-array)
             (gl:framebuffer-texture-2d
              target attachment '&&&CUBEMAP-TARGET&&&
              tex-id mipmap-level)))
          ;; Buffer Textures work like 1D texture, only they have a single image,
          ;; identified by mipmap level 0.
          (:texture-buffer (error "attaching to buffer textures has not been implmented yet"))
          ;; Cubemap array textures work like 2D array textures, only with 6 times
          ;; the number of images. Thus a 2D image in the array is identified by
          ;; the array layer (technically layer-face) and a mipmap level.
          ;; For cubemap arrays, the value that gl_Layer represents is the
          ;; layer-face index. Thus it is the face within a layer, ordered as
          ;; above. So if you want to render to the 3rd layer, +z face, you would
          ;; set gl_Layer to (2 * 6) + 4, or 16.
          (:texture-cube-map-array (error "attaching to cube-map-array textures has not been implmented yet")))))))

(defun fbo-gen-attach (fbo &rest args)
  "The are 3 kinds of valid argument:
   - keyword naming an attachment: This makes a new texture at
     +default-resolution+ and attaches
   - (keyword camera) creates a new texture at the framesize of
     the camera and attaches it to attachment named by keyword
   - (keyword vector2): creates a new texture sized by the vector
     and attaches it to attachment named by keyword
   - (keyword texarray): attaches the tex-array
   - (keyword texture): attaches the root tex-array"
  (let ((target (%extract-target (first args))))
    (mapcar (lambda (x y) (fbo-attach fbo x y target))
            (mapcar #'%gen-textures args)
            (mapcar (lambda (x) (binding-shorthand (first x)))
                    (mapcar #'listify args)))))

(defun %extract-target (x)
  (if (member x '(:draw-framebuffer :read-framebuffer :framebuffer :framebuffer))
      x
      :draw-framebuffer))

(defun %gen-textures (pattern)
  (assert (or (listp pattern) (keywordp pattern)))
  (cond
    ((keywordp pattern)
     (texref
      (make-texture nil :dimensions +default-resolution+
                    :internal-format (%get-default-texture-format pattern))))
    ((typep (second pattern) 'cepl-camera:camera)
     (texref
      (make-texture nil :dimensions (let ((fs (cepl-camera:frame-size (second pattern))))
                                      (list (aref fs 0) (aref fs 1)))
                    :internal-format (%get-default-texture-format
                                      (first pattern)))))
    ((typep (second pattern) 'gpu-array-t) (second pattern))
    ((typep (second pattern) 'gl-texture) (texref (second pattern)))
    (t (error "Invalid pattern in %gen-textures"))))

(defun %get-default-texture-format (attachment)
  (assert (keywordp attachment))
  (let ((char (char-downcase (aref (symbol-name attachment) 0))))
    (cond ((char= char #\c) :rgba8)
          ((char= char #\d) :depth-component16)
          (t (error "No default texture format for attachment: ~s" attachment)))))

(defun attachment-compatible (attachment internal-format)
  (case attachment
    ((:d :depth-attachment) (depth-formatp internal-format))
    ((:s :stencil-attachment) (stencil-formatp internal-format))
    ((:ds :depth-stencil-attachment) (depth-stencil-formatp internal-format))
    (otherwise (color-renderable-formatp internal-format))))

(defun fbo-detach (fbo attachment)
  ;; The texture argument is the texture object name you want to attach from.
  ;; If you pass zero as texture, this has the effect of clearing the attachment
  ;; for this attachment, regardless of what kind of image was attached there.
  (setf (attachment fbo attachment) nil)
  (%gl:framebuffer-texture-layer :draw-framebuffer attachment 0 0 0))
