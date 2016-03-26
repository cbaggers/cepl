(in-package :cepl.fbos)

;; {TODO} A fragment shader can output different data to any of these by
;;        linking out variables to attachments with the glBindFragDataLocation
;;        function

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun %make-fbo (&key (id -1)
		    (attachment-color
		     (make-array (max-draw-buffers *gl-context*)
				 :element-type 'attachment
				 :initial-contents
				 (loop :for i
				    :below (max-draw-buffers *gl-context*)
				    :collect (%make-attachment))))
		    (draw-buffer-map
		     (foreign-alloc 'cl-opengl-bindings:enum :count
				    (max-draw-buffers *gl-context*)
				    :initial-element :none))
		    (attachment-depth (%make-attachment))
		    (clear-mask (cffi:foreign-bitfield-value
				 '%gl::ClearBufferMask '(:color-buffer-bit)))
		    (is-default nil)
		    (blending-params (make-blending-params
				      :mode-rgb :func-add
				      :mode-alpha :func-add
				      :source-rgb :one
				      :source-alpha :one
				      :destination-rgb :zero
				      :destination-alpha :zero)))
  (%%make-fbo
   :id id
   :attachment-color attachment-color
   :draw-buffer-map draw-buffer-map
   :attachment-depth attachment-depth
   :clear-mask clear-mask
   :is-default is-default
   :blending-params blending-params))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod print-object ((obj attachment) stream)
  (format stream "#<~a-ATTACHMENT (:fbo ~s)>"
          (if (%attachment-gpu-array obj)
	      (let ((f (gpu-array-t-image-format (%attachment-gpu-array obj))))
		(cond
		  ((color-renderable-formatp f) "COLOR")
		  ((depth-formatp f) "DEPTH")
		  ((stencil-formatp f) "DEPTH")
		  ((depth-stencil-formatp f) "DEPTH-STENCIL")
		  (t "INVALID-IMAGE-FORMAT"))))
          (%fbo-id (%attachment-fbo obj))))

(defmethod print-object ((object fbo) stream)
  (format stream "#<~a~@[ COLOR-ATTACHMENTS ~a~]~@[ DEPTH-ATTACHMENT ~a~]>"
          (if (%fbo-is-default object) "DEFAULT-FBO" "FBO")
          (loop :for i :from 0 :for j :across (%fbo-attachment-color object)
             :when (%attachment-gpu-array j) :collect i)
          (and (%attachment-gpu-array (%fbo-attachment-depth object)) t)))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun %make-default-framebuffer (dimensions &optional (double-buffering t) (depth t))
  (let ((result
         (%update-fbo-state
          (%make-fbo
           :id 0
           :is-default t
           :attachment-color
           (make-array 4 :element-type 'attachment :initial-contents
                       (list (%make-default-attachment t dimensions)
                             (%make-default-attachment double-buffering
                                                       dimensions)
                             (%make-default-attachment nil dimensions)
                             (%make-default-attachment nil dimensions)))
           :attachment-depth (%make-default-attachment depth dimensions)))))
    (update-clear-mask result)
    (setf %default-framebuffer result
          %current-fbo result)
    result))

(defun %make-default-attachment (enabled dimensions)
  (%make-attachment
   :gpu-array (when enabled (%make-default-attachment-gpu-array dimensions))))

(defun %make-default-attachment-gpu-array (dimensions)
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type :gl-internal
   :dimensions dimensions
   :image-format :gl-internal))

(defun %set-default-fbo-viewport (new-dimensions)
  (let ((fbo %default-framebuffer))
    (loop :for c :across (%fbo-attachment-color fbo) :do
       (let ((garray (%attachment-gpu-array c)))
         (when garray
           (cepl.textures::with-gpu-array-t garray
             (setf dimensions new-dimensions)))))
    (cepl.textures::with-gpu-array-t
	(%attachment-gpu-array (%fbo-attachment-depth fbo))
      (setf dimensions new-dimensions))))

;; {TODO} this is pretty wasteful but will do for now
(defun attachment-viewport (attachment)
  (make-viewport (gpu-array-dimensions (%attachment-gpu-array attachment))
                 (v! 0 0)))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun %update-fbo-state (fbo)
  (update-draw-buffer-map fbo)
  (update-clear-mask fbo)
  (update-parent-for-all-attachments fbo))

(defun update-parent-for-all-attachments (fbo)
  (loop :for a :across (%fbo-attachment-color fbo) :do
     (setf (%attachment-fbo a) fbo))
  (setf (%attachment-fbo (%fbo-attachment-depth fbo)) fbo)
  fbo)

(defun update-clear-mask (fbo)
  (setf (%fbo-clear-mask fbo)
        (cffi:foreign-bitfield-value
         '%gl::ClearBufferMask
         `(:color-buffer-bit
           ,@(and (%attachment-gpu-array (%fbo-attachment-depth fbo))
                  '(:depth-buffer-bit))
           ;; ,@(list (and (attachment-gpu-array (%fbo-attachment-stencil object))
           ;;              :stencil-buffer-bit))
           )))
  fbo)

(defun update-draw-buffer-map (fbo)
  (let ((ptr (%fbo-draw-buffer-map fbo))
        (default-fbo (%fbo-is-default fbo)))
    (loop :for i :from 0 :for attachment :across (%fbo-attachment-color fbo) :do
       (setf (mem-aref ptr 'cl-opengl-bindings:enum i)
             (if (%attachment-gpu-array attachment)
                 (if default-fbo
                     (default-fbo-attachment-enum i)
                     (color-attachment-enum i))
                 :none))))
  fbo)

(let ((vals #(#.(cffi:foreign-enum-value '%gl:enum :back-left)
              #.(cffi:foreign-enum-value '%gl:enum :front-left)
              #.(cffi:foreign-enum-value '%gl:enum :back-right)
              #.(cffi:foreign-enum-value '%gl:enum :front-right))))
  (defun default-fbo-attachment-enum (attachment-num)
    (aref vals attachment-num)))

;;--------------------------------------------------------------
;; Macro to write the helper func and compiler macro

(defun replace-attachment-array (gpu-array attachment)
  ;; {TODO} ^^ seems like a bad name
  (%make-attachment
   :gpu-array gpu-array
   :owns-gpu-array (%attachment-owns-gpu-array attachment)
   :blending-enabled (%attachment-blending-enabled attachment)
   :override-blending (%attachment-override-blending attachment)
   :blending-params (cepl.blending:copy-blending-params
		     (%attachment-blending-params
		      attachment))))

(defun attachment-gpu-array (attachment)
  (%attachment-gpu-array attachment))

(defun (setf attachment-gpu-array) (value attachment)
  (let* ((fbo (%attachment-fbo attachment))
         (color-index (or (find attachment (%fbo-attachment-color fbo)) -1))
         (is-depth (eq attachment (%fbo-attachment-depth fbo)))
         ;;{TODO} add is-stencil here
         (attachment-enum (cond (is-depth :depth-attachment)
                                ;;(is-stencil :stencil-attachment)
                                (t (color-attachment-enum color-index)))))

    (when (%fbo-is-default fbo) (error "Cannot modify attachments of default-framebuffer"))
    (unless (or (>= color-index 0) is-depth)
      (error "FBO Internal state mismatch - Attachment thinks it belongs to an fbo, the fbo disagrees."))
    ;; update gl
    (fbo-detach fbo attachment-enum)
    (fbo-attach fbo value attachment-enum)
    ;; update structs
    (setf (%attachment-gpu-array attachment) value)
    (%update-fbo-state (%attachment-fbo attachment))
    ;; and we're done
    attachment))



;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; NOTE: The following seperation is to allow shadowing in compose-pipelines
(defun attachment (fbo attachment-name)
  (%attachment fbo attachment-name))

(defun %attachment (fbo attachment-name)
  (case attachment-name
    (:d (%fbo-attachment-depth fbo))
    ;; (:s (%fbo-attachment-depth fbo))
    ;; (:ds (%fbo-attachment-depth fbo))
    (otherwise (aref (%fbo-attachment-color fbo) attachment-name))))
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun (setf attachment) (value fbo attachment-name)
  ;;{TODO} add support for :S and :ds
  (when (%fbo-is-default fbo) (error "Cannot modify attachments on the default framebuffer"))
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
  (%update-fbo-state fbo)
  value)

(let ((max-draw-buffers -1))
  (defun get-gl-attachment-keyword (x)
    (unless (> max-draw-buffers 0)
      (setf max-draw-buffers (max-draw-buffers *gl-context*)))
    (cond ((eq x :c) (color-attachment-enum 0))
          ((eq x :d) #.(cffi:foreign-enum-value '%gl:enum :depth-attachment))
          ((eq x :s) #.(cffi:foreign-enum-value '%gl:enum :stencil-attachment))
          ((eq x :ds) #.(cffi:foreign-enum-value
                         '%gl:enum :depth-stencil-attachment))
          (t (if (<= x max-draw-buffers)
                 (color-attachment-enum x)
                 (error "Requested attachment ~s is outside the range of 0-~s supported by your current context"
                        x max-draw-buffers))))))

(define-compiler-macro get-gl-attachment-keyword (&whole whole x)
  (cond ((eq x :c) (color-attachment-enum 0))
        ((eq x :d) #.(cffi:foreign-enum-value '%gl:enum :depth-attachment))
        ((eq x :s) #.(cffi:foreign-enum-value '%gl:enum :stencil-attachment))
        ((eq x :ds) #.(cffi:foreign-enum-value
                       '%gl:enum :depth-stencil-attachment))
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
                   (when (and (>= num 0)
                              (< num (max-draw-buffers *gl-context*)))
                     num))))))))

;;--------------------------------------------------------------

(defun %fbo-draw-buffers (fbo)
  (let ((len (if (%fbo-is-default fbo)
                 1
                 (max-draw-buffers *gl-context*))))
    (%gl:draw-buffers len (%fbo-draw-buffer-map fbo))))

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
;;     (with-fbo-bound (fbo)
;;       (map-g #'prog-1 stream :tex tx))
;;   (print c0)
;;   (print d))

;;--------------------------------------------------------------

(defun make-fbo-from-id (gl-object &key color-attachments depth-attachment)
  "Will create an fbo and optionally attach the arguments using
   #'fbo-gen-attach"
  (let ((fbo (%make-fbo :id gl-object)))
    (loop :for c :in color-attachments :for i :from 0 :do
       (when c (setf (attachment fbo i) c)))
    (when depth-attachment
      (setf (attachment fbo :d) depth-attachment))
    fbo))

(defun make-fbo (&rest fuzzy-attach-args)
  (let ((fbo (make-fbo-from-id (first (gl:gen-framebuffers 1)))))
    (if fuzzy-attach-args
        (apply #'fbo-gen-attach fbo fuzzy-attach-args)
        (error "Jungl: FBOs must have at least one attachment"))
    (check-framebuffer-status fbo)
    fbo))

(defun check-framebuffer-status (fbo)
  (%bind-fbo fbo :framebuffer)
  (unwind-protect
       (let ((status (gl:check-framebuffer-status :framebuffer)))
         (unless (member status '(:framebuffer-complete
                                  :framebuffer-complete-ext
                                  :framebuffer-complete-oes))
           (error "check-framebuffer-status: Code:~s~%~s"
                  status
                  (case status
                    ((:framebuffer-undefined :framebuffer-undefined-oes :framebuffer-undefined-ext)
                     "target​ is the default framebuffer, but the default framebuffer does not exist.")
                    ((:framebuffer-incomplete-attachment :framebuffer-incomplete-attachment-oes :framebuffer-incomplete-attachment-ext)
                     "one or more of the framebuffer attachment points are framebuffer incomplete.")
                    ((:framebuffer-incomplete-missing-attachment :framebuffer-incomplete-missing-attachment-oes :framebuffer-incomplete-missing-attachment-ext)
                     "the framebuffer does not have at least one image attached to it.")
                    ((:framebuffer-incomplete-draw-buffer :framebuffer-incomplete-draw-buffer-oes :framebuffer-incomplete-draw-buffer-ext)
                     "the value of :FRAMEBUFFER-ATTACHMENT-OBJECT-TYPE is :NONE for any color attachment point(s) named by :DRAW-BUFFERi.")
                    ((:framebuffer-incomplete-read-buffer :framebuffer-incomplete-read-buffer-oes :framebuffer-incomplete-read-buffer-ext)
                     ":READ-BUFFER is not :NONE and the value of :FRAMEBUFFER-ATTACHMENT-OBJECT-TYPE is :NONE for the color attachment point named by :READ-BUFFER.")
                    ((:framebuffer-unsupported :framebuffer-unsupported-oes :framebuffer-unsupported-ext)
                     "the combination of internal formats of the attached images violates an implementation-dependent set of restrictions.")
                    ((:framebuffer-incomplete-multisample :framebuffer-incomplete-multisample-oes :framebuffer-incomplete-multisample-ext)
                     "the value of :RENDERBUFFER-SAMPLES is not the same for all attached renderbuffers; if the value of :TEXTURE-SAMPLES is the not same for all attached textures; or, if the attached images are a mix of renderbuffers and textures, the value of :RENDERBUFFER-SAMPLES does not match the value of :TEXTURE-SAMPLES.
-OR-
the value of :TEXTURE-FIXED-SAMPLE-LOCATIONS is not the same for all attached textures ; or, if the attached images are a mix of renderbuffers and textures, the value of :TEXTURE-FIXED-SAMPLE-LOCATIONS is not :TRUE for all attached textures.")
                    ((:framebuffer-incomplete-layer-targets :framebuffer-incomplete-layer-targets-oes :framebuffer-incomplete-layer-targets-ext)
                     "any framebuffer attachment is layered, and any populated attachment is not layered, or if all populated color attachments are not from textures of the same target.")
                    (otherwise "An error occurred")))))
    (%unbind-fbo)))

(defun make-fbos (&optional (count 1))
  (unless (> count 0)
    (error "Attempting to create invalid number of framebuffers: ~s" count))
  (mapcar (lambda (x) (let ((f (%make-fbo :id x))) (%update-fbo-state f) f))
          (gl:gen-framebuffers count)))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun %delete-fbo (fbo)
  (gl:delete-framebuffers (listify (%fbo-id fbo))))

(defun %delete-fbos (&rest fbos)
  (gl:delete-framebuffers (mapcar #'%fbo-id fbos)))

(defmethod free ((thing fbo))
  (if (%fbo-is-default thing)
      (error "Cannot free the default framebuffer")
      (print "FREE FBO NOT IMPLEMENTED - LEAKING")))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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


(defmacro with-fbo-bound ((fbo &key (target :framebuffer) (unbind t)
                              (with-viewport t) (attachment-for-size 0)
                              (with-blending t) (draw-buffers t))
                         &body body)
  `(let* ((%current-fbo ,fbo))
     (%bind-fbo %current-fbo ,target)
     ,(%write-draw-buffer-pattern-call
       draw-buffers '%current-fbo with-blending
       `(,@(if with-viewport
               `(with-fbo-viewport (%current-fbo ,attachment-for-size))
               '(progn))
           ,@body
           (when ,unbind (%unbind-fbo))
           %current-fbo))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %write-draw-buffer-pattern-call (pattern fbo with-blending &rest body)
    "This plays with the dispatch call from compose-pipelines
   The idea is that the dispatch func can preallocate one array
   with the draw-buffers patterns for ALL the passes in it, then
   we just upload from that one block of memory.
   All of this can be decided at compile time. It's gonna go fast!"
    (cond ((null pattern) `(progn ,@body))
          ((equal pattern t)
           `(progn
              (%fbo-draw-buffers ,fbo)
              ,@(if with-blending
                    `((%with-blending ,fbo t nil ,@body))
                    body)))
          ((listp pattern)
           (destructuring-bind (pointer len attachments) pattern
             (assert (numberp len))
             `(progn (%gl:draw-buffers ,len ,pointer)
                     (%with-blending ,fbo ,attachments nil ,@body)
                     (cffi:incf-pointer
                      ,pointer ,(* len (foreign-type-size 'cl-opengl-bindings:enum)))))))))

;; Attaching Images

;; Remember that textures are a set of images. Textures can have mipmaps; thus,
;; each individual mipmap level can contain one or more images.

;; {TODO} Ensure image formats are color-renderable for color attachments
;;
(defun fbo-attach (fbo tex-array attachment &optional (target :framebuffer))
  ;; To attach images to an FBO, we must first bind the FBO to the context.
  ;; target can be '(:framebuffer :read-framebuffer :draw-framebuffer)
  (dbind (tex-array . owned-by-fbo) (listify tex-array)
    (setf (%attachment-owns-gpu-array (attachment fbo attachment)) owned-by-fbo)
    (let ((attach-enum (get-gl-attachment-keyword attachment)))
      (with-fbo-bound (fbo :target target :with-viewport nil :draw-buffers nil)
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
	(cepl.textures::with-gpu-array-t tex-array
	  (unless (attachment-compatible attachment image-format)
	    (error "attachment is not compatible with this array"))
	  (let ((tex-id (texture-id texture)))
	    (case (gpu-array-t-texture-type tex-array)
	      ;; A 1D texture contains 2D images that have the vertical height of 1.
	      ;; Each individual image can be uniquely identified by a mipmap level.
	      (:texture-1d (progn
			     (setf (attachment fbo attachment) tex-array)
			     (gl:framebuffer-texture-1d target attach-enum :texture-1d
							tex-id level-num)))
	      ;; A 2D texture contains 2D images. Each individual image can be
	      ;; uniquely identified by a mipmap level.
	      (:texture-2d (progn
			     (setf (attachment fbo attachment) tex-array)
			     (gl:framebuffer-texture-2d target attach-enum :texture-2d
							tex-id level-num)))
	      ;; Each mipmap level of a 3D texture is considered a set of 2D images,
	      ;; with the number of these being the extent of the Z coordinate.
	      ;; Each integer value for the depth of a 3D texture mipmap level is a
	      ;; layer. So each image in a 3D texture is uniquely identified by a
	      ;; layer and a mipmap level.
	      ;; A single mipmap level of a 3D texture is a layered image, where the
	      ;; number of layers is the depth of that particular mipmap level.
	      (:texture-3d (progn
			     (setf (attachment fbo attachment) tex-array)
			     (%gl:framebuffer-texture-layer target attach-enum tex-id
							    level-num layer-num)))
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
				    target attach-enum tex-id level-num layer-num)))
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
				    target attach-enum tex-id level-num layer-num)))
	      ;; A Rectangle Texture has a single 2D image, and thus is identified by
	      ;; mipmap level 0.
	      (:texture-rectangle
	       (progn
		 (setf (attachment fbo attachment) tex-array)
		 (gl:framebuffer-texture-2d target attach-enum :texture-2d
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
		  target attach-enum '&&&CUBEMAP-TARGET&&&
		  tex-id level-num)))
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
	      (:texture-cube-map-array (error "attaching to cube-map-array textures has not been implmented yet")))))))))

(defun fbo-gen-attach (fbo &rest args)
  "The are 3 kinds of valid argument:
   - keyword naming an attachment: This makes a new texture
     with size of (current-viewport) and attaches
   - (keyword texarray): attaches the tex-array
   - (keyword texture): attaches the root tex-array
   - (keyword some-type) any types that supports the generic dimensions function
                         creates a new texture at the framesize of the object
                         and attaches it to attachment named by keyword"
  (let ((target (%extract-target (first args))))
    (mapcar (lambda (texture-pair attachment)
              (fbo-attach fbo texture-pair attachment target))
            (mapcar #'%gen-textures args)
            (mapcar (lambda (x) (binding-shorthand (first x)))
                    (mapcar #'listify args)))))

(defun %extract-target (x)
  (if (member x '(:draw-framebuffer :read-framebuffer :framebuffer :framebuffer))
      x
      :draw-framebuffer))

(defvar %possible-texture-keys '(:dimensions :element-type :mipmap
                                 :layer-count :cubes-p :rectangle
                                 :multisample :immutable :buffer-storage
                                 :lod-bias :min-lod :max-lod :minify-filter
                                 :magnify-filter :wrap :compare))
(defvar %valid-texture-subset '(:dimensions :element-type :mipmap
                                :immutable :lod-bias :min-lod :max-lod
                                :minify-filter :magnify-filter :wrap :compare))



(defun %gen-textures (pattern)
  (assert (or (listp pattern) (keywordp pattern)))
  (cond
    ;; simple keyword pattern to texture
    ((keywordp pattern)
     (cons (texref
	    (make-texture nil :dimensions (viewport-dimensions (current-viewport))
			  :element-type (%get-default-texture-format pattern)))
	   t))
    ;; pattern with args for make-texture
    ((some (lambda (x) (member x %possible-texture-keys)) pattern)
     (when (some (lambda (x) (and (member x %possible-texture-keys)
                                  (not (member x %valid-texture-subset))))
                 pattern)
         (error "Only the following args to make-texture are allowed inside a make-fbo ~s"
                %valid-texture-subset))
     (destructuring-bind
           (&key (dimensions (viewport-dimensions (current-viewport)))
                 (element-type (%get-default-texture-format (first pattern)))
                 mipmap (immutable t) lod-bias min-lod max-lod minify-filter
                 magnify-filter wrap compare)
         (rest pattern)
       (assert (attachment-compatible (first pattern) element-type))
       (cons (texref
	      (make-texture nil
			    :dimensions dimensions
			    :element-type element-type
			    :mipmap mipmap
			    :immutable immutable
			    :lod-bias lod-bias
			    :min-lod min-lod
			    :max-lod max-lod
			    :minify-filter minify-filter
			    :magnify-filter magnify-filter
			    :wrap wrap
			    :compare compare))
	     t)))
    ;; use an existing gpu-array
    ((typep (second pattern) 'gpu-array-t) (cons (second pattern) t))
    ;; use the first gpu-array in texture
    ((typep (second pattern) 'texture) (cons (texref (second pattern)) t))
    ;; take the dimensions from some object
    (t (cons (texref
	      (make-texture nil :dimensions (dimensions (second pattern))
			    :element-type (%get-default-texture-format
					   (first pattern))))
	     t))))

(defun %get-default-texture-format (attachment)
  (assert (keywordp attachment))
  (let ((char (char-downcase (aref (symbol-name attachment) 0))))
    (cond ((char= char #\c) :rgba8)
          ((char= char #\d) :depth-component24)
          (t (error "No default texture format for attachment: ~s" attachment)))))

(defun attachment-compatible (attachment image-format)
  (case attachment
    ((:d :depth-attachment) (depth-formatp image-format))
    ((:s :stencil-attachment) (stencil-formatp image-format))
    ((:ds :depth-stencil-attachment) (depth-stencil-formatp image-format))
    (otherwise (color-renderable-formatp image-format))))

(defun fbo-detach (fbo attachment)
  ;; The texture argument is the texture object name you want to attach from.
  ;; If you pass zero as texture, this has the effect of clearing the attachment
  ;; for this attachment, regardless of what kind of image was attached there.
  (setf (attachment fbo attachment) nil)
  (%gl:framebuffer-texture-layer :draw-framebuffer attachment 0 0 0))

(defun clear (&optional (target %current-fbo))
  (if (typep target 'attachment)
      (clear-attachment target)
      (clear-fbo target)))

(defun clear-fbo (fbo)
  (%gl:clear (%fbo-clear-mask fbo)))

(defun clear-attachment (attachment)
  (declare (ignore attachment))
  (error "CEPL: clear-attachment is not yet implemented"))
