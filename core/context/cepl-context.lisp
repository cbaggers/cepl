(in-package :cepl.context)

;;----------------------------------------------------------------------

(defvar *contexts* nil)

(defun2 make-context (&key gl-version (shared (first *contexts*))
                           (title "CEPL") (width 600) (height 600)
                           (fullscreen nil) (resizable t) (no-frame nil)
                           (hidden nil))
  (declare (ignore title width height fullscreen resizable no-frame hidden))
  ;;
  (assert (or (null shared) (typep shared 'cepl-context)))
  ;;
  (let* ((shared (if shared
                     (%cepl-context-shared shared)
                     (make-instance 'cepl-context-shared)))
         (result (%make-cepl-context
                  :gl-version gl-version
                  :shared shared
                  :current-surface nil
                  :surfaces nil)))
    (push result (slot-value shared 'members))
    (when cepl.host::*current-host*
      (on-host-initialized result))
    (push result *contexts*)
    ;; done!
    result))

(declaim (type cepl-context *cepl-context*))
(defvar *cepl-context* (make-context))

;;----------------------------------------------------------------------

;; {TODO} move this to delayed-resource-init.lisp
(defvar *on-context* nil)

(defn init-gl-context ((cepl-context cepl-context) (surface t))
    cepl-context
  (declare (profile t))
  (assert cepl-context)
  (assert surface)
  (%with-cepl-context-slots (gl-context gl-version current-surface gl-thread) cepl-context
    (setf gl-thread (bt:current-thread))
    (assert (not gl-context))
    (let ((raw-context (cepl.host:make-gl-context :version gl-version
                                                  :surface surface)))
      (ensure-cepl-compatible-setup)
      (let ((wrapped-context
             (make-instance
              'gl-context
              :handle raw-context
              :version-major (gl:major-version)
              :version-minor (gl:minor-version)
              :version-float (coerce (+ (gl:major-version)
                                        (/ (gl:minor-version) 10))
                                     'single-float))))
        ;;
        ;; hack until we support contexts properly
        (setf *gl-context* wrapped-context)
        ;;
        (setf gl-context wrapped-context)
        ;;
        ;; {TODO} Hmm this feels wrong
        (map nil #'funcall *on-context*)
        ;;
        ;; Set the default
        (%set-default-fbo-and-viewport surface cepl-context)
        (setf current-surface surface)
        ;;
        ;; Set GL Defaults
        (set-context-defaults cepl-context)
        ;;
        ;; initialize all the pending objects
        (initialize-all-delay-items-in-context cepl-context)
        ;;
        ;;
        cepl-context))))

(defn ensure-cepl-compatible-setup () boolean
  (declare (profile t))
  (unless (or (> (gl:major-version) 3)
              (and (= (gl:major-version) 3)
                   (>= (gl:minor-version) 1)))
    (error "Cepl requires OpenGL 3.1 or higher. Found: ~a.~a"
           (gl:major-version) (gl:minor-version))))

(defn %set-default-fbo-and-viewport ((surface t) (cepl-context cepl-context))
    cepl-context
  (declare (profile t))
  (%with-cepl-context-slots (current-viewport
                             default-viewport
                             default-framebuffer) cepl-context
    ;;
    (let* ((surface-size (cepl.host:window-size surface))
           (fbo (cepl.fbos::%make-default-framebuffer surface-size t t)))
      ;;
      ;; Setup default fbo
      (setf (%cepl-context-default-framebuffer cepl-context) fbo)
      ;;
      ;; Setup Viewports
      (let ((vp (make-viewport surface-size)))
        (setf current-viewport vp
              default-viewport vp)
        (%gl:viewport
         (%viewport-origin-x vp) (%viewport-origin-y vp)
         (%viewport-resolution-x vp) (%viewport-resolution-y vp))))
    cepl-context))

;;----------------------------------------------------------------------

(defn register-gpu-buffer ((cepl-context cepl-context)
                           (gpu-buffer gpu-buffer))
    gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (let ((id (gpu-buffer-id gpu-buffer)))
    (declare (type gl-id id))
    (assert (> id 0) (id)
            "Attempted to register ~s before id fully initialized"
            'gpu-buffer)
    (%with-cepl-context-slots (array-of-gpu-buffers)
        cepl-context
      (ensure-vec-index array-of-gpu-buffers id +null-gpu-buffer+)
      (setf (aref array-of-gpu-buffers id) gpu-buffer))))

(defn register-texture ((cepl-context cepl-context) (texture texture))
    texture
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (array-of-textures)
      cepl-context
    (let ((id (texture-id texture)))
      (declare (type gl-id id))
      (assert (> id 0) (id)
              "Attempted to register ~s before id fully initialized" 'texture)
      (ensure-vec-index array-of-textures id +null-texture+)
      (setf (aref array-of-textures id) texture))))

(defn register-fbo ((cepl-context cepl-context) (fbo fbo)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (fbos) cepl-context
    (let ((id (%fbo-id fbo)))
      (ensure-vec-index fbos id +null-fbo+)
      (setf (aref fbos id) fbo))))

;;----------------------------------------------------------------------
;; GPU-Buffers

;; Raw Cache indexed part

(defn-inline buffer-bound-static ((ctx cepl-context) (index (integer 0 11)))
    gpu-buffer
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (array-of-bound-gpu-buffers) ctx
    (aref array-of-bound-gpu-buffers index)))

(defn-inline set-buffer-bound-static ((ctx cepl-context)
                                      (buffer (or null gpu-buffer))
                                      (index (integer 0 11))
                                      (enum (signed-byte 32)))
    gpu-buffer
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (array-of-bound-gpu-buffers) ctx
    (when (not (eq buffer (aref array-of-bound-gpu-buffers index)))
      (let ((id (if buffer (gpu-buffer-id buffer) 0)))
        (%gl:bind-buffer enum id))
      (setf (aref array-of-bound-gpu-buffers index) buffer))
    buffer))

;; User friendly part

(defn-inline buffer-kind->cache-index ((kind keyword)) (integer 0 11)
  ;; :atomic-counter-buffer
  ;; :shader-storage-buffer
  (declare (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0))
           (profile t))
  (ecase kind
    (:array-buffer 0)
    (:atomic-counter-buffer 1)
    (:copy-read-buffer 2)
    (:copy-write-buffer 3)
    (:dispatch-indirect-buffer 4)
    (:draw-indirect-buffer 5)
    (:element-array-buffer 6)
    (:pixel-pack-buffer 7)
    (:pixel-unpack-buffer 8)
    (:query-buffer 9)
    (:shader-storage-buffer 10)
    (:texture-buffer 11)))

(defn-inline buffer-kind->enum ((kind keyword)) (signed-byte 32)
  ;; :atomic-counter-buffer
  ;; :shader-storage-buffer
  (declare (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0))
           (profile t))
  (ecase kind
    (:array-buffer
     #.(cffi:foreign-enum-value '%gl:enum :array-buffer))
    (:atomic-counter-buffer
     #.(cffi:foreign-enum-value '%gl:enum :atomic-counter-buffer))
    (:copy-read-buffer
     #.(cffi:foreign-enum-value '%gl:enum :copy-read-buffer))
    (:copy-write-buffer
     #.(cffi:foreign-enum-value '%gl:enum :copy-write-buffer))
    (:dispatch-indirect-buffer
     #.(cffi:foreign-enum-value '%gl:enum :dispatch-indirect-buffer))
    (:draw-indirect-buffer
     #.(cffi:foreign-enum-value '%gl:enum :draw-indirect-buffer))
    (:element-array-buffer
     #.(cffi:foreign-enum-value '%gl:enum :element-array-buffer))
    (:pixel-pack-buffer
     #.(cffi:foreign-enum-value '%gl:enum :pixel-pack-buffer))
    (:pixel-unpack-buffer
     #.(cffi:foreign-enum-value '%gl:enum :pixel-unpack-buffer))
    (:query-buffer
     #.(cffi:foreign-enum-value '%gl:enum :query-buffer))
    (:shader-storage-buffer
     #.(cffi:foreign-enum-value '%gl:enum :shader-storage-buffer))
    (:texture-buffer
     #.(cffi:foreign-enum-value '%gl:enum :texture-buffer))))

(defn gpu-buffer-bound ((cepl-context cepl-context) (target symbol)) gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline buffer-bound-static)
           (profile t))
  (%with-cepl-context-slots (array-of-gpu-buffers gl-context) cepl-context
    (buffer-bound-static cepl-context (buffer-kind->cache-index target))))

(define-compiler-macro gpu-buffer-bound (&whole whole ctx target)
  (if (keywordp target)
      (let ((index (buffer-kind->cache-index target)))
        `(locally (declare (inline buffer-bound-static))
           (buffer-bound-static ,ctx ,index)))
      whole))

(defn (setf gpu-buffer-bound) ((val (or null gpu-buffer))
                               (ctx cepl-context)
                               (target symbol))
    gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline set-buffer-bound-static
                   buffer-kind->cache-index
                   buffer-kind->enum)
           (profile t))
  (let* ((index (buffer-kind->cache-index target))
         (enum (buffer-kind->enum target)))
    (set-buffer-bound-static ctx val index enum))
  val)

(define-compiler-macro (setf gpu-buffer-bound) (&whole whole val ctx target)
  (if (keywordp target)
      (let ((index (buffer-kind->cache-index target))
            (enum (buffer-kind->enum target)))
        `(locally (declare (inline set-buffer-bound-static))
           (set-buffer-bound-static ,ctx ,val ,index ,enum)))
      whole))

;;----------------------------------------------------------------------
;; Uniform Buffer Objects
;;
;; UBOs don't exist as a true GLObjects. There are a number of bindings points
;; which you can attach regions of a gpu-buffer to so that pipelines can read
;; from them as uniforms.
;;
;; Although this is really about gpu-buffers we choose to keep this seperate
;; from the gpu-buffer section above as the GL context has multiple ubo
;; binding-points trying to mix them in the cache above was more confusing than
;; helpful.

(defn ubo-bind-buffer-id-range ((ctx cepl-context)
                                (id gl-id)
                                (ubo-binding-point array-index)
                                (offset (unsigned-byte 32))
                                (size (unsigned-byte 32)))
    gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline unknown-gl-id-p)
           (profile t))
  (assert (and offset size))
  ;; don't worry about checking cache for avoiding rebinding as we dont want to
  ;; cache ranges (yet?)
  (%with-cepl-context-slots (array-of-ubo-bindings-buffer-ids) ctx
    (ensure-vec-index array-of-ubo-bindings-buffer-ids ubo-binding-point
                      +null-gl-id+ gl-id)
    (let ((bind-id (if (unknown-gl-id-p id) 0 id)))
      (%gl:bind-buffer-range
       :uniform-buffer ubo-binding-point bind-id offset size)
      (setf (aref array-of-ubo-bindings-buffer-ids ubo-binding-point) id)
      id)))

;;----------------------------------------------------------------------
;; Transform Feedback Buffers
;;
;; Although this is really about gpu-buffers we choose to keep this seperate
;; from the gpu-buffer section above as the GL context has multiple tfb
;; binding-points trying to mix them in the cache above was more confusing than
;; helpful.

(defn transform-feedback-bind-buffer-id-range ((ctx cepl-context)
                                               (id gl-id)
                                               (tfb-binding-point gl-id)
                                               (offset (unsigned-byte 32))
                                               (size (unsigned-byte 32)))
    gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline unknown-gl-id-p)
           (profile t))
  (assert (and offset size))
  ;; don't worry about checking cache for avoiding rebinding as we dont want to
  ;; cache ranges (yet?)
  (%with-cepl-context-slots (array-of-transform-feedback-bindings-buffer-ids) ctx
    (ensure-vec-index array-of-transform-feedback-bindings-buffer-ids
                      tfb-binding-point
                      +null-gl-id+
                      gl-id)
    (let ((bind-id (if (unknown-gl-id-p id) 0 id)))
      (%gl:bind-buffer-range
       :uniform-buffer tfb-binding-point bind-id offset size)
      (setf (aref array-of-transform-feedback-bindings-buffer-ids
                  tfb-binding-point)
            id)
      id)))

;;----------------------------------------------------------------------

;; Sampler implementation in sampler/context.lisp as requires functions
;; defined later in the system

;;----------------------------------------------------------------------

;; GL_READ_FRAMEBUFFER_BINDING (name, intially 0, see glBindFramebuffer)
;;     The framebuffer object currently bound to the GL_READ_FRAMEBUFFER target. If the default framebuffer is bound, this value will be zero.
(defn read-framebuffer-binding ((context cepl-context))
    (unsigned-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (ignore context) (profile t))
  (the (unsigned-byte 32) (cl-opengl:get* :read-framebuffer-binding)))

(defn (setf read-framebuffer-binding) ((id gl-id) (context cepl-context)) gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (ignore context) (profile t))
  (%gl:bind-framebuffer :read-framebuffer id)
  id)

;; GL_DRAW_FRAMEBUFFER_BINDING (name, initially 0, see glBindFramebuffer)
;;     The framebuffer object currently bound to the GL_DRAW_FRAMEBUFFER target. If the default framebuffer is bound, this value will be zero.
(defn draw-framebuffer-binding ((context cepl-context)) (unsigned-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (ignore context) (profile t))
  (the (unsigned-byte 32) (cl-opengl:get* :draw-framebuffer-binding)))

(defn (setf draw-framebuffer-binding) ((id gl-id) (context cepl-context)) gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (ignore context) (profile t))
  (%gl:bind-framebuffer :draw-framebuffer id)
  id)

;; The GL_FRAMEBUFFER target sets both the read and the write to the same FBO.
(defn framebuffer-binding ((context cepl-context)) cons
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (cons (read-framebuffer-binding context)
        (draw-framebuffer-binding context)))

(defn (setf framebuffer-binding) ((id gl-id) (context cepl-context))
    gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (ignore context) (profile t))
  (%gl:bind-framebuffer :framebuffer id)
  id)

(defn read-fbo-bound ((cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (fbos read-fbo-binding-id) cepl-context
    (let* ((id (if (= read-fbo-binding-id +unknown-gl-id+)
                   (setf read-fbo-binding-id
                         (read-framebuffer-binding cepl-context))
                   read-fbo-binding-id))
           (fbo (when (>= id 0) (aref fbos id))))
      (assert (not (eq fbo +null-fbo+)))
      fbo)))

(defn (setf read-fbo-bound) ((fbo fbo) (cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (fbos read-fbo-binding-id) cepl-context
    (let ((id (if fbo
                  (%fbo-id fbo)
                  0)))
      (when (/= id read-fbo-binding-id)
        (setf (read-framebuffer-binding cepl-context) id
              read-fbo-binding-id id))
      fbo)))

(defn draw-fbo-bound ((cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (fbos draw-fbo-binding-id) cepl-context
    (let* ((id (if (= draw-fbo-binding-id +unknown-gl-id+)
                   (setf draw-fbo-binding-id
                         (draw-framebuffer-binding cepl-context))
                   draw-fbo-binding-id))
           (fbo (when (>= id 0) (aref fbos id))))
      (assert (not (eq fbo +null-fbo+)))
      fbo)))

(defn (setf draw-fbo-bound) ((fbo fbo) (cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (fbos draw-fbo-binding-id) cepl-context
    (let ((id (if fbo
                  (%fbo-id fbo)
                  0)))
      (when (/= id draw-fbo-binding-id)
        (setf (draw-framebuffer-binding cepl-context) id
              draw-fbo-binding-id id))
      fbo)))

(defn fbo-bound ((cepl-context cepl-context)) cons
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (cons (read-fbo-bound cepl-context)
        (draw-fbo-bound cepl-context)))

(defn (setf fbo-bound) ((fbo fbo) (cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (assert (typep fbo 'fbo))
  (%with-cepl-context-slots (fbos read-fbo-binding-id draw-fbo-binding-id)
      cepl-context
    (let* ((id (if fbo
                   (%fbo-id fbo)
                   0))
           (r-dif (/= id read-fbo-binding-id))
           (d-dif (/= id draw-fbo-binding-id)))
      (cond
        ((and r-dif d-dif) (setf (framebuffer-binding cepl-context) id))
        (r-dif (setf (read-framebuffer-binding cepl-context) id))
        (d-dif (setf (draw-framebuffer-binding cepl-context) id)))
      (setf draw-fbo-binding-id id
            read-fbo-binding-id id)
      fbo)))

;;----------------------------------------------------------------------

;; GL_VERTEX_ARRAY_BINDING (GLint, initially 0, see glBindVertexArray)
;; The name of the vertex array object currently bound to the context, or 0 if
;; none is bound.

(defn vao-bound ((cepl-context cepl-context)) gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (vao-binding-id) cepl-context
    (if (= vao-binding-id +unknown-gl-id+)
        (setf vao-binding-id (the (unsigned-byte 32)
                                  (cl-opengl:get* :vertex-array-binding)))
        vao-binding-id)))

(defn (setf vao-bound) ((vao gl-id) (cepl-context cepl-context)) gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (vao-binding-id) cepl-context
    (when (/= vao-binding-id vao)
      (%gl:bind-vertex-array vao)
      (setf vao-binding-id vao)))
  vao)

;;----------------------------------------------------------------------

(defn patch-uninitialized-context-with-version ((cepl-context cepl-context)
                                                gl-version)
    t
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (when (not (%cepl-context-gl-version cepl-context))
    (setf (%cepl-context-gl-version cepl-context) gl-version)))

;;----------------------------------------------------------------------

(defmacro l-identity (context)
  "An identity macro. Exists so it can be shadowed in certain contexts"
  ;; l for local..bad name, but the others I had at the time were worse.
  context)

(defun %inner-with-context (var-name cepl-context forgo-let body ctx-var)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (if (eq cepl-context ctx-var)
      (if var-name
          `(let ((,var-name ,ctx-var))
             ,@body)
          `(progn ,@body))
      (%with-context var-name cepl-context forgo-let body ctx-var)))

(defun %with-context (var-name cepl-context forgo-let body ctx-var)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (assert (constantp forgo-let))
  (let ((ctx (or ctx-var (gensym "CTX"))))
    `(let* ((,ctx ,cepl-context)
            ,@(when var-name `((,var-name ,ctx)))
            ,@(unless forgo-let `((*cepl-context* ,ctx))))
       (declare (ignorable ,ctx))
       (macrolet ((l-identity (context)
                    (declare (ignore context))
                    ',ctx)
                  (with-cepl-context
                      ((&optional var-name  (cepl-context ',ctx) forgo-let)
                       &body body)
                    (%inner-with-context
                     var-name cepl-context forgo-let body ',ctx)))
         ,@body))))

(defmacro with-cepl-context ((&optional var-name (cepl-context '(cepl-context))
                                        forgo-let)
                             &body body)
  (%with-context var-name cepl-context forgo-let body nil))

(defn-inline cepl-context () cepl-context
  *cepl-context*)

(define-compiler-macro cepl-context ()
  `(l-identity *cepl-context*))
