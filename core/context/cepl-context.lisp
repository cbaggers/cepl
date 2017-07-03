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
  (%with-cepl-context (gl-context gl-version current-surface gl-thread) cepl-context
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
  (%with-cepl-context (current-viewport
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
              default-viewport vp)))
    cepl-context))

;;----------------------------------------------------------------------

(defn register-gpu-buffer ((cepl-context cepl-context)
                           (gpu-buffer gpu-buffer))
    gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (let ((id (gpu-buffer-id gpu-buffer)))
    (declare (type fixnum id))
    (assert (> id 0) (id)
            "Attempted to register ~s before id fully initialized"
            'gpu-buffer)
    (%with-cepl-context (array-of-gpu-buffers)
        cepl-context
      (ensure-vec-index array-of-gpu-buffers id +null-gpu-buffer+)
      (setf (aref array-of-gpu-buffers id) gpu-buffer))))

(defn register-texture ((cepl-context cepl-context) (texture texture))
    texture
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context (array-of-textures)
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
  (%with-cepl-context (fbos) cepl-context
    (let ((id (%fbo-id fbo)))
      (ensure-vec-index fbos id +null-fbo+)
      (setf (aref fbos id) fbo))))

;;----------------------------------------------------------------------
;; GPU-Buffers

;; Actually making the state change

(defn ensure-buffer-bound-id ((cepl-context cepl-context)
                              (index (integer 0 11)))
    (values)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (profile t))
  (%with-cepl-context (array-of-actual-bound-gpu-buffer-ids
                       array-of-bound-gpu-buffer-ids)
      cepl-context
    (let ((id (aref array-of-bound-gpu-buffer-ids index)))
      (when (/= id (aref array-of-actual-bound-gpu-buffer-ids index))
        (let* ((id (if (unknown-gl-id-p id) 0 id))
               (cache-id->enum-id
                #(34962 37568 36662 36663 37102 36671 34963 35051 35052 37266
                  37074 35882)))
          (declare (dynamic-extent cache-id->enum-id))
          (%gl:bind-buffer (aref cache-id->enum-id index) id)
          (setf (aref array-of-actual-bound-gpu-buffer-ids index) id)))))
  (values))

;; Raw Cache indexed part

(defn-inline gpu-buffer-bound-id ((ctx cepl-context) (index (integer 0 11)))
    gl-id
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (profile t))
  (%with-cepl-context (array-of-bound-gpu-buffer-ids) ctx
    (declare (type (simple-array gl-id) array-of-bound-gpu-buffer-ids))
    (aref array-of-bound-gpu-buffer-ids index)))

(defn set-gpu-buffer-bound-id ((ctx cepl-context)
                               (index (integer 0 11))
                               (id gl-id)
                               &optional (eager boolean nil))
    gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline unknown-gl-id-p)
           (profile t))
  (%with-cepl-context (array-of-bound-gpu-buffer-ids
                       array-of-actual-bound-gpu-buffer-ids
                       gl-context)
      ctx
    (setf (aref array-of-bound-gpu-buffer-ids index) id)
    (when eager
      (ensure-buffer-bound-id ctx index))
    id))

;; User friendly part

(defn buffer-kind->cache-index ((kind keyword)) (integer 0 11)
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

(defn gpu-buffer-bound ((ctx cepl-context)
                        (target symbol))
    gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context (array-of-gpu-buffers gl-context) ctx
    (let* ((index (buffer-kind->cache-index target))
           (id (gpu-buffer-bound-id ctx index))
           (id
            (if (unknown-gl-id-p id)
                (set-gpu-buffer-bound-id ctx index (gl:get* target))
                id)))
      (when (and (>= id 0) (< (length array-of-gpu-buffers)))
        (aref array-of-gpu-buffers id)))))

(defn (setf gpu-buffer-bound) ((val gpu-buffer)
                               (ctx cepl-context)
                               (target symbol))
    gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (let ((index (buffer-kind->cache-index target))
        (id (if val (gpu-buffer-id val) 0)))
    (set-gpu-buffer-bound-id ctx index id t))
  val)

(define-compiler-macro (setf gpu-buffer-bound) (&whole whole val ctx target)
  (if (keywordp target)
      (let ((index (buffer-kind->cache-index target)))
        (alexandria:with-gensyms (gval id)
          `(let* ((,gval ,val)
                  (,id (if ,gval (gpu-buffer-id ,gval) 0)))
             (set-gpu-buffer-bound-id ,ctx ,index ,id t))))
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
  (%with-cepl-context (array-of-ubo-bindings-buffer-ids) ctx
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
  (%with-cepl-context (array-of-transform-feedback-bindings-buffer-ids) ctx
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

;; GL binding part

;; (mapcar (lambda (x) (cffi:foreign-enum-value '%gl::enum x))
;;         '(:texture-1d :texture-2d :texture-3d :texture-1d-array
;;           :texture-2d-array :texture-rectangle :texture-cube-map
;;           :texture-cube-map-array :texture-buffer :texture-2d-multisample
;;           :texture-2d-multisample-array))
(defn ensure-texture-id-bound ((cepl-context cepl-context)
                               (index (integer 0 10)))
    (values)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (profile t))
  (%with-cepl-context (array-of-actual-bound-texture-ids
                       array-of-bound-texture-ids)
      cepl-context
    (let ((id (aref array-of-bound-texture-ids index)))
      (when (/= id (aref array-of-actual-bound-texture-ids index))
        (let* ((cache-id->enum-id
                #(3552 3553 32879 35864 35866 34037
                  34067 36873 35882 37120 37122))
               (id (if (unknown-gl-id-p id) 0 id)))
          (declare (dynamic-extent cache-id->enum-id))
          ;; {TODO} we have already calculated the enum, try and remove the
          ;;        condition checking if keyword
          (%gl:bind-texture (aref cache-id->enum-id index) id)
          (setf (aref array-of-actual-bound-texture-ids index) id)))))
  (values))

;; (mapcar (lambda (x) (cffi:foreign-enum-value '%gl::enum x))
;;         '(:texture-binding-1d :texture-binding-2d :texture-binding-3d
;;           :texture-binding-1d-array :texture-binding-2d-array
;;           :texture-binding-rectangle :texture-binding-cube-map
;;           :texture-binding-cube-map-array :texture-binding-buffer
;;           :texture-binding-2d-multisample
;;           :texture-binding-2d-multisample-array))
(defn %texture-binding ((gl-ctx gl-context)
                        (index (integer 0 10)))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (ignore gl-ctx) (profile t))
  (let* ((cache-id->enum-id #(32872 32873 32874 35868 35869 34038
                              34068 36874 35884 37124 37125))
         (enum-val (aref cache-id->enum-id index)))
    (the (signed-byte 32) (cl-opengl:get-integer enum-val 1))))

;; Raw cached index part

(defn-inline texture-bound-id ((ctx cepl-context)
                               (index array-index))
    gl-id
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
  (%with-cepl-context (array-of-bound-texture-ids) ctx
    (aref array-of-bound-texture-ids index)))

(defn set-texture-bound-id ((ctx cepl-context)
                            (index array-index)
                            (id gl-id)
                            &optional (eager boolean nil))
    gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline unknown-gl-id-p) (profile t))
  (%with-cepl-context (array-of-bound-texture-ids gl-context) ctx
    (setf (aref array-of-bound-texture-ids index) id)
    (when eager
      (ensure-texture-id-bound ctx index))
    id))

;; human friendly part

(defn tex-kind->cache-index ((kind symbol)) (integer 0 10)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (ecase kind
    (:texture-1d 0)
    (:texture-2d 1)
    (:texture-3d 2)
    (:texture-1d-array 3)
    (:texture-2d-array 4)
    (:texture-rectangle 5)
    (:texture-cube-map 6)
    (:texture-cube-map-array 7)
    (:texture-buffer 8)
    (:texture-2d-multisample 9)
    (:texture-2d-multisample-array 10)))

(defn texture-bound ((ctx cepl-context)
                     (target symbol))
    texture
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline unknown-gl-id-p) (profile t))
  (let ((index (tex-kind->cache-index target)))
    (%with-cepl-context (array-of-textures gl-context)
        ctx
      (let* ((id (texture-bound-id ctx index))
             (id
              (if (unknown-gl-id-p id)
                  (set-texture-bound-id
                   ctx index (%texture-binding gl-context index) t)
                  id)))
        (aref array-of-textures id)))))

(defn (setf texture-bound) ((val texture)
                            (ctx cepl-context)
                            (target symbol))
    texture
  (let ((index (tex-kind->cache-index target)))
    (set-texture-bound-id ctx index (texture-id val) t)
    val))


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
  (%with-cepl-context (fbos read-fbo-binding-id) cepl-context
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
  (%with-cepl-context (fbos read-fbo-binding-id) cepl-context
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
  (%with-cepl-context (fbos draw-fbo-binding-id) cepl-context
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
  (%with-cepl-context (fbos draw-fbo-binding-id) cepl-context
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
  (%with-cepl-context (fbos read-fbo-binding-id draw-fbo-binding-id)
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

(defn vertex-array-binding ((cepl-context cepl-context)) (unsigned-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (declare (ignore cepl-context))
  (the (unsigned-byte 32) (cl-opengl:get* :vertex-array-binding)))

(defn (setf vertex-array-binding) ((id gl-id) (cepl-context cepl-context))
    gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (declare (ignore cepl-context))
  (%gl:bind-vertex-array id)
  id)

(defn vao-bound ((cepl-context cepl-context)) gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context (vao-binding-id) cepl-context
    (if (= vao-binding-id +unknown-gl-id+)
        (setf vao-binding-id (vertex-array-binding cepl-context))
        vao-binding-id)))

(defn (setf vao-bound) ((vao gl-id) (cepl-context cepl-context)) gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context (vao-binding-id) cepl-context
    (when (/= vao-binding-id vao)
      (setf (vertex-array-binding cepl-context) vao)
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

(defun %inner-with-context (cepl-context body ctx-var)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (if (eq cepl-context ctx-var)
      `(progn ,@body)
      (%with-context cepl-context body ctx-var)))

(defun %with-context (cepl-context body ctx-var)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (let ((ctx (or ctx-var (gensym "CTX"))))
    `(let ((,ctx ,cepl-context))
       (declare (ignorable ,ctx))
       (macrolet ((l-identity (context)
                    (declare (ignore context))
                    ',ctx)
                  (with-cepl-context
                      ((&optional (cepl-context ',ctx))
                       &body body)
                    (%inner-with-context cepl-context body ',ctx)))
         ,@body))))

(defmacro with-cepl-context ((&optional (cepl-context '(cepl-context)))
                             &body body)
  (%with-context cepl-context body nil))

(defn-inline cepl-context () cepl-context
  *cepl-context*)

(define-compiler-macro cepl-context ()
  `(l-identity *cepl-context*))
