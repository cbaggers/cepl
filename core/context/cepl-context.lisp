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
                     (slot-value shared 'shared)
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

(defvar *cepl-context* (make-context))

;;----------------------------------------------------------------------

;; {TODO} move this to delayed-resource-init.lisp
(defvar *on-context* nil)

(defun2 init-gl-context (cepl-context surface)
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

(defun2 ensure-cepl-compatible-setup ()
  (unless (or (> (gl:major-version) 3)
              (and (= (gl:major-version) 3)
                   (>= (gl:minor-version) 1)))
    (error "Cepl requires OpenGL 3.1 or higher. Found: ~a.~a"
           (gl:major-version) (gl:minor-version))))

(defun2 %set-default-fbo-and-viewport (surface cepl-context)
  (%with-cepl-context (current-viewport
               default-viewport
               default-framebuffer) cepl-context
    ;;
    (let* ((surface-size (cepl.host:window-size surface))
           (fbo (cepl.fbos::%make-default-framebuffer surface-size t t)))
      ;;
      ;; Setup default fbo
      (setf (slot-value cepl-context 'default-framebuffer) fbo)
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
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (let ((id (gpu-buffer-id gpu-buffer)))
    (declare (type fixnum id))
    (assert (> id 0) (id)
            "Attempted to register ~s before id fully initialized"
            'gpu-buffer)
    (%with-cepl-context (array-of-gpu-buffers)
        cepl-context
      (ensure-vec-index array-of-gpu-buffers id +null-gpu-buffer+)
      (setf (aref array-of-gpu-buffers id) gpu-buffer))))

(defun2 register-texture (cepl-context texture)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (array-of-textures)
      cepl-context
    (let ((id (texture-id texture)))
      (declare (type gl-id id))
      (assert (> id 0) (id)
              "Attempted to register ~s before id fully initialized" 'texture)
      (ensure-vec-index array-of-textures id +null-texture+)
      (setf (aref array-of-textures id) texture))))

(defun2 register-fbo (cepl-context fbo)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (fbos) cepl-context
    (let ((id (%fbo-id fbo)))
      (ensure-vec-index fbos id +null-fbo+)
      (setf (aref fbos id) fbo))))

;;----------------------------------------------------------------------
;; GPU-Buffers

;; Raw Cache indexed part

(defn-inline gpu-buffer-bound-id ((ctx cepl-context) (index (integer 0 11)))
    gl-id
  (declare (profile t))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (array-of-bound-gpu-buffer-ids) ctx
    (declare (type (simple-array gl-id) array-of-bound-gpu-buffer-ids))
    (aref array-of-bound-gpu-buffer-ids index)))

(let ((cache-id->enum-id
       #(34962 37568 36662 36663 37102 36671 34963 35051 35052 37266 37074
         35882)))
  (defun2 set-gpu-buffer-bound-id (ctx index id)
    (declare (inline unknown-gl-id-p))
    (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
    (%with-cepl-context (array-of-bound-gpu-buffer-ids gl-context) ctx
      (let ((current (gpu-buffer-bound-id ctx index))
            (bind-id (if (unknown-gl-id-p id)
                         0
                         id)))
        (unless (= id current)
          (gl:bind-buffer (aref cache-id->enum-id index) bind-id)
          (setf (aref array-of-bound-gpu-buffer-ids index) id))
        id))))

;; User friendly part

(defn buffer-kind->cache-index ((kind keyword))
    (integer 0 11)
  (declare (profile t))
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

;; :atomic-counter-buffer
;; :shader-storage-buffer

(defun2 gpu-buffer-bound (ctx target)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (let ((index (buffer-kind->cache-index target)))
    (%with-cepl-context (array-of-gpu-buffers gl-context)
        ctx
      (let* ((id (gpu-buffer-bound-id ctx index))
             (id
              (if (unknown-gl-id-p id)
                  (set-gpu-buffer-bound-id ctx index (gl:get* target))
                  id)))
        (when (and (>= id 0) (< (length array-of-gpu-buffers)))
          (aref array-of-gpu-buffers id))))))

(defun2 (setf gpu-buffer-bound) (val ctx target)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (let ((index (buffer-kind->cache-index target)))
    (set-gpu-buffer-bound-id ctx index (gpu-buffer-id val))))

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

(defun2 ubo-bind-buffer-id-range
    (ctx id ubo-binding-point offset size)
  (declare (inline unknown-gl-id-p))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (assert (and offset size))
  ;; don't worry about checking cache for avoiding rebinding as we dont want to
  ;; cache ranges (yet?)
  (%with-cepl-context (array-of-ubo-bindings-buffer-ids) ctx
    (ensure-vec-index array-of-ubo-bindings-buffer-ids ubo-binding-point
                      +null-gl-id+)
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

(defun2 transform-feedback-bind-buffer-id-range
    (ctx id tfb-binding-point offset size)
  (declare (inline unknown-gl-id-p))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (assert (and offset size))
  ;; don't worry about checking cache for avoiding rebinding as we dont want to
  ;; cache ranges (yet?)
  (%with-cepl-context (array-of-transform-feedback-bindings-buffer-ids) ctx
    (ensure-vec-index array-of-transform-feedback-bindings-buffer-ids
                      tfb-binding-point
                      +null-gl-id+)
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
;;         '(:texture-binding-1d :texture-binding-2d :texture-binding-3d
;;           :texture-binding-1d-array :texture-binding-2d-array
;;           :texture-binding-rectangle :texture-binding-cube-map
;;           :texture-binding-cube-map-array :texture-binding-buffer
;;           :texture-binding-2d-multisample
;;           :texture-binding-2d-multisample-array))

(let ((cache-id->enum-id
       #(32872 32873 32874 35868 35869 34038 34068 36874 35884 37124 37125)))
  (defun2 %texture-binding (gl-ctx index)
    (declare (ignore gl-ctx))
    (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
    (let ((enum-val (aref cache-id->enum-id index)))
      (cl-opengl:get-integer enum-val 1))))

;; (mapcar (lambda (x) (cffi:foreign-enum-value '%gl::enum x))
;;         '(:texture-1d :texture-2d :texture-3d :texture-1d-array
;;           :texture-2d-array :texture-rectangle :texture-cube-map
;;           :texture-cube-map-array :texture-buffer :texture-2d-multisample
;;           :texture-2d-multisample-array))
(let ((cache-id->enum-id
       #(3552 3553 32879 35864 35866 34037 34067 36873 35882 37120 37122)))
  (defun2 (setf %texture-binding) (id gl-ctx index)
    (declare (ignore gl-ctx))
    (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
    (let ((target-val (aref cache-id->enum-id index)))
      ;; {TODO} we have already calculated the enum, try and remove the
      ;;        condition checking if keyword
      (gl:bind-texture target-val id))
    id))

;; Raw cached index part

(declaim (inline texture-bound-id))
(defun2 texture-bound-id (ctx index)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (array-of-bound-texture-ids) ctx
    (aref array-of-bound-texture-ids index)))

(defun2 set-texture-bound-id (ctx index id)
  (declare (inline unknown-gl-id-p))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (array-of-bound-texture-ids gl-context)
      ctx
    (let ((current (texture-bound-id ctx index))
          (bind-id
           (if (unknown-gl-id-p id)
               0
               id)))
      (unless (= id current)
        (setf (%texture-binding gl-context index) bind-id)
        (setf (aref array-of-bound-texture-ids index) id))
      id)))

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

(defun2 texture-bound (ctx target)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (declare (inline unknown-gl-id-p))
  (let ((index (tex-kind->cache-index target)))
    (%with-cepl-context (array-of-textures gl-context)
        ctx
      (let* ((id (texture-bound-id ctx index))
             (id
              (if (unknown-gl-id-p id)
                  (set-texture-bound-id ctx index
                                        (%texture-binding gl-context index))
                  id)))
        (when (and (>= id 0) (< (length array-of-textures)))
          (aref array-of-textures id))))))

(defun2 (setf texture-bound) (val ctx target)
  (let ((index (tex-kind->cache-index target)))
    (set-texture-bound-id ctx index (texture-id val))))


;;----------------------------------------------------------------------

;; GL_READ_FRAMEBUFFER_BINDING (name, intially 0, see glBindFramebuffer)
;;     The framebuffer object currently bound to the GL_READ_FRAMEBUFFER target. If the default framebuffer is bound, this value will be zero.
(defun2 read-framebuffer-binding (context)
  (declare (ignore context))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (cl-opengl:get* :read-framebuffer-binding))

(defun2 (setf read-framebuffer-binding) (id context)
  (declare (ignore context))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (gl:bind-framebuffer :read-framebuffer id)
  id)

;; GL_DRAW_FRAMEBUFFER_BINDING (name, initially 0, see glBindFramebuffer)
;;     The framebuffer object currently bound to the GL_DRAW_FRAMEBUFFER target. If the default framebuffer is bound, this value will be zero.
(defun2 draw-framebuffer-binding (context)
  (declare (ignore context))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (cl-opengl:get* :draw-framebuffer-binding))

(defun2 (setf draw-framebuffer-binding) (id context)
  (declare (ignore context))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (gl:bind-framebuffer :draw-framebuffer id)
  id)

;; The GL_FRAMEBUFFER target sets both the read and the write to the same FBO.
(defun2 framebuffer-binding (context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (cons (read-framebuffer-binding context)
        (draw-framebuffer-binding context)))

(defun2 (setf framebuffer-binding) (id context)
  (declare (ignore context))
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (gl:bind-framebuffer :framebuffer id)
  id)

(defun2 read-fbo-bound (cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (gl-context fbos read-fbo-binding-id) cepl-context
    (let* ((id (if (= read-fbo-binding-id +unknown-gl-id+)
                   (setf read-fbo-binding-id
                         (read-framebuffer-binding gl-context))
                   read-fbo-binding-id))
           (fbo (when (>= id 0) (aref fbos id))))
      (assert (not (eq fbo +null-fbo+)))
      fbo)))

(defun2 (setf read-fbo-bound) (fbo cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (gl-context fbos read-fbo-binding-id) cepl-context
    (let ((id (if fbo
                  (%fbo-id fbo)
                  0)))
      (when (/= id read-fbo-binding-id)
        (setf (read-framebuffer-binding gl-context) id
              read-fbo-binding-id id))
      fbo)))

(defun2 draw-fbo-bound (cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (gl-context fbos draw-fbo-binding-id) cepl-context
    (let* ((id (if (= draw-fbo-binding-id +unknown-gl-id+)
                   (setf draw-fbo-binding-id
                         (draw-framebuffer-binding gl-context))
                   draw-fbo-binding-id))
           (fbo (when (>= id 0) (aref fbos id))))
      (assert (not (eq fbo +null-fbo+)))
      fbo)))

(defun2 (setf draw-fbo-bound) (fbo cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (gl-context fbos draw-fbo-binding-id) cepl-context
    (let ((id (if fbo
                  (%fbo-id fbo)
                  0)))
      (when (/= id draw-fbo-binding-id)
        (setf (draw-framebuffer-binding gl-context) id
              draw-fbo-binding-id id))
      fbo)))

(defun2 fbo-bound (cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (cons (read-fbo-bound cepl-context)
        (draw-fbo-bound cepl-context)))

(defun2 (setf fbo-bound) (fbo cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (assert (typep fbo 'fbo))
  (%with-cepl-context (gl-context fbos read-fbo-binding-id draw-fbo-binding-id)
      cepl-context
    (let* ((id (if fbo
                   (%fbo-id fbo)
                   0))
           (r-dif (/= id read-fbo-binding-id))
           (d-dif (/= id draw-fbo-binding-id)))
      (cond
        ((and r-dif d-dif) (setf (framebuffer-binding gl-context) id))
        (r-dif (setf (read-framebuffer-binding gl-context) id))
        (d-dif (setf (draw-framebuffer-binding gl-context) id)))
      (setf draw-fbo-binding-id id
            read-fbo-binding-id id)
      fbo)))

;;----------------------------------------------------------------------

;; GL_VERTEX_ARRAY_BINDING (GLint, initially 0, see glBindVertexArray)
;; The name of the vertex array object currently bound to the context, or 0 if
;; none is bound.

(defun2 vertex-array-binding (context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (declare (ignore context))
  (cl-opengl:get* :vertex-array-binding))

(defun2 (setf vertex-array-binding) (id context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (declare (ignore context))
  (gl:bind-vertex-array id)
  id)

(defun2 vao-bound (cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (gl-context vao-binding-id) cepl-context
    (if (= vao-binding-id +unknown-gl-id+)
        (setf vao-binding-id (vertex-array-binding gl-context))
        vao-binding-id)))

(defun2 (setf vao-bound) (vao cepl-context)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (%with-cepl-context (gl-context vao-binding-id) cepl-context
    (when (/= vao-binding-id vao)
      (setf (vertex-array-binding gl-context) vao)
      (setf vao-binding-id vao)))
  vao)

;;----------------------------------------------------------------------

(defun2 patch-uninitialized-context-with-version (context gl-version)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (when (or (not (slot-boundp context 'cepl.context::gl-version))
            (not (slot-value context 'cepl.context::gl-version)))
    (setf (slot-value context 'cepl.context::gl-version)
          gl-version)))

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
