(in-package :cepl.context)

;;----------------------------------------------------------------------
;;

(defvar *contexts-lock* (bt:make-lock))
(defvar *contexts* nil)

(defun assert-no-other-context-is-bound-to-thread (this-thread)
  (loop :for context :in *contexts* :do
     (%with-cepl-context-slots (bound-thread) context
       (let ((existing-context-thread bound-thread))
         (assert (not (eq this-thread existing-context-thread))
                 (this-thread existing-context-thread)
                 'tried-to-make-context-on-thread-that-already-has-one
                 :context context
                 :thread this-thread)))))

(defun+ make-context (&key (gl-version t))
  (make-context-internals nil gl-version))

(defun+ make-context-internals (is-implicit-context-p &optional (gl-version t))
  ;;
  ;; Make sure there is only 1 cepl context per thread
  (bt:with-lock-held (*contexts-lock*)
    ;; we delay thread bindings for the implicit cepl-context
    (let ((this-thread (if is-implicit-context-p
                           nil
                           (bt:current-thread))))
      (when this-thread
        (assert-no-other-context-is-bound-to-thread this-thread))
      (let* ((gl-version (cond
                           ((and (eq gl-version t) *contexts*)
                            (let ((ctx (first *contexts*)))
                              (if (> (%cepl-context-gl-version-float ctx) 0.0)
                                  (%cepl-context-gl-version-float ctx)
                                  (%cepl-context-requested-gl-version ctx))))
                           ((eq gl-version t) nil)
                           (t gl-version)))
             (shared-arr (make-array 0 :fill-pointer 0 :adjustable t))
             (result (%make-cepl-context
                      :id (get-free-context-id)
                      :bound-thread this-thread
                      :requested-gl-version gl-version
                      :current-surface nil
                      :shared shared-arr
                      :surfaces nil)))
        (vector-push-extend result (%cepl-context-shared result))
        (push result *contexts*)
        ;; done!
        result))))

(defun+ free-context (cepl-context)
  (format t "free-context not yet implemented. Leaking ~a" cepl-context))

;;----------------------------------------------------------------------
;; Implicit Context & Inlining Logic

(declaim (type cepl-context *cepl-context*))
(defvar *cepl-context* (make-context-internals t))
(defvar *primary-context* *cepl-context*)

(defn primary-context () cepl-context
  *primary-context*)

(defn primary-thread () (or null bt:thread)
  (%cepl-context-bound-thread (primary-context)))

(defmacro l-identity (context)
  "An identity macro. Exists so it can be shadowed in certain contexts"
  ;; l for local..bad name, but the others I had at the time were worse.
  context)

(defun %inner-with-context (var-name cepl-context forgo-let body ctx-var)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (if (eq cepl-context ctx-var)
      (if var-name
          `(let ((,var-name ,ctx-var))
             (declare (ignorable ,var-name))
             ,@body)
          `(progn ,@body))
      (%with-context var-name cepl-context forgo-let body ctx-var)))

(defun %with-context (var-name cepl-context forgo-let body ctx-var)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))
  (assert (constantp forgo-let))
  (let* ((getting-current (eq cepl-context '(cepl-context)))
         (forgo-let (or forgo-let getting-current))
         (ctx (or ctx-var (gensym "CTX")))
         (tmp (gensym "tmp-ctx")))
    `(let* ((,tmp ,cepl-context)
            (,ctx (etypecase ,tmp
                    (cepl-context ,tmp)
                    (unbound-cepl-context (complete-unbound-context ,tmp))))
            ,@(when var-name `((,var-name ,ctx)))
            ,@(unless forgo-let `((*cepl-context* ,ctx))))
       (declare (ignorable ,ctx))
       (macrolet ((l-identity (context)
                    (declare (ignore context))
                    ',ctx)
                  (with-cepl-context
                      ((&optional var-name (cepl-context ',ctx) forgo-let)
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

;;----------------------------------------------------------------------
;; Shared Contexts

(defn make-context-shared-with-current-context ()
    unbound-cepl-context
  (declare (profile t))
  (let ((cepl-context (cepl-context)))
    (assert cepl-context)
    (%with-cepl-context-slots (gl-context
                               requested-gl-version
                               current-surface
                               bound-thread
                               surfaces)
        cepl-context
      (assert (eq bound-thread (bt:current-thread)) ()
              'shared-context-created-from-incorrect-thread
              :ctx-thread bound-thread
              :init-thread (bt:current-thread))
      (assert current-surface ()
              "CEPL: The CEPL context you attempted to share with does not have a surface current")
      (multiple-value-bind (new-raw-gl-context new-surface)
          (cepl.host:make-gl-context-shared-with-current-context
           :current-gl-context gl-context
           :version (version-float gl-context)
           :surface current-surface)
        (cepl.host:make-gl-context-current-on-surface (handle gl-context)
                                                      current-surface)
        (let* ((wrapped-context
                (make-instance
                 'gl-context
                 :handle new-raw-gl-context
                 :version-major (major-version gl-context)
                 :version-minor (minor-version gl-context)
                 :version-float (version-float gl-context))))
          (make-unbound-cepl-context
           :requested-gl-version requested-gl-version
           :shared cepl-context
           :gl-context wrapped-context
           :surface new-surface
           :surfaces surfaces))))))

(defun+ complete-unbound-context (unbound-context)
  (bt:with-lock-held (*contexts-lock*)
    (let ((this-thread (bt:current-thread)))
      (assert-no-other-context-is-bound-to-thread this-thread)
      (assert (not (unbound-cepl-context-consumed unbound-context)))
      (let* ((gl-context (unbound-cepl-context-gl-context unbound-context))
             (shared (unbound-cepl-context-shared unbound-context))
             (surface (unbound-cepl-context-surface unbound-context))
             (surfaces (unbound-cepl-context-surfaces unbound-context))
             (surfaces (if (find surface surfaces)
                           surfaces
                           (cons surface surfaces)))
             (gl-version (unbound-cepl-context-requested-gl-version
                          unbound-context))
             (shared-arr (%cepl-context-shared shared))
             (result (%make-cepl-context
                      :id (get-free-context-id)
                      :gl-context gl-context
                      :bound-thread this-thread
                      :requested-gl-version gl-version
                      :current-surface nil
                      :shared shared-arr
                      :surfaces surfaces)))
        (vector-push-extend result (%cepl-context-shared result))
        (setf (%cepl-context-array-of-gpu-buffers result)
              (%cepl-context-array-of-gpu-buffers shared))
        (setf (%cepl-context-array-of-textures result)
              (%cepl-context-array-of-textures shared))
        (push result *contexts*)
        (make-surface-current result surface)
        (setf (unbound-cepl-context-consumed unbound-context) t)
        ;; done!
        result))))

;;----------------------------------------------------------------------
;; Define Functions for interacting with the current context

(defmacro define-context-func (name args ret-type context-slots &body body)
  "This simple encodes a pattern I was writing too many times.
   Basically we want to have the call to #'cepl-context inline
   at the callsite as then a surrounding with-cepl-context block
   will be able to replace it with a local version (improving performance)
   the way we have taken to doing this "
  (let* ((setfp (and (listp name) (eq (first name) 'setf)))
         (hname (if setfp
                    (symb-package (symbol-package (second name))
                                  :%set- (second name))
                    (symb-package (symbol-package name) :% name)))
         (args-opt (if (find :&optional args :test #'symb-name=)
                       args
                       `(,@args &optional)))
         (arg-symbs (mapcar
                     (lambda (x) (if (listp x) (first x) x))
                     args-opt))
         (arg-names (remove-if
                     (lambda (x) (char= #\& (char (symbol-name x) 0)))
                     arg-symbs)))
    (multiple-value-bind (body decls doc)
        (alexandria:parse-body body :documentation t)
      (let* ((not-inline (find 'not-inline-internals
                               decls :key #'second :test #'string=))
             (decls (remove not-inline decls))
             (def (if not-inline 'defn 'defn-inline)))
        `(progn
           (,def ,hname (,@args (cepl-context cepl-context)) ,ret-type
                 ,@(when doc (list doc))
                 (declare (optimize (speed 3) (debug 0) (safety 1))
                          (profile t))
                 ,@decls
                 (with-cepl-context (cepl-context cepl-context t)
                   (%with-cepl-context-slots ,context-slots cepl-context
                     ,@body)))
           (defn ,name (,@args-opt (cepl-context cepl-context (cepl-context)))
               ,ret-type
             (declare (optimize (speed 3) (debug 1) (safety 1))
                      (profile t))
             (,hname ,@arg-names cepl-context))
           (define-compiler-macro ,name (,@arg-symbs cepl-context)
             (if cepl-context
                 (list ',hname ,@arg-names cepl-context)
                 (list ',hname ,@arg-names '(cepl-context)))))))))

;;----------------------------------------------------------------------

(defn init-gl-context ((cepl-context cepl-context) (surface t))
    cepl-context
  (declare (profile t))
  (assert cepl-context)
  (assert surface)
  (%with-cepl-context-slots (gl-context gl-version-float requested-gl-version
                                        current-surface bound-thread)
      cepl-context
    (assert (not gl-context))
    (let ((this-thread (bt:current-thread)))
      (when (and (eq cepl-context (primary-context))
                 (null bound-thread))
        (setf bound-thread (bt:current-thread)))
      (assert (eq bound-thread this-thread) (bound-thread this-thread)
              'gl-context-initialized-from-incorrect-thread
              :ctx-thread bound-thread
              :init-thread this-thread))
    (let ((raw-context (cepl.host:make-gl-context :version requested-gl-version
                                                  :surface surface)))
      (cepl.host:make-gl-context-current-on-surface raw-context surface)
      (ensure-cepl-compatible-setup)
      (let* ((maj (gl:major-version))
             (min (gl:minor-version))
             (ver-f (float (+ maj (/ min 10)) 0f0))
            (wrapped-context
             (make-instance
              'gl-context
              :handle raw-context
              :version-major maj
              :version-minor min
              :version-float ver-f)))
        ;;
        (setf gl-context wrapped-context)
        (setf gl-version-float ver-f)
        ;;
        ;; Set GL Defaults
        (set-context-defaults cepl-context)
        ;;
        ;; {TODO} this is ugly, find a better way
        (funcall 'cepl.samplers::sampler-on-context)
        (funcall 'cepl.textures::check-immutable-feature)
        (funcall 'cepl.samplers::check-anisotrophy-feature)
        ;;
        ;; Set the default
        (%set-default-fbo-and-viewport surface cepl-context)
        (setf current-surface surface)
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
      (setf (%cepl-context-default-framebuffer cepl-context) fbo
            (%cepl-context-read-fbo-binding cepl-context) fbo
            (%cepl-context-draw-fbo-binding cepl-context) fbo)
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

(defn forget-gpu-buffer ((cepl-context cepl-context)
                           (gpu-buffer gpu-buffer))
    gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (array-of-gpu-buffers)
      cepl-context
    (setf (aref array-of-gpu-buffers (gpu-buffer-id gpu-buffer))
          +null-gpu-buffer+)))

(defn forget-texture ((cepl-context cepl-context) (texture texture))
    texture
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (array-of-textures)
      cepl-context
    (setf (aref array-of-textures (texture-id texture))
          +null-texture+)))

(defn forget-fbo ((cepl-context cepl-context) (fbo fbo)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (fbos) cepl-context
    (setf (aref fbos (%fbo-id fbo)) +null-fbo+)))

;;----------------------------------------------------------------------
;; GPU-Buffers

;; Raw Cache indexed part

(defn-inline buffer-bound-static ((ctx cepl-context) (index (integer 0 11)))
    (or gpu-buffer null)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (array-of-bound-gpu-buffers) ctx
    (aref array-of-bound-gpu-buffers index)))

(defn-inline set-buffer-bound-static ((ctx cepl-context)
                                      (buffer (or null gpu-buffer))
                                      (index (integer 0 11))
                                      (enum gl-enum-value))
    (or null gpu-buffer)
  (declare (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0))
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

(defn-inline buffer-kind->enum ((kind keyword)) gl-enum-value
  ;; :atomic-counter-buffer
  ;; :shader-storage-buffer
  (declare (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0))
           (profile t))
  (ecase kind
    (:array-buffer
     #.(gl-enum :array-buffer))
    (:atomic-counter-buffer
     #.(gl-enum :atomic-counter-buffer))
    (:copy-read-buffer
     #.(gl-enum :copy-read-buffer))
    (:copy-write-buffer
     #.(gl-enum :copy-write-buffer))
    (:dispatch-indirect-buffer
     #.(gl-enum :dispatch-indirect-buffer))
    (:draw-indirect-buffer
     #.(gl-enum :draw-indirect-buffer))
    (:element-array-buffer
     #.(gl-enum :element-array-buffer))
    (:pixel-pack-buffer
     #.(gl-enum :pixel-pack-buffer))
    (:pixel-unpack-buffer
     #.(gl-enum :pixel-unpack-buffer))
    (:query-buffer
     #.(gl-enum :query-buffer))
    (:shader-storage-buffer
     #.(gl-enum :shader-storage-buffer))
    (:texture-buffer
     #.(gl-enum :texture-buffer))))

(defn gpu-buffer-bound ((cepl-context cepl-context) (target symbol))
    (or null gpu-buffer)
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
    (or null gpu-buffer)
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
;; UBOS & SSBOS
;;
;; UBOs & SSBOs don't exist as a true GLObjects. There are a number of bindings
;; points which you can attach regions of a gpu-buffer to so that pipelines
;; can read from them as uniforms.
;;
;; Although this is really about gpu-buffers we choose to keep this seperate
;; from the gpu-buffer section above as the GL context has multiple ubo
;; binding-points trying to mix them in the cache above was more confusing than
;; helpful.

(defn %register-ubo-id ((ctx cepl-context) (ubo-binding-point array-index))
    (values)
  (%with-cepl-context-slots (array-of-ubo-bindings-buffer-ids) ctx
    (ensure-vec-index array-of-ubo-bindings-buffer-ids ubo-binding-point
                      +null-gl-id+ gl-id))
  (values))

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
  (%with-cepl-context-slots (array-of-ubo-bindings-buffer-ids
                             array-of-ubo-binding-ranges)
      ctx
    (let ((bind-id (if (unknown-gl-id-p id) 0 id))
          (range-index (the array-index (+ (* id 2) 1))))
      (ensure-vec-index array-of-ubo-bindings-buffer-ids ubo-binding-point
                        +null-gl-id+ gl-id)
      (ensure-vec-index array-of-ubo-binding-ranges range-index
                        0 (unsigned-byte 32))
      (%gl:bind-buffer-range
       #.(gl-enum :uniform-buffer)
       ubo-binding-point
       bind-id
       offset
       size)
      (setf (aref array-of-ubo-bindings-buffer-ids ubo-binding-point) id)
      (setf (aref array-of-ubo-binding-ranges (- range-index 1)) offset)
      (setf (aref array-of-ubo-binding-ranges range-index) size)
      id)))

(defn %register-ssbo-id ((ctx cepl-context) (ssbo-binding-point array-index))
    (values)
  (%with-cepl-context-slots (array-of-ssbo-bindings-buffer-ids) ctx
    (ensure-vec-index array-of-ssbo-bindings-buffer-ids ssbo-binding-point
                      +null-gl-id+ gl-id))
  (values))

(defn ssbo-bind-buffer-id-range ((ctx cepl-context)
                                 (id gl-id)
                                 (ssbo-binding-point array-index)
                                 (offset (unsigned-byte 32))
                                 (size (unsigned-byte 32)))
    gl-id
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (inline unknown-gl-id-p)
           (profile t))
  (assert (and offset size))
  ;; don't worry about checking cache for avoiding rebinding as we dont want to
  ;; cache ranges (yet?)
  (%with-cepl-context-slots (array-of-ssbo-bindings-buffer-ids
                             array-of-ssbo-binding-ranges)
      ctx
    (let ((bind-id (if (unknown-gl-id-p id) 0 id))
          (range-index (the array-index (+ (* id 2) 1))))
      (ensure-vec-index array-of-ssbo-bindings-buffer-ids ssbo-binding-point
                        +null-gl-id+ gl-id)
      (ensure-vec-index array-of-ssbo-binding-ranges range-index
                        0 (unsigned-byte 32))
      (%gl:bind-buffer-range
       #.(gl-enum :shader-storage-buffer)
       ssbo-binding-point
       bind-id
       offset
       size)
      (setf (aref array-of-ssbo-bindings-buffer-ids ssbo-binding-point) id)
      (setf (aref array-of-ssbo-binding-ranges (- range-index 1)) offset)
      (setf (aref array-of-ssbo-binding-ranges range-index) size)
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
       #.(gl-enum :transform-feedback-buffer)
       tfb-binding-point bind-id offset size)
      (setf (aref array-of-transform-feedback-bindings-buffer-ids
                  tfb-binding-point)
            id)
      id)))

;;----------------------------------------------------------------------

;; Sampler implementation in sampler/context.lisp as requires functions
;; defined later in the system

;;----------------------------------------------------------------------

(defn-inline %set-read-fbo-no-check ((cepl-context cepl-context) (fbo fbo))
    (values)
  ;; used in cases where the caller is doing the bound check
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (read-fbo-binding) cepl-context
    (%gl:bind-framebuffer #.(gl-enum :read-framebuffer) (%fbo-id fbo))
    (setf read-fbo-binding fbo)
    (values)))

(defn-inline %set-draw-fbo-no-check ((cepl-context cepl-context) (fbo fbo))
    (values)
  ;; used in cases where the caller is doing the bound check
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (draw-fbo-binding) cepl-context
    (%gl:bind-framebuffer #.(gl-enum :draw-framebuffer) (%fbo-id fbo))
    (setf draw-fbo-binding fbo)
    (values)))

(defn-inline %set-fbo-no-check ((cepl-context cepl-context) (fbo fbo)) (values)
  (%with-cepl-context-slots (read-fbo-binding draw-fbo-binding)
      cepl-context
    (%gl:bind-framebuffer #.(gl-enum :framebuffer) (%fbo-id fbo))
    (setf read-fbo-binding fbo)
    (setf draw-fbo-binding fbo))
  (values))

(defn-inline read-fbo-bound ((cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (read-fbo-binding current-surface) cepl-context
    (let ((read-fbo read-fbo-binding))
      (assert read-fbo () 'fbo-binding-missing
              :kind "read"
              :current-surface current-surface)
      read-fbo)))

(defn (setf read-fbo-bound) ((fbo fbo) (cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (read-fbo-binding) cepl-context
    (unless (eq fbo read-fbo-binding)
      (%gl:bind-framebuffer #.(gl-enum :read-framebuffer) (%fbo-id fbo))
      (setf read-fbo-binding fbo))
    fbo))

(defn-inline draw-fbo-bound ((cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (draw-fbo-binding current-surface) cepl-context
    (let ((draw-fbo draw-fbo-binding))
      (assert draw-fbo () 'fbo-binding-missing
              :kind "draw"
              :current-surface current-surface)
      draw-fbo)))

(defn (setf draw-fbo-bound) ((fbo fbo) (cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (draw-fbo-binding) cepl-context
    (unless (eq fbo draw-fbo-binding)
      (%gl:bind-framebuffer #.(gl-enum :draw-framebuffer) (%fbo-id fbo))
      (setf draw-fbo-binding fbo))
    fbo))

(defn fbo-bound ((cepl-context cepl-context)) (values fbo fbo)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (values (read-fbo-bound cepl-context)
          (draw-fbo-bound cepl-context)))

(defn %set-fbo-bound ((cepl-context cepl-context) (fbo fbo))
    (values boolean boolean)
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (read-fbo-binding draw-fbo-binding)
      cepl-context
    (let* ((r-eq (eq fbo read-fbo-binding))
           (d-eq (eq fbo draw-fbo-binding))
           (id (%fbo-id fbo)))
      (if r-eq
          (unless d-eq
            (%gl:bind-framebuffer #.(gl-enum :draw-framebuffer) id)
            (setf draw-fbo-binding fbo))
          (if d-eq
              (progn
                (%gl:bind-framebuffer #.(gl-enum :read-framebuffer) id)
                (setf read-fbo-binding fbo))
              (progn
                (%gl:bind-framebuffer #.(gl-enum :framebuffer) id)
                (setf read-fbo-binding fbo)
                (setf draw-fbo-binding fbo))))
      (values r-eq d-eq))))

(defn-inline (setf fbo-bound) ((fbo fbo) (cepl-context cepl-context)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (assert (typep fbo 'fbo))
  (%set-fbo-bound cepl-context fbo)
  fbo)

;;----------------------------------------------------------------------

(defn-inline can-bind-query-p ((cepl-context cepl-context)
                               (query gpu-query))
    (values boolean (or null gpu-query))
  (%with-cepl-context-slots (array-of-bound-queries) cepl-context
    (let ((currently-bound (aref array-of-bound-queries
                                 (gpu-query-cache-id query))))
      (values (null currently-bound)
              currently-bound))))

(defn-inline force-bind-query ((cepl-context cepl-context)
                               (query gpu-query))
    gpu-query
  (%with-cepl-context-slots (array-of-bound-queries) cepl-context
    (setf (aref array-of-bound-queries (gpu-query-cache-id query)) query)
    (setf (scoped-gpu-query-active-p query) t))
  query)

(defn-inline force-unbind-query ((cepl-context cepl-context)
                                 (query gpu-query))
    gpu-query
  (%with-cepl-context-slots (array-of-bound-queries) cepl-context
    (setf (aref array-of-bound-queries (gpu-query-cache-id query)) nil)
    (setf (scoped-gpu-query-active-p query) nil))
  query)

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

(defn force-bind-vao ((vao gl-id) (cepl-context cepl-context)) gl-id
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (profile t))
  (%with-cepl-context-slots (vao-binding-id) cepl-context
    (%gl:bind-vertex-array vao)
    (setf vao-binding-id vao)))

;;----------------------------------------------------------------------

(defn patch-uninitialized-context-with-version ((cepl-context cepl-context)
                                                requested-gl-version)
    t
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (when (not (%cepl-context-requested-gl-version cepl-context))
    (setf (%cepl-context-requested-gl-version cepl-context)
          requested-gl-version)))

;;----------------------------------------------------------------------

(defn gl-initialized-p (&optional (context cepl-context (cepl-context)))
    boolean
  (not (null (%cepl-context-gl-context context))))

;;----------------------------------------------------------------------

(defn surfaces (&optional (cepl-context cepl-context (cepl-context))) list
  (%cepl-context-surfaces cepl-context))

(defn current-surface (&optional (cepl-context cepl-context (cepl-context))) t
  (%cepl-context-current-surface cepl-context))

;;----------------------------------------------------------------------

(defmethod version-float ((ctx cepl-context))
  (%cepl-context-gl-version-float ctx))

;;----------------------------------------------------------------------
