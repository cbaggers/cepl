(in-package :cepl.fbos)

(define-const +valid-fbo-targets+
    '(:read-framebuffer :draw-framebuffer :framebuffer)
  :type list)

;; {TODO} what is the meaning of an attachment with no array, no attachment?
;;        in that case should attachment-viewport return nil or error?
;;        same goes for attachment-tex. Could we use the +null-att+ instead
;;        and force attr to be populated?

(define-const +possible-texture-keys+
  '(:dimensions :element-type :mipmap :layer-count :cubes-p :rectangle
    :multisample :immutable :buffer-storage)
  :type list)

;; {TODO} A fragment shader can output different data to any of these by
;;        linking out variables to attachments with the glBindFragDataLocation
;;        function, sounds kind like a multiple-value-bind.

;;----------------------------------------------------------------------

(defun+ fbo-color-arrays (fbo)
  (loop :for i :across (%fbo-color-arrays fbo) :collect (att-array i)))

;;----------------------------------------------------------------------

(defn viewport-for-array ((arr (or null gpu-array))) (or null viewport)
  (when arr
    (make-viewport (gpu-array-dimensions arr) (vec2 0f0 0f0))))

;;----------------------------------------------------------------------

(defun+ ensure-fbo-array-size (fbo desired-size)
  (let* ((arr (%fbo-color-arrays fbo))
         (len (length arr)))
    (if (< len desired-size)
        (progn
          (loop :for i :below (- desired-size len) :do
             (vector-push-extend (make-att) arr))
          arr)
        arr)))

;;----------------------------------------------------------------------

(defun+ pre-gl-init (fbo-obj
                     &key color-arrays depth-array stencil-array
                     is-default blending-params)
  (when color-arrays
    (setf (%fbo-color-arrays fbo-obj)
          (make-array (length color-arrays)
                      :element-type 'att
                      :initial-contents (mapcar (lambda (x)
                                                  (let ((v (viewport-for-array
                                                            x)))
                                                    (make-att :array x
                                                              :viewport v)))
                                                color-arrays))))
  ;;
  (when depth-array
    (setf (%fbo-depth-array fbo-obj)
          (make-att :array depth-array
                    :viewport (viewport-for-array depth-array))))
  (when stencil-array
    (if depth-array
        (assert (and (eq depth-array stencil-array)
                     (member (element-type stencil-array)
                             *depth-stencil-formats*)))
        (assert (member (element-type stencil-array)
                        *stencil-formats*)))

    (setf (%fbo-stencil-array fbo-obj)
          (make-att :array stencil-array
                    :viewport (viewport-for-array stencil-array))))
  ;;
  (setf (%fbo-is-default fbo-obj) is-default)
  ;;
  (setf (%fbo-blending-params fbo-obj)
        (or blending-params (make-blending-params
                             :mode-rgb :func-add
                             :mode-alpha :func-add
                             :source-rgb :one
                             :source-alpha :one
                             :destination-rgb :zero
                             :destination-alpha :zero)))
  ;;
  fbo-obj)

(defun+ post-gl-init (fbo-obj &key id draw-buffer-map clear-mask)
  (setf (%fbo-id fbo-obj) (or id (first (gl::gen-framebuffers 1))))
  (setf (%fbo-draw-buffer-map fbo-obj)
        (or draw-buffer-map
            (foreign-alloc '%gl:enum :count
                           (max-draw-buffers
                            (cepl.context::%cepl-context-gl-context
                             (cepl-context)))
                           :initial-element :none)))
  (setf (%fbo-clear-mask fbo-obj)
        (or clear-mask
            (cffi:foreign-bitfield-value
             '%gl::ClearBufferMask '(:color-buffer-bit))))
  (cepl.context::register-fbo (cepl-context) fbo-obj)
  fbo-obj)

;;----------------------------------------------------------------------

(defun+ %make-default-framebuffer
    (dimensions &optional (double-buffering t) (depth t))
  ;;
  (labels ((gen-array (dimensions)
             (%make-gpu-array-t
              :texture +null-texture+
              :texture-type :gl-internal
              :dimensions dimensions
              :image-format :gl-internal)))
    (let ((result
           (%update-fbo-state
            (post-gl-init
             (pre-gl-init
              (make-uninitialized-fbo)
              :is-default t
              :color-arrays (cons (gen-array dimensions)
                                  (when double-buffering
                                    (list (gen-array dimensions))))
              :depth-array (when depth (gen-array dimensions)))
             :id 0))))
      (update-clear-mask result)
      (%with-cepl-context-slots (default-framebuffer) (cepl-context)
        (setf default-framebuffer result))
      result)))

;; %update-default-framebuffer-dimensions lives in viewport.lisp as
;; otherwise there would be a circular dependency

(defun+ %set-default-fbo-viewport (new-dimensions)
  (%with-cepl-context-slots (default-framebuffer) (cepl-context)
    (let ((fbo default-framebuffer))
      ;; - - -
      (loop :for a :across (%fbo-color-arrays fbo) :when a :do
         (setf (viewport-dimensions (att-viewport a)) new-dimensions)
         (cepl.textures::with-gpu-array-t (att-array a)
           (setf dimensions new-dimensions)))
      ;; - - -
      (let ((d (%fbo-depth-array fbo)))
        (setf (viewport-dimensions (att-viewport d)) new-dimensions)
        (cepl.textures::with-gpu-array-t (att-array d)
          (setf dimensions new-dimensions))))))

(defn attachment-viewport-allowing-t
    ((fbo fbo) (attachment-name attachment-name))
    viewport
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (case attachment-name
    ((t)
     (assert (fbo-empty-p fbo) ()
             'attachment-viewport-empty-fbo
             :fbo fbo
             :attachment attachment-name)
     (empty-fbo-params-viewport
      (%fbo-empty-params fbo)))
    (:d (att-viewport (%fbo-depth-array fbo)))
    (:s (att-viewport (%fbo-stencil-array fbo)))
    (otherwise
     (let ((arr (%fbo-color-arrays fbo)))
       (if (< attachment-name (length arr))
           (att-viewport (aref arr attachment-name))
           (error "No attachment at ~a" attachment-name))))))

(defn attachment-viewport ((fbo fbo)
                           (attachment-name attachment-name))
    viewport
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (case attachment-name
    ((t) (error 'attachment-viewport-empty-fbo
                :fbo fbo
                :attachment attachment-name))
    (:d (att-viewport (%fbo-depth-array fbo)))
    (:s (att-viewport (%fbo-stencil-array fbo)))
    (otherwise
     (let ((arr (%fbo-color-arrays fbo)))
       (if (< attachment-name (length arr))
           (att-viewport (aref arr attachment-name))
           (error "No attachment at ~a" attachment-name))))))

;;----------------------------------------------------------------------

(defn-inline color-attachment-enum ((attachment-num attachment-num))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (+ attachment-num #.(gl-enum :color-attachment0)))

;;----------------------------------------------------------------------

(defun+ %update-fbo-state (fbo)
  (update-clear-mask
   (update-draw-buffer-map
    fbo)))

(defun+ update-clear-mask (fbo)
  (setf (%fbo-clear-mask fbo)
        (cffi:foreign-bitfield-value
         '%gl::ClearBufferMask
         `(:color-buffer-bit
           ,@(when (att-array (%fbo-depth-array fbo))
               '(:depth-buffer-bit))
           ,@(when (att-array (%fbo-stencil-array fbo))
               '(:stencil-buffer-bit)))))
  fbo)

(defn-inline default-fbo-attachment-enum ((attachment-num (integer 0 3)))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (let ((vals #(#.(gl-enum :back-left)
                #.(gl-enum :front-left)
                #.(gl-enum :back-right)
                #.(gl-enum :front-right))))
    (declare (type (simple-array (signed-byte 32) (4))))
    (aref vals attachment-num)))

(defun+ update-draw-buffer-map (fbo)
  (let ((ptr (%fbo-draw-buffer-map fbo))
        (default-fbo (%fbo-is-default fbo)))
    (loop :for i :from 0 :for att :across (%fbo-color-arrays fbo) :do
       (let ((arr (att-array att)))
         (setf (mem-aref ptr '%gl:enum i)
               (if arr
                   (if default-fbo
                       (default-fbo-attachment-enum i)
                       (color-attachment-enum i))
                   :none)))))
  fbo)

;;----------------------------------------------------------------------

(defun+ %fbo-owns (fbo attachment-name)
  (case attachment-name
    (:d (att-owned-p (%fbo-depth-array fbo)))
    (:s (att-owned-p (%fbo-stencil-array fbo)))
    (otherwise (att-owned-p (aref (%fbo-color-arrays fbo) attachment-name)))))

(defun+ (setf %fbo-owns) (value fbo attachment-name)
  (case attachment-name
    (:d (setf (att-owned-p (%fbo-depth-array fbo)) value))
    (:s (setf (att-owned-p (%fbo-stencil-array fbo)) value))
    (otherwise
     (let ((arr (%fbo-color-arrays fbo))
           (index attachment-name))
       (ensure-fbo-array-size fbo (1+ index))
       (setf (att-owned-p (aref arr index)) value)))))

;;----------------------------------------------------------------------

(defn attachment-blending ((fbo fbo) (attachment-name attachment-name))
    (or null blending-params)
  (case attachment-name
    ((t) (error 'attachment-viewport-empty-fbo
                :fbo fbo
                :attachment attachment-name))
    (:d (let ((att (%fbo-depth-array fbo)))
          (or (att-bparams att) (att-blend att))))
    (:s nil)
    (otherwise
     (let ((arr (%fbo-color-arrays fbo))
           (index attachment-name))
       (if (< index (length arr))
           (let ((att (aref arr index)))
             (or (att-bparams att)
                 (att-blend att)))
           nil)))))

(defun+ (setf attachment-blending) (value fbo attachment-name)
  (assert (not (fbo-empty-p fbo)) ()
          'attachment-viewport-empty-fbo
          :fbo fbo
          :attachment attachment-name)
  (let ((att (case attachment-name
               (:d (%fbo-depth-array fbo))
               (:s (error "Cannot specifiy blending params for the stencil attachment"))
               (otherwise
                (let ((arr (%fbo-color-arrays fbo)))
                  (ensure-fbo-array-size fbo (1+ attachment-name))
                  (aref arr attachment-name))))))
    ;;
    (if (blending-params-p value)
        (setf (att-bparams att) value)
        (setf (att-bparams att) nil))
    (setf (att-blend att) (not (null value)))))

;;----------------------------------------------------------------------

(defn %attachment ((fbo fbo) (attachment-name attachment-name))
    (or null gpu-array-t)
  (case attachment-name
    (:d (att-array (%fbo-depth-array fbo)))
    (:s (att-array (%fbo-stencil-array fbo)))
    (otherwise
     (let ((arr (%fbo-color-arrays fbo)))
       (if (< attachment-name (length arr))
           (att-array (aref arr attachment-name))
           nil)))))

(defn (setf %attachment) ((value (or null gpu-array-t render-buffer))
                          (fbo fbo)
                          (attachment-name attachment-name))
    (or null gpu-array-t)
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  ;; we dont remove the empty-info if an empty fbo has something
  ;; attached to it. emptiness is tracked using attachment-count
  (flet ((incf-att-count ()
           (when (= (incf (%fbo-attachment-count fbo)) 1)
             (make-fbo-non-empty fbo)))
         (decf-att-count ()
           (when (= (decf (%fbo-attachment-count fbo)) 0)
             (make-existing-fbo-empty fbo (%fbo-depth-array fbo)))))
    (declare (inline incf-att-count decf-att-count))
    (case attachment-name
      (:d
       (let ((current (att-array (%fbo-depth-array fbo))))
         (if value
             (unless current
               (incf-att-count)
               (setf (att-viewport (%fbo-depth-array fbo))
                     (viewport-for-array value))
               (setf (att-array (%fbo-depth-array fbo)) value))
             (when current
               (decf-att-count)
               (setf (att-blend (%fbo-depth-array fbo)) nil
                     (att-bparams (%fbo-depth-array fbo)) nil
                     (att-viewport (%fbo-depth-array fbo)) nil
                     (att-array (%fbo-depth-array fbo)) nil)))))
      (:s
       (let ((current (att-array (%fbo-depth-array fbo))))
         (if value
             (unless current
               (incf-att-count))
             (when current
               (decf-att-count)))
         (setf (att-viewport (%fbo-stencil-array fbo))
               (viewport-for-array value))
         (setf (att-array (%fbo-stencil-array fbo)) value)))
      (otherwise
       (let ((arr (%fbo-color-arrays fbo)))
         (ensure-fbo-array-size fbo (1+ attachment-name))
         (let* ((att (aref arr attachment-name))
                (current (att-array att)))
           (if value
               (unless current
                 (incf-att-count))
               (when current
                 (decf-att-count)
                 (setf (att-blend att) nil)
                 (setf (att-bparams att) nil)))
           (setf (att-viewport att) (viewport-for-array value))
           (setf (att-array att) value)))))))


;; NOTE: The following seperation is to allow shadowing in compose-pipelines
(defn-inline attachment ((fbo fbo) (attachment-name attachment-name))
    (or null gpu-array-t)
  (assert (not (fbo-empty-p fbo)) ()
          "Empty FBOs have no attachments: ~a" fbo)
  (%attachment fbo attachment-name))

(defun+ (setf attachment) (value fbo attachment-name)
  (assert (not (%fbo-is-default fbo)) ()
          "Cannot modify attachments of default-framebuffer")
  (let ((current-value (%attachment fbo attachment-name))
        (initialized (initialized-p fbo)))
    ;; update the fbo
    (setf (%attachment fbo attachment-name) value)
    (when initialized
      ;; update gl
      (when current-value
        (fbo-detach fbo attachment-name))
      (when value
        (etypecase value
          (gpu-array-t
           (fbo-attach-array fbo value attachment-name))
          (render-buffer
           (fbo-attach-render-buffer fbo value attachment-name))))
      ;; update cached gl details
      (%update-fbo-state fbo)))
  ;;
  value)

;;----------------------------------------------------------------------

(defun+ attachment-tex (fbo attachment-name)
  (gpu-array-t-texture (%attachment fbo attachment-name)))

;;----------------------------------------------------------------------

;; The caching of the value is janky, use the cepl-context
(let ((max-draw-buffers -1))
  (declare (type (signed-byte 32) max-draw-buffers))
  (defn get-gl-attachment-keyword ((fbo fbo) (x attachment-name))
      (signed-byte 32)
    (declare (optimize (speed 3) (safety 1) (debug 1))
             (profile t))
    (let ((gl-ctx (when (cepl-context)
                    (cepl.context::%cepl-context-gl-context
                     (cepl-context)))))
      (unless (> max-draw-buffers 0)
        (when gl-ctx
          (setf max-draw-buffers (max-draw-buffers gl-ctx))))
      (case x
        (:d #.(gl-enum :depth-attachment))
        (:s (if (att-array (%fbo-depth-array fbo))
                #.(gl-enum  :depth-stencil-attachment)
                #.(gl-enum :stencil-attachment)))
        (otherwise
         (if (<= x max-draw-buffers)
             (color-attachment-enum x)
             (if gl-ctx
                 (error "Requested attachment ~s is outside the range of 0-~s supported by your current context"
                        x max-draw-buffers)
                 (color-attachment-enum x))))))))

(define-compiler-macro get-gl-attachment-keyword (&whole whole fbo x)
  (declare (ignore fbo))
  (if (numberp x)
      (color-attachment-enum x)
      (case x
        (:d #.(gl-enum :depth-attachment))
        (otherwise whole))))

;;--------------------------------------------------------------

(defun+ %fbo-draw-buffers (fbo)
  (let ((len (if (%fbo-is-default fbo)
                 1
                 (length (%fbo-color-arrays fbo)))))
    (%gl:draw-buffers len (%fbo-draw-buffer-map fbo))))

;;--------------------------------------------------------------

(defmacro with-fbo-slots (attachment-bindings expression &body body)
  (let ((expr (gensym "expression")))
    `(let* ((,expr ,expression)
            ,@(loop :for var-form :in attachment-bindings :collect
                 (if (listp var-form)
                     `(,(second var-form) (attachment ,expr
                                                      ,(kwd (first var-form))))
                     `(,var-form (attachment ,expr ,var-form)))))
       ,@body)))


;; (with-fbo-slots (c0 d)
;;     (with-fbo-bound (fbo)
;;       (map-g #'prog-1 stream :tex tx))
;;   (print c0)
;;   (print d))

;;--------------------------------------------------------------

(defun+ extract-matching-dimension-value (args)
  ;; The matching-dimensions flag is what tells cepl whether we
  ;; should throw an error if the dimensions of the args don't
  ;; match
  (let* ((p (position :matching-dimensions args))
         (v (if p (elt args (1+ p)) t)))
    (values v (if p (append (subseq args 0 p) (subseq args (+ 2 p)))
                  args))))

(defun+ make-fbo (&rest fuzzy-attach-args)
  (let* ((fbo-obj (pre-gl-init (make-uninitialized-fbo)))
         (arrays (fuzzy-args->arrays fbo-obj fuzzy-attach-args)))
    (cepl.context::if-gl-context
     (make-fbo-now %pre%)
     fbo-obj
     (append
      (remove-if-not #'holds-gl-object-ref-p
                     (cepl-utils:flatten fuzzy-attach-args))
      arrays))))

(defun+ empty-fbo-args-p (fuzzy-args)
  (or (null fuzzy-args)
      (equal fuzzy-args '(nil))
      (find-if (lambda (x) (and (listp x) (> (length x) 0) (null (first x))))
               fuzzy-args)))

(defun+ fuzzy-args->arrays (fbo-obj fuzzy-args)
  (multiple-value-bind (check-dimensions-matchp fuzzy-args)
      (extract-matching-dimension-value fuzzy-args)
    (cond
      ((empty-fbo-args-p fuzzy-args)
       (handle-empty-framebuffer fbo-obj fuzzy-args))
      ((and (texture-p (first fuzzy-args))
            (eq (texture-type (first fuzzy-args)) :texture-cube-map))
       (cube->fbo-arrays fbo-obj fuzzy-args))
      (fuzzy-args (apply #'fbo-gen-attach fbo-obj check-dimensions-matchp
                         fuzzy-args))
      (t (error "CEPL: FBOs must have at least one attachment")))))

(defun+ handle-empty-framebuffer (fbo-obj args)
  (assert (<= (length args) 1) ()
          'invalid-attachments-for-empty-fbo
          :args args)
  (assert-lambda-list
   (name &key dimensions layer-count samples fixed-sample-locations)
   (first args) 'invalid-attachments-for-empty-fbo :args (first args))
  (dbind (name
          &key
          dimensions
          (layer-count 0)
          (samples 0)
          (fixed-sample-locations nil))
      (or (first args) '(nil))
    (let ((dimensions (or (listify dimensions)
                          (viewport-dimensions (current-viewport)))))
      (assert (not (eq (first dimensions) 'quote)) ()
              'quote-symbol-found-in-fbo-dimensions
              :form (first args))
      (assert (and (null name)
                   dimensions
                   (<= (length dimensions) 2)
                   (every #'numberp dimensions))
              () 'invalid-attachments-for-empty-fbo :args args)
      (setf (%fbo-empty-params fbo-obj)
            (make-empty-fbo-params
             :dimensions dimensions
             :layer-count layer-count
             :samples samples
             :viewport (make-viewport dimensions)
             :fixed-sample-locations-p (not (null fixed-sample-locations))))
      ;; return nil as we have nothing that needs to be initialized
      ;; before the fbo can be initialized (no dependancies).
      nil)))

(defun+ cube->fbo-arrays (fbo-obj fuzzy-args)
  (let ((depth (listify
                (find-if (lambda (x)
                           (or (eq x :d) (and (listp x) (eq (first x) :d))))
                         fuzzy-args))))
    (dbind (cube-tex . rest) fuzzy-args
      (if (not (or (null rest) (and depth (= 1 (length rest)))))
          (error 'invalid-cube-fbo-args :args fuzzy-args)
          (fuzzy-args->arrays
           fbo-obj
           (append `((0 ,(texref cube-tex :cube-face 0))
                     (1 ,(texref cube-tex :cube-face 1))
                     (2 ,(texref cube-tex :cube-face 2))
                     (3 ,(texref cube-tex :cube-face 3))
                     (4 ,(texref cube-tex :cube-face 4))
                     (5 ,(texref cube-tex :cube-face 5)))
                   (when depth
                     (list
                      (if (member :dimensions depth)
                          depth
                          (append depth
                                  `(:dimensions
                                    ,(dimensions
                                      (texref cube-tex :cube-face 0)))))))))))))


(defun+ make-fbo-now (fbo-obj)
  (post-gl-init fbo-obj)
  (if (%fbo-empty-params fbo-obj)
      (initialize-as-empty-fbo fbo-obj)
      (initialize-regular-fbo fbo-obj))
  (%update-fbo-state fbo-obj)
  (check-framebuffer-status fbo-obj)
  fbo-obj)

(defun+ check-framebuffer-status (fbo)
  (%bind-fbo fbo :framebuffer)
  (release-unwind-protect
       (let ((status (%gl:check-framebuffer-status :framebuffer)))
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
                    (otherwise "An error occurred"))))
         status)
    (%unbind-fbo)))

;;----------------------------------------------------------------------

(defmethod free ((fbo fbo))
  (free-fbo fbo))

(defun free-fbo (fbo)
  (if (%fbo-is-default fbo)
      (error "Cannot free the default framebuffer")
      (free-user-fbo fbo)))

(defun free-user-fbo (fbo)
  (with-foreign-object (id :uint)
    (setf (mem-ref id :uint) (%fbo-id fbo))
    (cepl.context::forget-fbo (cepl-context) fbo)
    (setf (%fbo-id fbo) +null-gl-id+)
    (%gl:delete-framebuffers 1 id)
    nil))

;;----------------------------------------------------------------------

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

;;
(defn %bind-fbo ((fbo fbo) (target symbol)) fbo
  (with-cepl-context (ctx)
    (ecase target
      (:framebuffer (setf (fbo-bound ctx) fbo))
      (:read-framebuffer (setf (read-fbo-bound ctx) fbo))
      (:draw-framebuffer (setf (draw-fbo-bound ctx) fbo)))))

(defun+ %unbind-fbo ()
  (%with-cepl-context-slots (default-framebuffer) (cepl-context)
    (%bind-fbo default-framebuffer :framebuffer)))

(defmacro with-fbo-bound ((fbo &key (target :draw-framebuffer)
                               (with-viewport t) (attachment-for-size 0)
                               (with-blending t) (draw-buffers t))
                          &body body)
  (assert (member target +valid-fbo-targets+) (target)
          'fbo-target-not-valid-constant
          :target target)
  (labels ((%write-draw-buffer-pattern-call (fbo body)
             "This plays with the dispatch call from compose-pipelines
              The idea is that the dispatch func can preallocate one array
              with the draw-buffers patterns for ALL the passes in it, then
              we just upload from that one block of memory.
              All of this can be decided at compile time. It's gonna go fast!"
             (cond ((null draw-buffers)
                    `(progn
                       ,@body))
                   ((equal draw-buffers t)
                    `(progn
                       (%fbo-draw-buffers ,fbo)
                       ,(if with-blending
                             `(cepl.blending::%with-blending ,fbo t nil ,@body)
                             `(progn ,@body))))
                   ((listp draw-buffers)
                    (destructuring-bind (pointer len attachments) draw-buffers
                      (assert (numberp len))
                      `(progn
                         (%gl:draw-buffers ,len ,pointer)
                         (%with-blending ,fbo ,attachments nil ,@body)
                         (cffi:incf-pointer
                          ,pointer ,(* len (foreign-type-size '%gl:enum)))))))))
    (if (eq target :framebuffer)
        (alexandria:with-gensyms (old-read-fbo old-draw-fbo new-fbo ctx)
          `(with-cepl-context (,ctx)
             (let* ((,new-fbo ,fbo)
                    (,old-read-fbo (read-fbo-bound ,ctx))
                    (,old-draw-fbo (draw-fbo-bound ,ctx)))
               (,@(if with-viewport
                      `(with-fbo-viewport (,new-fbo ,attachment-for-size))
                      '(progn))
                  (release-unwind-protect
                      (progn
                        (setf (fbo-bound ,ctx) ,new-fbo)
                        ,(%write-draw-buffer-pattern-call new-fbo body))
                    (if (eq ,old-read-fbo ,old-draw-fbo)
                        (setf (fbo-bound ,ctx) ,old-read-fbo)
                        (progn
                          (setf (read-fbo-bound ,ctx) ,old-read-fbo)
                          (setf (draw-fbo-bound ,ctx) ,old-draw-fbo))))))))
        (alexandria:with-gensyms (old-fbo new-fbo ctx)
          `(with-cepl-context (,ctx)
             (let* ((,new-fbo ,fbo)
                    (,old-fbo ,(if (eq target :read-framebuffer)
                                   `(read-fbo-bound ,ctx)
                                   `(draw-fbo-bound ,ctx))))
               (,@(if with-viewport
                      `(with-fbo-viewport (,new-fbo ,attachment-for-size))
                      '(progn))
                  (release-unwind-protect
                      (progn
                        ,(if (eq target :read-framebuffer)
                             `(setf (read-fbo-bound ,ctx) ,new-fbo)
                             `(setf (draw-fbo-bound ,ctx) ,new-fbo))
                        ,(%write-draw-buffer-pattern-call new-fbo body))
                    ,(if (eq target :read-framebuffer)
                         `(setf (read-fbo-bound ,ctx) ,old-fbo)
                         `(setf (draw-fbo-bound ,ctx) ,old-fbo))))))))))


(defun+ fbo-gen-attach (fbo check-dimensions-matchp &rest args)
  "The are 4 kinds of valid argument:
   - keyword naming an attachment: This makes a new texture
     with size of (current-viewport) and attaches
   - (keyword texarray): attaches the tex-array
   - (keyword texture): attaches the root tex-array
   - (keyword some-type) any types that supports the generic dimensions function
                         creates a new texture at the framesize of the object
                         and attaches it to attachment named by keyword"
  (when check-dimensions-matchp
    (let ((dims (mapcar #'extract-dimension-from-make-fbo-pattern args)))
      (unless (every (lambda (x) (equal (first dims) x))
                     (rest dims))
        (error 'attachments-with-different-sizes
               :args args
               :sizes dims))))
  (mapcar (lambda (processed-pattern)
            (dbind (attachment-name tex-array owned-by-fbo) processed-pattern
              (setf (%fbo-owns fbo attachment-name) owned-by-fbo)
              (setf (attachment fbo attachment-name) tex-array)
              tex-array))
          (process-fbo-init-pattern args)))

(defun+ extract-dimension-from-make-fbo-pattern (pattern)
  (assert (or (listp pattern) (keywordp pattern) (numberp pattern)))
  (cond
    ;; simple keyword pattern to texture
    ((or (keywordp pattern) (numberp pattern))
     (viewport-dimensions (current-viewport)))
    ;; pattern with args for make-texture
    ((some (lambda (x) (member x +possible-texture-keys+)) pattern)
     (destructuring-bind
           (&key (dimensions (viewport-dimensions (current-viewport)))
                 &allow-other-keys)
         (rest pattern)
       dimensions))
    ;; use an existing gpu-array
    ((typep (second pattern) 'gpu-array-t)
     (gpu-array-dimensions (second pattern)))
    ;; use the first gpu-array in texture
    ((typep (second pattern) 'texture)
     (gpu-array-dimensions (texref (second pattern))))
    ;; take the dimensions from some object
    (t (dimensions (second pattern)))))

;;----------------------------------------------------------------------

(defun make-fbo-non-empty (fbo)
  (let ((params (%fbo-empty-params fbo)))
    (when params
      (setf (%empty-fbo-params-fbo params) nil)))
  fbo)

(defn empty-fbo-params ((fbo fbo)) empty-fbo-params
  (assert (fbo-empty-p fbo) ()
          "Cannot get the empty-fbo-params of a non-empty fbo: ~a"
          fbo)
  (%fbo-empty-params fbo))

(defn empty-fbo-params-dimensions ((params empty-fbo-params))
    list
  (%empty-fbo-params-dimensions params))

(defn (setf empty-fbo-params-dimensions)
    ((value list) (params empty-fbo-params))
    list
  (let ((fbo (%empty-fbo-params-fbo params)))
    (assert fbo () 'modify-non-empty-fbo-params)
    (assert (and (= (length value) 2)
                 (every (lambda (x) (typep x 'unsigned-byte))
                        value)))
    (with-fbo-bound (fbo :target :read-framebuffer
                         :with-viewport nil
                         :draw-buffers nil)
      (%gl:framebuffer-parameter-i
       :read-framebuffer
       :framebuffer-default-width (or (first value) 1))
      (%gl:framebuffer-parameter-i
       :read-framebuffer
       :framebuffer-default-height (or (second value) 1))
      (let ((vp (%empty-fbo-params-viewport params)))
        (setf (viewport-dimensions vp) value
              (%empty-fbo-params-dimensions params) value)))))

(defn empty-fbo-params-layer-count ((params empty-fbo-params))
    unsigned-byte
  (%empty-fbo-params-layer-count params))

(defn (setf empty-fbo-params-layer-count)
    ((value unsigned-byte) (params empty-fbo-params))
    unsigned-byte
  (let ((fbo (%empty-fbo-params-fbo params)))
    (assert fbo () 'modify-non-empty-fbo-params)
    (assert (and (integerp value) (>= value 0)))
    (with-fbo-bound (fbo :target :read-framebuffer
                         :with-viewport nil
                         :draw-buffers nil)
      (%gl:framebuffer-parameter-i
       :read-framebuffer
       :framebuffer-default-layers value)
      (setf (%empty-fbo-params-layer-count params) value))))

(defn empty-fbo-params-samples ((params empty-fbo-params))
    unsigned-byte
  (%empty-fbo-params-samples params))

(defn (setf empty-fbo-params-samples)
    ((value unsigned-byte) (params empty-fbo-params))
    unsigned-byte
  (let ((fbo (%empty-fbo-params-fbo params)))
    (assert fbo () 'modify-non-empty-fbo-params)
    (assert (and (integerp value) (>= value 0)))
    (with-fbo-bound (fbo :target :read-framebuffer
                         :with-viewport nil
                         :draw-buffers nil)
      (%gl:framebuffer-parameter-i
       :read-framebuffer
       :framebuffer-default-samples value)
      (setf (%empty-fbo-params-samples params) value))))

(defn empty-fbo-params-fixed-sample-locations-p ((params empty-fbo-params))
    boolean
  (%empty-fbo-params-fixed-sample-locations-p params))

(defn (setf empty-fbo-params-fixed-sample-locations-p)
    ((value boolean) (params empty-fbo-params))
    boolean
  (let ((fbo (%empty-fbo-params-fbo params)))
    (assert fbo () 'modify-non-empty-fbo-params)
    (with-fbo-bound (fbo :target :read-framebuffer
                         :with-viewport nil
                         :draw-buffers nil)
      (%gl:framebuffer-parameter-i
       :read-framebuffer
       :framebuffer-default-fixed-sample-locations (if value 1 0))
      (setf (%empty-fbo-params-fixed-sample-locations-p params)
            value))))

(defn empty-fbo-params-viewport ((params empty-fbo-params))
    viewport
  (%empty-fbo-params-viewport params))


(defun+ make-existing-fbo-empty (fbo last-attachment)
  (assert (>= (version-float (cepl-context)) 4.3) ()
          'gl-version-too-low-for-empty-fbos
          :version (version-float (cepl-context)))
  (let* ((att last-attachment)
         (arr (when att (att-array att)))
         (tex (when arr (gpu-array-t-texture arr)))
         (dimensions (if arr
                         (dimensions arr)
                         '(1 1)))
         (layer-count (if tex
                          (texture-layer-count tex)
                          0))
         (samples (if tex
                      (texture-samples tex)
                      0))
         (fsl-p (when tex
                  (texture-fixed-sample-locations-p tex)))
         (curr (%fbo-empty-params fbo))
         (params
          (if curr
              (progn
                (assert
                 (null (%empty-fbo-params-fbo curr)) ()
                 "CEPL BUG: empty fbo hadnt had info cleared correctly: ~s"
                 fbo)
                (setf (%empty-fbo-params-fbo curr) fbo)
                curr)
              (setf (%fbo-empty-params fbo)
                    (make-empty-fbo-params :fbo fbo)))))
    (with-fbo-bound (fbo :target :read-framebuffer
                         :with-viewport nil
                         :draw-buffers nil)
      ;; we rely on the state tracking to ensure we dont bind & unbind a tonne
      ;; of times during this block
      (setf (empty-fbo-params-dimensions params) dimensions
            (empty-fbo-params-layer-count params) layer-count
            (empty-fbo-params-samples params) samples
            (empty-fbo-params-fixed-sample-locations-p params) fsl-p))
    fbo))

(defun+ initialize-as-empty-fbo (fbo)
  (assert (>= (version-float (cepl-context)) 4.3) ()
          'gl-version-too-low-for-empty-fbos
          :version (version-float (cepl-context)))
  (with-fbo-bound (fbo :target :read-framebuffer
                         :with-viewport nil
                         :draw-buffers nil)
    ;; This is the one place where an empty info object can
    ;; exist without the params having been applied. By
    ;; reappplying them we force the gl change to be made.
    (let ((info (%fbo-empty-params fbo)))
      (setf (%empty-fbo-params-fbo info)
            fbo)
      (setf (empty-fbo-params-dimensions info)
            (empty-fbo-params-dimensions info))
      (setf (empty-fbo-params-layer-count info)
            (empty-fbo-params-layer-count info))
      (setf (empty-fbo-params-samples info)
            (empty-fbo-params-samples info))
      (setf (empty-fbo-params-fixed-sample-locations-p info)
            (empty-fbo-params-fixed-sample-locations-p info))
      fbo)))

(defun+ initialize-regular-fbo (fbo-obj)
  (loop :for a :across (%fbo-color-arrays fbo-obj)
     :for i :from 0 :do
     (when a (setf (attachment fbo-obj i) (att-array a))))
  (when (attachment fbo-obj :d)
    (setf (attachment fbo-obj :d) (attachment fbo-obj :d)))
  (when (attachment fbo-obj :s)
    (setf (attachment fbo-obj :s) (attachment fbo-obj :s))))

;;----------------------------------------------------------------------

(defun fbo-attach-render-buffer (fbo render-buffer attachment-name)
  (let ((attach-enum (get-gl-attachment-keyword fbo attachment-name)))
    (with-fbo-bound (fbo :target :read-framebuffer :with-viewport nil :draw-buffers nil)
      (%gl:framebuffer-renderbuffer :read-framebuffer
                                    attach-enum
                                    #.(gl-enum :renderbuffer)
                                    (%render-buffer-id render-buffer)))))

;;----------------------------------------------------------------------

;; Attaching Images

;; Remember that textures are a set of images. Textures can have mipmaps; thus,
;; each individual mipmap level can contain one or more images.

;; {TODO} Ensure image formats are color-renderable for color attachments
;;
(defun+ fbo-attach-array (fbo tex-array attachment-name)
  ;; To attach images to an FBO, we must first bind the FBO to the context.
  ;; target could be any of '(:framebuffer :read-framebuffer :draw-framebuffer)
  ;; but we just pick :read-framebuffer as in this case it makes no difference
  ;; to us
  (let ((attach-enum (get-gl-attachment-keyword fbo attachment-name)))
    (with-fbo-bound (fbo :target :read-framebuffer :with-viewport nil :draw-buffers nil)
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
      ;;----------------------------------------------------------------------
      (cepl.textures::with-gpu-array-t tex-array
        (assert (attachment-compatible attachment-name image-format t)
                ()  "attachment is not compatible with this array~%~a~%~a"
                image-format tex-array)
        (let ((tex-id (texture-id texture)))
          (case (gpu-array-t-texture-type tex-array)
            ;; A 1D texture contains 2D images that have the vertical height of 1.
            ;; Each individual image can be uniquely identified by a mipmap level.
            (:texture-1d
             (%gl:framebuffer-texture-1d :read-framebuffer attach-enum :texture-1d
                                         tex-id level-num))
            ;; A 2D texture contains 2D images. Each individual image can be
            ;; uniquely identified by a mipmap level.
            (:texture-2d
             (%gl:framebuffer-texture-2d :read-framebuffer attach-enum :texture-2d
                                         tex-id level-num))
            ;; The image in this texture (only one image. No mipmapping) is 2D.
            ;; Each pixel in these images contains multiple samples instead of
            ;; just one value.
            (:texture-2d-multisample
             (%gl:framebuffer-texture-2d :read-framebuffer attach-enum
                                         :texture-2d-multisample tex-id
                                         level-num))
            ;; Each mipmap level of a 3D texture is considered a set of 2D images,
            ;; with the number of these being the extent of the Z coordinate.
            ;; Each integer value for the depth of a 3D texture mipmap level is a
            ;; layer. So each image in a 3D texture is uniquely identified by a
            ;; layer and a mipmap level.
            ;; A single mipmap level of a 3D texture is a layered image, where the
            ;; number of layers is the depth of that particular mipmap level.
            (:texture-3d
             (%gl:framebuffer-texture-layer :read-framebuffer attach-enum tex-id
                                            level-num layer-num))
            ;; Each mipmap level of a 1D Array Textures contains a number of images,
            ;; equal to the count images in the array. While these images are
            ;; technically one-dimensional, they are promoted to 2D status for FBO
            ;; purposes in the same way as a non-array 1D texture: by using a height
            ;; of 1. Thus, each individual image is uniquely identified by a layer
            ;; (the array index) and a mipmap level.
            ;; A single mipmap level of a 1D Array Texture is a layered image, where
            ;; the number of layers is the array size.
            (:texture-1d-array
             (%gl:framebuffer-texture-layer :read-framebuffer attach-enum tex-id
                                            level-num layer-num))
            ;; 2D Array textures are much like 3D textures, except instead of the
            ;; number of Z slices, it is the array count. Each 2D image in an array
            ;; texture can be uniquely identified by a layer (the array index) and a
            ;; mipmap level. Unlike 3D textures, the array count doesn't change when
            ;; going down the mipmap hierarchy.
            ;; A single mipmap level of a 2D Array Texture is a layered image, where
            ;; the number of layers is the array size.
            (:texture-2d-array
             (%gl:framebuffer-texture-layer :read-framebuffer attach-enum tex-id
                                            level-num layer-num))
            ;; A Rectangle Texture has a single 2D image, and thus is identified by
            ;; mipmap level 0.
            (:texture-rectangle
             (%gl:framebuffer-texture-2d :read-framebuffer attach-enum :texture-2d
                                         tex-id 0))
            ;; When attaching a cubemap, you must use the Texture2D function, and
            ;; the textarget must be one of the 6 targets for cubemap binding.
            ;; Cubemaps contain 6 targets, each of which is a 2D image. Thus, each
            ;; image in a cubemap texture can be uniquely identified by a target
            ;; and a mipmap level.
            ;; Also, a mipmap level of a Cubemap Texture is a layered image. For
            ;; cubemaps, you get exactly 6 layers, one for each face. And the order
            ;; of the faces is the same as the order of the enumerators:
            ;; Layer number     Cubemap face
            ;; 0        GL_TEXTURE_CUBE_MAP_POSITIVE_X
            ;; 1        GL_TEXTURE_CUBE_MAP_NEGATIVE_X
            ;; 2        GL_TEXTURE_CUBE_MAP_POSITIVE_Y
            ;; 3        GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
            ;; 4        GL_TEXTURE_CUBE_MAP_POSITIVE_Z
            ;; 5        GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
            (:texture-cube-map
             (%gl:framebuffer-texture-2d :read-framebuffer attach-enum
                                         (elt '(:texture-cube-map-positive-x
                                                :texture-cube-map-negative-x
                                                :texture-cube-map-positive-y
                                                :texture-cube-map-negative-y
                                                :texture-cube-map-positive-z
                                                :texture-cube-map-negative-z)
                                              face-num)
                                         tex-id level-num))
            ;; Buffer Textures work like 1D texture, only they have a single image,
            ;; identified by mipmap level 0.
            (:texture-buffer (error "attaching to buffer textures has not been implemented yet"))
            ;; Cubemap array textures work like 2D array textures, only with 6 times
            ;; the number of images. Thus a 2D image in the array is identified by
            ;; the array layer (technically layer-face) and a mipmap level.
            ;; For cubemap arrays, the value that gl_Layer represents is the
            ;; layer-face index. Thus it is the face within a layer, ordered as
            ;; above. So if you want to render to the 3rd layer, +z face, you would
            ;; set gl_Layer to (2 * 6) + 4, or 16.
            (:texture-cube-map-array (error "attaching to cube-map-array textures has not been implemented yet"))))))))

(defun+ fbo-detach (fbo attachment-name)
  ;; The texture argument is the texture object name you want to attach from.
  ;; If you pass zero as texture, this has the effect of clearing the attachment
  ;; for this attachment, regardless of what kind of image was attached there.
  ;;
  ;; the attachment argument is the attachment-name
  ;;
  ;; {TODO} when using GL v4.5 use %gl:named-framebuffer-texture-layer,
  ;;        avoids binding
  (let ((enum (get-gl-attachment-keyword fbo attachment-name)))
    (with-fbo-bound (fbo :target :read-framebuffer
                         :with-viewport nil
                         :with-blending nil
                         :draw-buffers nil)
      (%gl:framebuffer-texture-layer :read-framebuffer enum 0 0 0))))

;;----------------------------------------------------------------------
;; Generating Textures from FBO Patterns

(define-const +valid-texture-subset+
    '(:dimensions :element-type :mipmap :immutable)
  :type list)

(defun+ process-fbo-init-pattern (pattern)
  (labels ((name (x) (if (listp x) (first x) x))
           (for-color (x) (numberp (name x)))
           (for-depth (x) (eq (name x) :d))
           (for-stencil (x) (eq (name x) :s)))

    (let* ((c (remove-if-not #'for-color pattern))
           (d (remove-if-not #'for-depth pattern))
           (s (remove-if-not #'for-stencil pattern))
           (o (set-difference pattern (append c d s) :test #'equal)))
      (assert (not o)
              () "CEPL: Invalid arguments passed to make-fbo: ~s" o)
      (assert (<= (length d) 1)
              () "FBO can only have 1 depth attachemnt: ~s" pattern)
      (assert (<= (length s) 1)
              () "FBO can only have 1 stencil attachemnt: ~s" pattern)

      (let ((d (first d))
            (s (first s)))
        (append (mapcar #'%gen-texture c)
                (cond ((and d s) (gen-depth-stencil-texture d s))
                      (d (list (%gen-texture d)))
                      (s (list (%gen-texture s)))))))))

(defun+ gen-depth-stencil-texture (depth stencil)
  (labels ((tweak (x y) `(,x ,@(rest y))))
    (cond
      ;;
      ;; If both are symbols then we can return the same tex for both
      ((and (symbolp depth) (symbolp stencil))
       (let ((tex (%gen-texture :ds)))
         (list (tweak :d tex) (tweak :s tex))))
      ;;
      ;; If only one is a list pattern then use that for both
      ((or (symbolp depth) (symbolp stencil))
       (let* ((pattern (if (listp depth) depth stencil))
              (elem-pos (position :element-type pattern))
              (existing (or (texture-p (second pattern))
                            (gpu-array-t-p (second pattern)))))
         (cond
           ;;
           ;; The pattern contains a valid ds texture
           (existing
            (let ((tex (%gen-texture (tweak :ds pattern))))
              (list (tweak :d tex) (tweak :s tex))))
           ;;
           ;; It was a pattern that will result in a valid ds texture
           ((and elem-pos (attachment-compatible
                           :ds (elt pattern (1+ elem-pos))))
            (let ((tex (%gen-texture (tweak :ds pattern))))
              (list (tweak :d tex) (tweak :s tex)))))))
      ;; Nope :(
      (t (if (and (eq (second depth) (second stencil))
                  (or (gpu-array-p (second depth))
                      (texture-p (second depth))))
             (let ((tex (%gen-texture (tweak :ds depth))))
               (list (tweak :d tex) (tweak :s tex)))
             (error "CEPL: Could not find a satifactory way to create a depth-stencil texture from:~%~a & ~a"
                    depth stencil))))))

(defun+ %gen-texture (pattern)
  ;;
  ;; (attachment-name tex-array owned-by-fbo)
  ;;
  (assert (or (listp pattern) (keywordp pattern)
              (numberp pattern)))
  (cond
    ;; simple keyword pattern to texture
    ((or (keywordp pattern)
         (numberp pattern))
     (list pattern
      (texref
            (make-texture
             nil :dimensions (viewport-dimensions (current-viewport))
             :element-type (%get-default-texture-format pattern)))
           t))
    ;; pattern with args for make-texture
    ((some (lambda (x) (member x +possible-texture-keys+)) pattern)
     (when (some (lambda (x) (and (member x +possible-texture-keys+)
                                  (not (member x +valid-texture-subset+))))
                 pattern)
       (error "Only the following args to make-texture are allowed inside a make-fbo ~s"
              +valid-texture-subset+))
     (destructuring-bind
           (&key (dimensions (viewport-dimensions (current-viewport)))
                 (element-type (%get-default-texture-format (first pattern)))
                 mipmap (immutable t))
         (rest pattern)
       (let ((element-type
              (if (image-formatp element-type)
                  element-type
                  (lisp-type->image-format element-type))))
         (assert (attachment-compatible (first pattern) element-type))
         (list (first pattern)
               (texref
                (make-texture nil
                              :dimensions dimensions
                              :element-type element-type
                              :mipmap mipmap
                              :immutable immutable))
               t))))
    ;; use an existing gpu-array
    ((typep (second pattern) 'gpu-array-t) (list (first pattern)
                                                 (second pattern)
                                                 t))
    ;; use the first gpu-array in texture
    ((typep (second pattern) 'texture) (list (first pattern)
                                             (texref (second pattern))
                                             t))
    ;; take the dimensions from some object
    (t (list (first pattern)
             (texref
              (make-texture nil :dimensions (dimensions (second pattern))
                            :element-type (%get-default-texture-format
                                           (first pattern))))
             t))))

(defun+ %get-default-texture-format (attachment)
  (cond ((numberp attachment) :rgba8)
        ((eq attachment :d) :depth-component24)
        ((eq attachment :s) :stencil-index8)
        ((eq attachment :ds) :depth24-stencil8)
        (t (error "No default texture format for attachment: ~s" attachment))))

(defun+ attachment-compatible (attachment-name image-format &optional for-bind)
  (case attachment-name
    ((:d :depth-attachment) (or (depth-formatp image-format)
                                (when for-bind
                                  (depth-stencil-formatp image-format))))
    ((:s :stencil-attachment) (or (stencil-formatp image-format)
                                  (when for-bind
                                    (depth-stencil-formatp image-format))))
    ((:ds :depth-stencil-attachment) (depth-stencil-formatp image-format))
    (otherwise (color-renderable-formatp image-format))))

(defn-inline clear (&optional (target fbo)) (values)
  (declare (profile t))
  (if target
      (clear-fbo target)
      (multiple-value-bind (read draw) (fbo-bound (cepl-context))
        (clear-fbo read)
        (unless (eq read draw)
          (clear-fbo draw))))
  (values))

(defn clear-fbo ((fbo fbo)) fbo
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (with-fbo-bound (fbo :target :draw-framebuffer
                       :with-blending nil
                       :with-viewport nil)
    (%gl:clear (%fbo-clear-mask fbo)))
  fbo)

(defun+ clear-attachment (attachment)
  (declare (ignore attachment))
  (error "CEPL: clear-attachment is not yet implemented"))

;;--------------------------------------------------------------

(defmethod print-object ((object fbo) stream)
  (if (initialized-p object)
      (let ((empty (fbo-empty-p object)))
        (format stream "#<~a~@[ COLOR-ATTRS ~a~]~@[ DEPTH-ATTR ~a~]~@[ STENCIL-ATTR ~a~]>"
                (if (%fbo-is-default object) "DEFAULT-FBO" "FBO")
                (loop :for i :from 0 :for j :in (fbo-color-arrays object)
                   :when j :collect i)
                (when (and (not empty) (attachment object :d)) t)
                (when (and (not empty) (attachment object :s)) t)))
      (format stream "#<FBO :UNINITIALIZED>")))

(defmethod print-object ((object empty-fbo-params) stream)
  (if (%empty-fbo-params-fbo object)
      (format
       stream
       "#<EMTPY-FBO-PARAMS :DIMENSIONS ~a :LAYERS ~a :SAMPLES ~a :FIXED-LOCATIONS ~a>"
       (empty-fbo-params-dimensions object)
       (empty-fbo-params-layer-count object)
       (empty-fbo-params-samples object)
       (empty-fbo-params-fixed-sample-locations-p object))
      (format
       stream
       "#<EMTPY-FBO-PARAMS :STATUS DETACHED>"))
  object)

;;----------------------------------------------------------------------
