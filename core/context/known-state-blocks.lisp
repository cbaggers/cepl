(in-package :cepl.context)

;;----------------------------------------------------------------------

;; (with-context-state-restored (:program t)
;;   foo)

(defmacro with-context-state-restored ((&key
                                        program
                                        stencil
                                        vao
                                        fbos-bound
                                        depth-test-function
                                        depth-mask
                                        depth-range
                                        depth-clamp
                                        clear-color
                                        cull-face
                                        front-face
                                        viewport
                                        ;;
                                        color-mask-indices
                                        tex-unit-ids
                                        buffer-targets
                                        scissor-viewport-indices
                                        ubo-indices
                                        ssbo-indices
                                        ;;
                                        pack-alignment
                                        unpack-alignment)
                                       &body body)
  (assert (member program '(t nil)))
  (assert (member stencil '(t nil)))
  (assert (member vao '(t nil)))
  (assert (member fbos-bound '(t nil)))
  (assert (member depth-test-function '(t nil)))
  (assert (member depth-mask '(t nil)))
  (assert (member depth-range '(t nil)))
  (assert (member depth-clamp '(t nil)))
  (assert (member clear-color '(t nil)))
  (assert (member cull-face '(t nil)))
  (assert (member front-face '(t nil)))
  (assert (member viewport '(t nil)))
  (assert (member unpack-alignment '(t nil)))
  (assert (member pack-alignment '(t nil)))
  (let ((color-mask-indices (uiop:ensure-list color-mask-indices))
        (tex-unit-ids (uiop:ensure-list tex-unit-ids))
        (buffer-targets (uiop:ensure-list buffer-targets))
        (scissor-viewport-indices (uiop:ensure-list scissor-viewport-indices))
        (ubo-indices (uiop:ensure-list ubo-indices))
        (ssbo-indices (uiop:ensure-list ssbo-indices)))
    (assert (not (eq (first color-mask-indices) 'quote)))
    (assert (not (eq (first tex-unit-ids) 'quote)))
    (assert (not (eq (first buffer-targets) 'quote)))
    (assert (not (eq (first scissor-viewport-indices) 'quote)))
    (assert (not (eq (first ubo-indices) 'quote)))
    (assert (not (eq (first ssbo-indices) 'quote)))
    (let ((ctx (gensym "ctx")))
      `(with-cepl-context (,ctx)
         (runtime-restore-block-checks ,ctx)
         (unwind-protect (progn ,@body)
           ,@(when program
               `((restore-program ,ctx)))
           ,@(when stencil
               `((restore-stencil ,ctx)))
           ,@(when vao
               `((restore-vao ,ctx)))
           ,@(when fbos-bound
               `((restore-fbo-bindings ,ctx)))
           ,@(when depth-test-function
               `((restore-depth-test-function ,ctx)))
           ,@(when depth-mask
               `((restore-depth-mask ,ctx)))
           ,@(when depth-range
               `((restore-depth-range ,ctx)))
           ,@(when depth-clamp
               `((restore-depth-clamp ,ctx)))
           ,@(when cull-face
               `((restore-cull-face ,ctx)))
           ,@(when front-face
               `((restore-front-face ,ctx)))
           ,@(when clear-color
               `((restore-clear-color ,ctx)))
           ,@(when viewport
               `((restore-viewport ,ctx)))
           ,@(when unpack-alignment
               `((restore-unpack-alignment ,ctx)))
           ,@(when pack-alignment
               `((restore-pack-alignment ,ctx)))
           ,@(when color-mask-indices
               `((restore-color-masks ,ctx)))
           ,@(when scissor-viewport-indices
               `((restore-current-scissor-viewports ,ctx)))
           ,@(when tex-unit-ids
               `((restore-array-of-bound-samplers ,ctx)))
           ,@(when buffer-targets
               `((restore-array-of-bound-gpu-buffers ,ctx)))
           ,@(when ubo-indices
               `((restore-array-of-ubo-bindings-buffer-ids ,ctx)))
           ,@(when ssbo-indices
               `((restore-array-of-ssbo-bindings-buffer-ids ,ctx))))))))

;;----------------------------------------------------------------------

(defn runtime-restore-block-checks ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  ;; I'd like to handle the stuff below but Im not sure how yet
  ;;
  ;; I dont think these 2 can be done:
  ;; - array-of-transform-feedback-bindings-buffer-ids
  ;; - array-of-bound-queries. If these are rebound then they are broken
  (%with-cepl-context-slots (current-tfs
                             current-blend-params)
      context
    (assert (not current-blend-params) ()
            'state-restore-limitation-blending)
    (assert (not current-tfs) ()
            'state-restore-limitation-transform-feedback))
  (values))

(defn-inline restore-program ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (cepl.pipelines::force-use-program context 0)
  (values))

(defn restore-stencil ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (cepl.stencil:current-stencil-params :front context)
        (cepl.stencil:current-stencil-params :front context))

  (setf (cepl.stencil:current-stencil-params :back context)
        (cepl.stencil:current-stencil-params :back context))

  (multiple-value-bind (front back)
      (cepl:stencil-mask :front-and-back context)
    (setf (cepl:stencil-mask :front context) front
          (cepl:stencil-mask :back context) back))
  (values))

;; vao-binding-id
(defn-inline restore-vao ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (force-bind-vao (vao-bound context) context)
  (values))

;; fbo-bindings
(defn-inline restore-fbo-bindings ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (%set-read-fbo-no-check context (read-fbo-bound context))
  (%set-draw-fbo-no-check context (draw-fbo-bound context))
  (values))

;; depth-func
(defn-inline restore-depth-test-function ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (force-depth-test-function (depth-test-function context) context)
  (values))

;; depth-mask
(defn-inline restore-depth-mask ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (depth-mask context) (depth-mask context))
  (values))

;; depth-range
(defn-inline restore-depth-range ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (depth-range-vec2 context) (depth-range-vec2 context))
  (values))

;; depth-clamp
(defn-inline restore-depth-clamp ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (depth-clamp context) (depth-clamp context))
  (values))

;; cull-face
(defn-inline restore-cull-face ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (cull-face context) (cull-face context))
  (values))

;; front-face
(defn-inline restore-front-face ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (front-face context) (front-face context))
  (values))

;; clear-color
(defn-inline restore-clear-color ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (clear-color context) (clear-color context))
  (values))

;; viewport
(defn-inline restore-viewport ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (%with-cepl-context-slots (current-viewport)
      context
    (cepl.viewports::%set-current-viewport context current-viewport)
    (values)))

(defn-inline restore-unpack-alignment ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (unpack-alignment t context) (unpack-alignment context))
  (values))

(defn-inline restore-pack-alignment ((context cepl-context)) (values)
  (declare (speed 3) (safety 1) (debug 1))
  (setf (pack-alignment t context) (pack-alignment context))
  (values))

(defn restore-color-masks ((context cepl-context)
                           (color-mask-indices list))
    (values)
  (loop
     :for index :in color-mask-indices
     :for mask := (color-mask index)
     :when mask
     :do (setf (color-mask index context) mask))
  (values))

(defn restore-current-scissor-viewports ((context cepl-context)
                                         (scissor-viewport-indices list))
    (values)
  (%with-cepl-context-slots (current-scissor-viewports)
      context
    (loop
       :for index :in scissor-viewport-indices
       :for viewport := (aref current-scissor-viewports index)
       :when viewport
       :do (cepl.scissor::force-scissor-viewport viewport index context)))
  (values))

(defn restore-array-of-bound-samplers ((context cepl-context)
                                       (tex-unit-ids list))
    (values)
  (%with-cepl-context-slots (array-of-bound-samplers)
      context
    (loop
       :for id :in tex-unit-ids
       :for sampler := (aref array-of-bound-samplers id)
       :when sampler
       :do (force-sampler-bound context sampler id)))
  (values))

(defn restore-array-of-bound-gpu-buffers ((context cepl-context)
                                          (buffer-targets list))
    (values)
  (%with-cepl-context-slots (array-of-bound-gpu-buffers)
      context
    (loop
       :for target :in buffer-targets
       :for index := (buffer-kind->cache-index target)
       :for buffer := (aref array-of-bound-gpu-buffers index)
       :when buffer
       :do (setf (gpu-buffer-bound context target) buffer)))
  (values))

(defn restore-array-of-ubo-bindings-buffer-ids ((context cepl-context)
                                                (ubo-indices list))
    (values)
  (%with-cepl-context-slots (array-of-ubo-binding-ranges
                             array-of-ubo-bindings-buffer-ids)
      context
    (loop
       :for ubo-binding-point :in ubo-indices
       :for ubo-id := (aref array-of-ubo-bindings-buffer-ids
                            ubo-binding-point)
       :do
         (unless (unknown-gl-id-p ubo-id)
           (let* ((range-index (the array-index (* ubo-id 2)))
                  (offset (aref array-of-ubo-binding-ranges range-index))
                  (size (aref array-of-ubo-binding-ranges
                              (+ range-index 1))))
             (%gl:bind-buffer-range #.(gl-enum :uniform-buffer)
                                    ubo-binding-point
                                    ubo-id
                                    offset
                                    size)))))
  (values))

(defn restore-array-of-ssbo-bindings-buffer-ids ((context cepl-context)
                                                 (ssbo-indices list))
    (values)
  (%with-cepl-context-slots (array-of-ssbo-binding-ranges
                             array-of-ssbo-bindings-buffer-ids)
      context
    (loop
       :for ssbo-binding-point :in ssbo-indices
       :for ssbo-id := (aref array-of-ssbo-bindings-buffer-ids
                             ssbo-binding-point)
       :do
         (unless (unknown-gl-id-p ssbo-id)
           (let* ((range-index (the array-index (* ssbo-id 2)))
                  (offset (aref array-of-ssbo-binding-ranges range-index))
                  (size (aref array-of-ssbo-binding-ranges
                              (+ range-index 1))))
             (%gl:bind-buffer-range #.(gl-enum :uniform-buffer)
                                    ssbo-binding-point
                                    ssbo-id
                                    offset
                                    size)))))
  (values))
