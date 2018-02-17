(in-package :cepl.context)

;;----------------------------------------------------------------------

(defmacro with-context-state-restored ((&key
                                        program
                                        stencil
                                        vao
                                        fbos-bound
                                        depth-test-function
                                        depth-mask
                                        depth-range
                                        depth-clamp)
                                       &body body)
  (assert (member program '(t nil)))
  (assert (member stencil '(t nil)))
  (assert (member vao '(t nil)))
  (assert (member fbos-bound '(t nil)))
  (assert (member depth-test-function '(t nil)))
  (assert (member depth-mask '(t nil)))
  (assert (member depth-range '(t nil)))
  (assert (member depth-clamp '(t nil)))
  (let ((ctx (gensym "ctx")))
    `(with-cepl-context (,ctx)
       (unwind-protect (progn ,@body)
         (restore-state ,ctx
                        ,program
                        ,stencil
                        ,vao
                        ,fbos-bound
                        ,depth-test-function
                        ,depth-mask
                        ,depth-range
                        ,depth-clamp
                        nil
                        nil
                        nil
                        nil)))))

(defn restore-state ((context cepl-context)
                     (program boolean)
                     (stencil boolean)
                     (vao boolean)
                     (fbos-bound boolean)
                     (depth-test-function boolean)
                     (depth-mask boolean)
                     (depth-range boolean)
                     (depth-clamp boolean)
                     (color-mask-indices list)
                     (tex-unit-ids list)
                     (buffer-targets list)
                     (scissor-viewport-indices list))
    null
  (%with-cepl-context-slots (current-tfs
                             current-viewport
                             array-of-bound-gpu-buffers
                             array-of-bound-queries
                             array-of-bound-samplers
                             current-scissor-viewports)
      context
    ;; current-tfs
    (assert (not current-tfs))

    ;; current-program
    (when program
      (cepl.pipelines::force-use-program context 0))

    (when stencil
      (setf (cepl.stencil:current-stencil-params :front context)
            (cepl.stencil:current-stencil-params :front context))

      (setf (cepl.stencil:current-stencil-params :back context)
            (cepl.stencil:current-stencil-params :back context)))

    ;; vao-binding-id
    (when vao
      (force-bind-vao (vao-bound context) context))

    ;; fbo-bindings
    (when fbos-bound
      (%set-read-fbo-no-check context (read-fbo-bound context))
      (%set-draw-fbo-no-check context (draw-fbo-bound context)))

    ;; depth-func
    (when depth-test-function
      (force-depth-test-function (depth-test-function context) context))

    ;; depth-mask
    (when depth-mask
      (setf (depth-mask context) (depth-mask context)))

    ;; depth-range
    (when depth-range
      (setf (depth-range-vec2 context) (depth-range-vec2 context)))

    ;; depth-clamp
    (when depth-clamp
      (setf (depth-clamp context) (depth-clamp context)))

    ;; cull-face
    (setf (cull-face context) (cull-face context))

    ;; front-face
    (setf (front-face context) (front-face context))

    ;; clear-color
    (setf (clear-color context) (clear-color context))

    ;; viewport
    (cepl.viewports::%set-current-viewport context current-viewport)

    ;; color-masks
    (loop :for index :in color-mask-indices
       :for mask := (color-mask index)
       :when mask
       :do (setf (color-mask index context) mask))

    ;; current-scissor-viewports
    (loop :for index :in scissor-viewport-indices
       :for viewport := (aref current-scissor-viewports index)
       :when viewport
       :do (cepl.scissor::force-scissor-viewport viewport index context))

    ;; array-of-bound-samplers
    (loop :for id :in tex-unit-ids
       :for sampler := (aref array-of-bound-samplers id)
       :when sampler
       :do (force-sampler-bound context sampler id))

    ;; array-of-bound-gpu-buffers
    (loop :for target :in buffer-targets
       :for index := (buffer-kind->cache-index target)
       :for buffer := (aref array-of-bound-gpu-buffers index)
       :when buffer
       :do (setf (gpu-buffer-bound context target) buffer))


    ;; current-stencil-mask-front
    ;; current-stencil-mask-back
    (multiple-value-bind (front back)
        (cepl:stencil-mask :front-and-back context)
      (setf (cepl:stencil-mask :front context) front
            (cepl:stencil-mask :back context) back))

    ;; need, but dont know how to handle yet
    ;;
    ;; Problem with these two are ranges
    ;; array-of-ubo-bindings-buffer-ids
    ;; array-of-ssbo-bindings-buffer-ids
    ;;
    ;; these are per fbo (usually)
    ;; current-blend-params
    ;;
    ;; I dont think this can be done
    ;; array-of-transform-feedback-bindings-buffer-ids
    ;; queries. If these are rebound then they are broken
    nil))

;;----------------------------------------------------------------------
