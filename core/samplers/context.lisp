(in-package :cepl.context)

;; We couldnt have this in cepl-context as it required a bunch of stuff
;; from textures & samplers

(defn set-sampler-bound ((ctx cepl-context)
                         (sampler sampler)
                         (tex-unit tex-unit))
    (values)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (inline %sampler-texture)
           (profile t))
  (%with-cepl-context-slots (array-of-bound-samplers) ctx
    (when (not (eq sampler (aref array-of-bound-samplers tex-unit)))
      (let ((texture (%sampler-texture sampler)))
        (active-texture-num tex-unit)
        (%gl:bind-texture (texture-cache-id texture) (texture-id texture))
        (if cepl.samplers::*samplers-available*
            (%gl:bind-sampler tex-unit (%sampler-id sampler))
            (cepl.textures::fallback-sampler-set sampler))
        (setf (aref array-of-bound-samplers tex-unit) sampler)))
    (values)))

(defn force-sampler-bound ((ctx cepl-context)
                           (sampler sampler)
                           (tex-unit tex-unit))
    (values)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (inline %sampler-texture)
           (profile t))
  (%with-cepl-context-slots (array-of-bound-samplers) ctx
    (let ((texture (%sampler-texture sampler)))
      (active-texture-num tex-unit)
      (%gl:bind-texture (texture-cache-id texture) (texture-id texture))
      (if cepl.samplers::*samplers-available*
          (%gl:bind-sampler tex-unit (%sampler-id sampler))
          (cepl.textures::fallback-sampler-set sampler))
      (setf (aref array-of-bound-samplers tex-unit) sampler))
    (values)))
