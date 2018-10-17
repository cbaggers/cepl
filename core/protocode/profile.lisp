(defmacro profile-all-cepl ()
  (let ((packages '(:cl
                    :cepl-utils
                    :cepl.errors
                    :cepl.host
                    :cepl.lifecycle
                    :cepl.measurements
                    :%cepl.types
                    :cepl.memory
                    :cepl.types.foreign
                    :cepl.types
                    :cepl.types.predefined
                    :cepl.internals
                    ;; :cepl.render-state
                    :cepl.context
                    :cepl.viewports
                    :cepl.image-formats
                    :cepl.pixel-formats
                    :cepl.c-arrays
                    :cepl.gpu-buffers
                    :cepl.gpu-arrays.buffer-backed
                    :cepl.vaos
                    :cepl.streams
                    :cepl.ubos
                    :cepl.textures
                    :cepl.gpu-arrays.texture-backed
                    :cepl.gpu-arrays
                    :cepl.samplers
                    :cepl.fbos
                    :cepl.blending
                    :cepl.pipelines
                    :cffi
                    :%gl)))
    (when (find-package :livesupport)
      (pushnew :livesupport packages))
    `(sb-profile:profile
      ,@(loop :for p :in packages :append
           (let ((pkg (find-package p)))
             (loop :for s :being :the symbol :in p
                :when (and (eq pkg (symbol-package s)) (fboundp s))
                :collect s))))))

(defmacro unprofile-all-cepl ()
  (let ((packages '(:cl
                    :cepl-utils
                    :cepl.errors
                    :cepl.host
                    :cepl.lifecycle
                    :cepl.measurements
                    :%cepl.types
                    :cepl.memory
                    :cepl.types.foreign
                    :cepl.types
                    :cepl.types.predefined
                    :cepl.internals
                    ;; :cepl.render-state
                    :cepl.context
                    :cepl.viewports
                    :cepl.image-formats
                    :cepl.pixel-formats
                    :cepl.c-arrays
                    :cepl.gpu-buffers
                    :cepl.gpu-arrays.buffer-backed
                    :cepl.vaos
                    :cepl.streams
                    :cepl.ubos
                    :cepl.textures
                    :cepl.gpu-arrays.texture-backed
                    :cepl.gpu-arrays
                    :cepl.samplers
                    :cepl.fbos
                    :cepl.blending
                    :cepl.pipelines
                    :cffi
                    :%gl)))
    (when (find-package :livesupport)
      (pushnew :livesupport packages))
    `(progn
       (sb-profile:unprofile
        ,@(loop :for p :in packages :append
             (let ((pkg (find-package p)))
               (loop :for s :being :the symbol :in p
                  :when (and (eq pkg (symbol-package s)) (fboundp s))
                  :collect s))))
       (sb-profile:reset))))
