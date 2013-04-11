(in-package :classimp)

;;-------------------------------------------------------------
;; Helper Functions
;;------------------

(defun import-file-raw (filename &key processing-flags)
  (without-fp-traps
    (%ai:ai-import-file
     (namestring filename)
     (cffi:foreign-bitfield-value
      '%ai:ai-post-process-steps processing-flags))))

(defun release-raw-scene (scene-object)
  (%ai:ai-release-import scene-object))

(defun import-from-string (string extension &key (processing-flags t) (raw-times t))
  (temporary-file:with-open-temporary-file 
      (stream :direction :io
              :template (format nil "%.~a" extension))
    (format stream string)
    (finish-output stream)
    (ai:import-into-lisp (pathname stream) 
                         :processing-flags processing-flags
                         :raw-times raw-times)))

(export '(import-from-string release-raw-scene import-file-raw))

;; ;;-------------------------------------------------------------
;; ;; C-Structs
;; ;;------------------

;; (cffi:defcstruct ai-string
;;   (length size-t)
;;   (data :char :count 1024))

;; (cffi:defcstruct ai-string32
;;   (length :uint32)
;;   (data :char :count 1024))

;; (cffi:defcstruct ai-camera
;;   (m-name ai-string)
;;   (m-position ai-vector-3d)
;;   (m-up ai-vector-3d)
;;   (m-look-at ai-vector-3d)
;;   (m-horizontal-fov :float)
;;   (m-clip-plane-near :float)
;;   (m-clip-plane-far :float)
;;   (m-aspect :float))

;; (cffi:defcstruct ai-vector-key
;;   (m-time :double)
;;   (m-value ai-vector-3d))

;; (cffi:defcstruct ai-quat-key
;;   (m-time :double)
;;   (m-value ai-quaternion))

;; (cffi:defcstruct ai-node-anim
;;   (m-node-name ai-string)
;;   (m-num-position-keys :unsigned-int)
;;   (m-position-keys :pointer)
;;   (m-num-rotation-keys :unsigned-int)
;;   (m-rotation-keys :pointer)
;;   (m-num-scaling-keys :unsigned-int)
;;   (m-scaling-keys :pointer)
;;   (m-pre-state ai-anim-behaviour)
;;   (m-post-state ai-anim-behaviour))

;; (cffi:defcstruct ai-animation
;;   (m-name ai-string)
;;   (m-duration :double)
;;   (m-ticks-per-second :double)
;;   (m-num-channels :unsigned-int)
;;   (m-channels :pointer))

;; (cffi:defcstruct ai-material-property
;;   (m-key ai-string)
;;   (m-semantic ai-texture-type)
;;   (m-index :unsigned-int)
;;   (m-data-length :unsigned-int)
;;   (m-type ai-property-type-info)
;;   (m-data (:pointer :char)))

;; (cffi:defcstruct ai-material
;;   (m-properties :pointer)
;;   (m-num-properties :unsigned-int)
;;   (m-num-allocated :unsigned-int))

;; (cffi:defcstruct ai-node
;;   (m-name ai-string)
;;   (m-transformation ai-matrix-4x-4)
;;   (m-parent :pointer)
;;   (m-num-children :unsigned-int)
;;   (m-children :pointer)
;;   (m-num-meshes :unsigned-int)
;;   (m-meshes (:pointer :unsigned-int)))

;; (cffi:defcstruct ai-face
;;   (m-num-indices :unsigned-int)
;;   (m-indices (:pointer :unsigned-int)))

;; (cffi:defcstruct ai-vertex-weight
;;   (m-vertex-id :unsigned-int)
;;   (m-weight :float))

;; (cffi:defcstruct ai-bone
;;   (m-name ai-string)
;;   (m-num-weights :unsigned-int)
;;   (m-weights :pointer)
;;   (m-offset-matrix ai-matrix-4x-4))

;; ;; fixme: probably should grovel these, aiMesh.h says they shouldn't change
;; ;; though, so ignoring for now...
;; ;; ... they changed anyway :/
;; (cl:eval-when (:compile-toplevel :load-toplevel :execute)
;;   (cl:defconstant +ai-max-number-of-color-sets+ 8)
;;   (cl:defconstant +ai-max-number-of-texturecoords+ 8))

;; ;; 3.0 unused
;; #++
;; (cffi:defcstruct ai-anim-mesh
;;   (m-vertices :pointer)
;;   (m-normals :pointer)
;;   (m-tangents :pointer)
;;   (m-bitangents :pointer)
;;   (m-colors :pointer :count #.+ai-max-number-of-color-sets+)
;;   (m-texture-coords :pointer :count #.+ai-max-number-of-texturecoords+)
;;   (m-num-vertices :unsigned-int))

;; (cffi:defcstruct ai-mesh
;;   (m-primitive-types :unsigned-int)
;;   (m-num-vertices :unsigned-int)
;;   (m-num-faces :unsigned-int)
;;   (m-vertices :pointer)
;;   (m-normals :pointer)
;;   (m-tangents :pointer)
;;   (m-bitangents :pointer)
;;   (m-colors :pointer :count #.+ai-max-number-of-color-sets+)
;;   (m-texture-coords :pointer :count #.+ai-max-number-of-texturecoords+)
;;   (m-num-uv-components :unsigned-int :count #.+ai-max-number-of-texturecoords+)
;;   (m-faces :pointer)
;;   (m-num-bones :unsigned-int)
;;   (m-bones :pointer)
;;   (m-material-index :unsigned-int)
;;   (m-name ai-string) ;; 3.0
;;   (m-num-anim-meshes :unsigned-int) ;; 3.0, unused
;;   (m-anim-meshes :pointer)) ;; 3.0 unused

;; (cffi:defcstruct ai-texel
;;   (b :unsigned-char)
;;   (g :unsigned-char)
;;   (r :unsigned-char)
;;   (a :unsigned-char))

;; (cffi:defcstruct ai-texture
;;   (m-width :unsigned-int)
;;   (m-height :unsigned-int)
;;   (ach-format-hint :char :count 4)
;;   (pc-data :pointer))

;; (cffi:defcstruct ai-light
;;   (m-name ai-string)
;;   (m-type ai-light-source-type)
;;   (m-position ai-vector-3d)
;;   (m-direction ai-vector-3d)
;;   (m-attenuation-constant :float)
;;   (m-attenuation-linear :float)
;;   (m-attenuation-quadratic :float)
;;   (m-color-diffuse ai-color-3d)
;;   (m-color-specular ai-color-3d)
;;   (m-color-ambient ai-color-3d)
;;   (m-angle-inner-cone :float)
;;   (m-angle-outer-cone :float))


;; ;; (cgl:defglstruct ai-scene
;; ;;   (m-flags :unsigned-int)
;; ;;   (m-root-node :pointer)
;; ;;   (m-num-meshes :unsigned-int)
;; ;;   (m-meshes :pointer)
;; ;;   (m-num-materials :unsigned-int)
;; ;;   (m-materials :pointer)
;; ;;   (m-num-animations :unsigned-int)
;; ;;   (m-animations :pointer)
;; ;;   (m-num-textures :unsigned-int)
;; ;;   (m-textures :pointer)
;; ;;   (m-num-lights :unsigned-int)
;; ;;   (m-lights :pointer)
;; ;;   (m-num-cameras :unsigned-int)
;; ;;   (m-cameras :pointer)
;; ;;   (m-private :pointer))

;; (cffi:defcstruct ai-scene
;;   (m-flags :unsigned-int)
;;   (m-root-node :pointer)
;;   (m-num-meshes :unsigned-int)
;;   (m-meshes :pointer)
;;   (m-num-materials :unsigned-int)
;;   (m-materials :pointer)
;;   (m-num-animations :unsigned-int)
;;   (m-animations :pointer)
;;   (m-num-textures :unsigned-int)
;;   (m-textures :pointer)
;;   (m-num-lights :unsigned-int)
;;   (m-lights :pointer)
;;   (m-num-cameras :unsigned-int)
;;   (m-cameras :pointer)
;;   (m-private :pointer)) ;; 3.0

;; #+nil
;; (cffi:defbitfield ai-post-process-steps
;;   (:ai-process-calc-tangent-space #x1)
;;   (:ai-process-join-identical-vertices #x2)
;;   (:ai-process-make-left-handed #x4)
;;   (:ai-process-triangulate #x8)
;;   (:ai-process-remove-component #x10)
;;   (:ai-process-gen-normals #x20)
;;   (:ai-process-gen-smooth-normals #x40)
;;   (:ai-process-split-large-meshes #x80)
;;   (:ai-process-pre-transform-vertices #x100)
;;   (:ai-process-limit-bone-weights #x200)
;;   (:ai-process-validate-data-structure #x400)
;;   (:ai-process-improve-cache-locality #x800)
;;   (:ai-process-remove-redundant-materials #x1000)
;;   (:ai-process-fix-infacing-normals #x2000)
;;   (:ai-process-sort-by-p-type #x8000)
;;   (:ai-process-find-degenerates #x10000)
;;   (:ai-process-find-invalid-data #x20000)
;;   (:ai-process-gen-uv-coords #x40000)
;;   (:ai-process-transform-uv-coords #x80000)
;;   (:ai-process-find-instances #x100000)
;;   (:ai-process-optimize-meshes #x200000)
;;   (:ai-process-optimize-graph #x400000)
;;   (:ai-process-flip-u-vs #x800000)
;;   (:ai-process-flip-winding-order #x1000000)

;;   (:ai-process-convert-to-left-handed #x1800004)
;;   (:ai-process-preset-target-realtime-fast #x4802B)
;;   (:ai-process-preset-target-realtime-quality #x79ACB)

;;   )

;; (cffi:defbitfield ai-post-process-steps
;;   (:ai-process-calc-tangent-space #x1)
;;   (:ai-process-join-identical-vertices #x2)
;;   (:ai-process-make-left-handed #x4)
;;   (:ai-process-triangulate #x8)
;;   (:ai-process-remove-component #x10)
;;   (:ai-process-gen-normals #x20)
;;   (:ai-process-gen-smooth-normals #x40)
;;   (:ai-process-split-large-meshes #x80)
;;   (:ai-process-pre-transform-vertices #x100)
;;   (:ai-process-limit-bone-weights #x200)
;;   (:ai-process-validate-data-structure #x400)
;;   (:ai-process-improve-cache-locality #x800)
;;   (:ai-process-remove-redundant-materials #x1000)
;;   (:ai-process-fix-infacing-normals #x2000)
;;   (:ai-process-sort-by-p-type #x8000)
;;   (:ai-process-find-degenerates #x10000)
;;   (:ai-process-find-invalid-data #x20000)
;;   (:ai-process-gen-uv-coords #x40000)
;;   (:ai-process-transform-uv-coords #x80000)
;;   (:ai-process-find-instances #x100000)
;;   (:ai-process-optimize-meshes #x200000)
;;   (:ai-process-optimize-graph #x400000)
;;   (:ai-process-flip-u-vs #x800000)
;;   (:ai-process-flip-winding-order #x1000000)

;;   (:ai-process-convert-to-left-handed #x1800004
;;                                       #-(and) (:ai-process-make-left-handed
;;                                                :ai-process-flip-u-vs
;;                                                :ai-process-flip-winding-order))
;;   (:ai-process-preset-target-realtime-fast #x4802B
;;                                            #- (and)
;;                                            (:ai-process-calc-tangent-space
;;                                             :ai-process-gen-normals
;;                                             :ai-process-join-identical-vertices
;;                                             :ai-process-triangulate
;;                                             :ai-process-gen-uv-coords
;;                                             :ai-process-sort-by-p-type))
;;   (:ai-process-preset-target-realtime-quality
;;    #x79ACB
;;    #- (and)
;;    (:ai-process-calc-tangent-space
;;     :ai-process-gen-smooth-normals
;;     :ai-process-join-identical-vertices
;;     :ai-process-improve-cache-locality
;;     :ai-process-limit-bone-weights
;;     :ai-process-remove-redundant-materials
;;     :ai-process-split-large-meshes
;;     :ai-process-triangulate
;;     :ai-process-gen-uv-coords
;;     :ai-process-sort-by-p-type
;;     :ai-process-find-degenerates
;;     :ai-process-find-invalid-data))
;;   (:ai-process-preset-target-realtime-max-quality
;;    #x379ECB
;;    #- (and)
;;    (:ai-process-preset-target-realtime-quality
;;     :ai-process-find-instances
;;     :ai-process-validate-data-structure
;;     :ai-process-optimize-meshes)))

;; (cffi:defcstruct ai-uv-transform
;;   (m-translation ai-vector-2d)
;;   (m-scaling ai-vector-2d)
;;   (m-rotation :float))

;; (cffi:defcstruct ai-memory-info
;;   (textures :unsigned-int)
;;   (materials :unsigned-int)
;;   (meshes :unsigned-int)
;;   (nodes :unsigned-int)
;;   (animations :unsigned-int)
;;   (cameras :unsigned-int)
;;   (lights :unsigned-int)
;;   (total :unsigned-int))

;; (cffi:defcstruct ai-log-stream
;;   (callback ai-log-stream-callback)
;;   (user (:pointer :char)))

;; (cffi:defbitfield (ai-default-log-stream :int)
;;   (:ai-default-log-stream-file 1)
;;   (:ai-default-log-stream-stdout 2)
;;   (:ai-default-log-stream-stderr 4)
;;   (:ai-default-log-stream-debugger 8))

;; (cffi:defcstruct ai-file-io
;;   (open-proc ai-file-open-proc)
;;   (close-proc ai-file-close-proc)
;;   (user-data ai-user-data))

;; (cffi:defcstruct ai-file
;;   (read-proc ai-file-read-proc)
;;   (write-proc ai-file-write-proc)
;;   (tell-proc ai-file-tell-proc)
;;   (file-size-proc ai-file-tell-proc)
;;   (seek-proc ai-file-seek)
;;   (flush-proc ai-file-flush-proc)
;;   (user-data ai-user-data))

;; (cffi:defbitfield (ai-texture-flags :int)
;;   (:ai-texture-flags-invert 1)
;;   (:ai-texture-flags-use-alpha 2)
;;   (:ai-texture-flags-ignore-alpha 4))

;; (cffi:defcstruct ai-plane
;;   (a :float)
;;   (b :float)
;;   (c :float)
;;   (d :float))

;; (cffi:defcstruct ai-ray
;;   (pos ai-vector-3d)
;;   (dir ai-vector-3d))
