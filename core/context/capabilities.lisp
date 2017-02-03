(in-package :cepl.context)

;;------------------------------------------------------------
;; Clear Color

(defun clear-color (cepl-context)
  (with-slots (clear-color) cepl-context
    clear-color))

(defun (setf clear-color) (vec4-color cepl-context)
  (assert (typep vec4-color 'rtg-math.types:vec4))
  (with-slots (clear-color) cepl-context
    (gl:clear-color (v:x vec4-color) (v:y vec4-color)
                    (v:z vec4-color) (v:w vec4-color))
    (setf clear-color vec4-color)))


;;------------------------------------------------------------
;; Cull Face

(defun cull-face (cepl-context)
  (with-slots (cull-face) cepl-context
    cull-face))

(defun (setf cull-face) (face cepl-context)
  (assert (member face '(nil :front :back :front-and-back)))
  (with-slots (cull-face) cepl-context
    (if face
        (progn
          (gl:enable :cull-face)
          (gl:cull-face face))
        (gl:disable :cull-face))
    (setf cull-face face)))

;;------------------------------------------------------------
;; Front Face

(defun front-face (cepl-context)
  (with-slots (front-face) cepl-context
    front-face))

(defun (setf front-face) (winding-direction cepl-context)
  (assert (or (eq winding-direction :ccw)
              (eq winding-direction :cw)))
  (with-slots (front-face) cepl-context
    (gl:front-face winding-direction)
    (setf front-face winding-direction)))

;;------------------------------------------------------------
;; Depth Range

(defun depth-range-vec2 (cepl-context)
  (with-slots (depth-range) cepl-context
    depth-range))

(defun (setf depth-range-vec2) (vec2-range cepl-context)
  (assert (typep vec2-range 'rtg-math.types:vec2))
  (with-slots (depth-range) cepl-context
    (gl:depth-range (v:x vec2-range) (v:y vec2-range))
    (setf depth-range vec2-range)))

;;------------------------------------------------------------
;; Depth Clamp

(defun depth-clamp (cepl-context)
  (with-slots (depth-clamp) cepl-context
    depth-clamp))

(defun (setf depth-clamp) (value cepl-context)
  (with-slots (depth-clamp) cepl-context
    (let ((value (not (null value))))
      (if value
          (gl:enable :depth-clamp)
          (gl:disable :depth-clamp))
      (setf depth-clamp value))))

;;------------------------------------------------------------
;; Depth Mask

(defun depth-mask (cepl-context)
  (with-slots (depth-mask) cepl-context
    depth-mask))

(defun (setf depth-mask) (value cepl-context)
  (with-slots (depth-mask) cepl-context
    (let ((value (not (null value))))
      (if value
          (gl:depth-mask :true)
          (gl:depth-mask :false))
      (setf depth-mask value))))

;;------------------------------------------------------------
;; Depth Test

(defun depth-test-function (cepl-context)
  (with-slots (depth-func) cepl-context
    depth-func))

(defun (setf depth-test-function) (function cepl-context)
  (with-slots (depth-func) cepl-context
    (if function
        (progn
          (gl:enable :depth-test)
          (cond
            ;;
            ((eq function depth-func) depth-func)
            ((or (eq function ':never) (eq function #'never)) nil
             (gl:depth-func :never)
             (setf depth-func #'never))
            ;;
            ((or (eq function ':less) (eq function #'<)) nil
             (gl:depth-func :less)
             (setf depth-func #'<))
            ;;
            ((or (eq function ':equal) (eq function #'=)) nil
             (gl:depth-func :equal)
             (setf depth-func #'=))
            ;;
            ((or (eq function ':lequal) (eq function #'<=)) nil
             (gl:depth-func :lequal)
             (setf depth-func #'<=))
            ;;
            ((or (eq function ':greater) (eq function #'>)) nil
             (gl:depth-func :greater)
             (setf depth-func #'>))
            ;;
            ((or (eq function ':notequal) (eq function #'/=)) nil
             (gl:depth-func :notequal)
             (setf depth-func #'/=))
            ;;
            ((or (eq function ':gequal) (eq function #'>=)) nil
             (gl:depth-func :gequal)
             (setf depth-func #'>=))
            ;;
            ((or (eq function ':always) (eq function #'aways)) nil
             (gl:depth-func :always)
             (setf depth-func #'always))
            (t (error "CEPL: Invalid function for depth-test-function: ~a"
                      function))))
        ;; function was nil
        (progn
          (gl:disable :depth-test)
          (setf depth-func nil)))))

(defun never (incoming-depth stored-depth)
  "Never passes"
  (declare (ignore incoming-depth stored-depth))
  nil)

(defun always (incoming-depth stored-depth)
  "Always passes"
  (declare (ignore incoming-depth stored-depth))
  t)

;;------------------------------------------------------------
;; Todo

;; :BLEND If enabled, blend the computed fragment color values with the
;;     values in the color buffers. See glBlendFunc. Sets the blend
;;     enable/disable flag for all color buffers.

;; :CLIP-DISTANCEi If enabled, clip geometry against user-defined half
;;     space i.

;; :COLOR-LOGIC-OP If enabled, apply the currently selected logical
;;     operation to the computed fragment color and color buffer
;;     values. See glLogicOp.

;; :DEBUG-OUTPUT If enabled, debug messages are produced by a debug
;;     context. When disabled, the debug message log is silenced. Note
;;     that in a non-debug context, very few, if any messages might be
;;     produced, even when :DEBUG-OUTPUT is enabled.

;; :DEBUG-OUTPUT-SYNCHRONOUS If enabled, debug messages are produced
;;     synchronously by a debug context. If disabled, debug messages may
;;     be produced asynchronously. In particular, they may be delayed
;;     relative to the execution of GL commands, and the debug callback
;;     function may be called from a thread other than that in which the
;;     commands are executed. See glDebugMessageCallback​.

;; :DITHER If enabled, dither color components or indices before they are
;;     written to the color buffer.

;; :FRAMEBUFFER-SRGB If enabled and the value of
;;     :FRAMEBUFFER-ATTACHMENT-COLOR-ENCODING for the framebuffer
;;     attachment corresponding to the destination buffer is :SRGB, the
;;     R, G, and B destination color values (after conversion from
;;     fixed-point to floating-point) are considered to be encoded for
;;     the sRGB color space and hence are linearized prior to their use
;;     in blending.

;; :LINE-SMOOTH If enabled, draw lines with correct filtering. Otherwise,
;;     draw aliased lines. See glLineWidth.

;; :MULTISAMPLE If enabled, use multiple fragment samples in computing
;;     the final color of a pixel. See glSampleCoverage.

;; :POLYGON-OFFSET-FILL If enabled, and if the polygon is rendered in
;;     :FILL mode, an offset is added to depth values of a polygon's
;;     fragments before the depth comparison is performed. See
;;     glPolygonOffset.

;; :POLYGON-OFFSET-LINE If enabled, and if the polygon is rendered in
;;     :LINE mode, an offset is added to depth values of a polygon's
;;     fragments before the depth comparison is performed. See
;;     glPolygonOffset.

;; :POLYGON-OFFSET-POINT If enabled, an offset is added to depth values
;;     of a polygon's fragments before the depth comparison is performed,
;;     if the polygon is rendered in :POINT mode. See glPolygonOffset.

;; :POLYGON-SMOOTH If enabled, draw polygons with proper
;;     filtering. Otherwise, draw aliased polygons. For correct
;;     antialiased polygons, an alpha buffer is needed and the polygons
;;     must be sorted front to back.

;; :PRIMITIVE-RESTART Enables primitive restarting. If enabled, any one
;;     of the draw commands which transfers a set of generic attribute
;;     array elements to the GL will restart the primitive when the index
;;     of the vertex is equal to the primitive restart index. See
;;     glPrimitiveRestartIndex​.

;; :PRIMITIVE-RESTART-FIXED-INDEX Enables primitive restarting with a
;;     fixed index. If enabled, any one of the draw commands which
;;     transfers a set of generic attribute array elements to the GL will
;;     restart the primitive when the index of the vertex is equal to the
;;     fixed primitive index for the specified index type. The fixed
;;     index is equal to 2^n - 1 where n is equal to 8 for
;;     :UNSIGNED-BYTE, 16 for :UNSIGNED-SHORT and 32 for :UNSIGNED-INT.

;; :RASTERIZER-DISCARD If enabled, all primitives are discarded before
;;     rasterization, but after any optional transform feedback. Also
;;     causes glClear​ and glClearBuffer​ commands to be ignored.

;; :SAMPLE-ALPHA-TO-COVERAGE If enabled, compute a temporary coverage
;;     value where each bit is determined by the alpha value at the
;;     corresponding sample location. The temporary coverage value is
;;     then ANDed with the fragment coverage value.

;; :SAMPLE-ALPHA-TO-ONE If enabled, each sample alpha value is replaced
;;     by the maximum representable alpha value.

;; :SAMPLE-COVERAGE If enabled, the fragment's coverage is ANDed with the
;;     temporary coverage value. If :SAMPLE-COVERAGE-INVERT is set to
;;     :TRUE, invert the coverage value. See glSampleCoverage.

;; :SAMPLE-SHADING If enabled, the active fragment shader is run once for
;;     each covered sample, or at fraction of this rate as determined by
;;     the current value of :MIN-SAMPLE-SHADING-VALUE. See
;;     glMinSampleShading.

;; :SAMPLE-MASK If enabled, the sample coverage mask generated for a
;;     fragment during rasterization will be ANDed with the value of
;;     :SAMPLE-MASK-VALUE before shading occurs. See glSampleMaski.

;; :SCISSOR-TEST If enabled, discard fragments that are outside the
;;     scissor rectangle. See glScissor.

;; :STENCIL-TEST If enabled, do stencil testing and update the stencil
;;     buffer. See glStencilFunc and glStencilOp.

;; :TEXTURE-CUBE-MAP-SEAMLESS If enabled, cubemap textures are sampled
;;     such that when linearly sampling from the border between two
;;     adjacent faces, texels from both faces are used to generate the
;;     final sample value. When disabled, texels from only a single face
;;     are used to construct the final sample value.

;; :PROGRAM-POINT-SIZE If enabled and a vertex or geometry shader is
;;     active, then the derived point size is taken from the (potentially
;;     clipped) shader builtin gl-PointSize​ and clamped to the
;;     implementation-dependent point size range.

;; Indexed Capabilities

;; Some of the GL's capabilities are indexed. glEnablei and glDisablei
;; enable and disable indexed capabilities. Only the following
;; capabilities may be used with indices higher than zero:


;; :BLEND If enabled, blend the computed fragment color values with the
;;     values in the specified color buffer. index​ must be less than
;;     :MAX-DRAW-BUFFERS or :INVALID-VALUE will result. See glBlendFunc​.

;; :SCISSOR-TEST If enabled, the given viewport index will have
;;     scissoring enabled or disabled. index​ must be less than
;;     :MAX-VIEWPORTS or :INVALID-VALUE will result. See
;;     glScissorIndexed​.
