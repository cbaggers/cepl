(in-package :cepl.viewports)

(defun %set-current-viewport (cepl-context viewport)
  (with-slots (cepl.context::current-viewport) cepl-context
    (%viewport viewport)
    (setf cepl.context::current-viewport viewport)))

(defun current-viewport ()
  (with-slots (cepl.context::current-viewport) cepl.context:*cepl-context*
    (or cepl.context::current-viewport
        (error "No default framebuffer found ~a"
               (if (and (boundp '*gl-context*)
                        (symbol-value '*gl-context*))
                   "but we do have a gl context. This is a bug"
                   "because the GL context is not yet available")))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun make-viewport (&optional (resolution '(320 240)) (origin '(0 0)))
  (if (listp origin)
      (%make-viewport :resolution-x (first resolution)
                      :resolution-y (second resolution)
                      :origin-x (first origin)
                      :origin-y (second origin))
      (%make-viewport :resolution-x (first resolution)
                      :resolution-y (second resolution)
                      :origin-x (ceiling (v:x origin))
                      :origin-y (ceiling (v:y origin)))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod dimensions ((viewport viewport))
  (viewport-dimensions viewport))

(defmethod (setf dimensions) (value (viewport viewport))
  (setf (viewport-dimensions viewport) value))

(defun viewport-dimensions (viewport)
  (list (%viewport-resolution-x viewport)
        (%viewport-resolution-y viewport)))

(defun (setf viewport-dimensions) (value viewport)
  (let ((dim (if (typep value 'viewport)
                 (viewport-dimensions value)
                 value)))
    (%set-resolution viewport (first dim) (second dim))
    value))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


(defmethod resolution ((viewport viewport))
  (viewport-resolution viewport))

(defmethod (setf resolution) (value (viewport viewport))
  (setf (viewport-resolution viewport) value))

(defun viewport-resolution (viewport)
  (v! (%viewport-resolution-x viewport)
      (%viewport-resolution-y viewport)))

(defun (setf viewport-resolution) (value viewport)
  (unless (typep value 'rtg-math.types:vec2)
    (error "The value given to (setf viewport-resolution) must be a vec2"))
  (%set-resolution viewport (floor (v:x value)) (floor (v:y value))))

(defun %set-resolution (viewport x y)
  (setf (%viewport-resolution-x viewport) x
        (%viewport-resolution-y viewport) y)
  (with-slots (default-viewport) *cepl-context*
    (when (eq viewport default-viewport)
      (cepl.fbos::%update-default-framebuffer-dimensions x y)))
  (when (eq (current-viewport) viewport)
    (%viewport viewport)))

(defun viewport-resolution-x (viewport)
  (%viewport-resolution-x viewport))

(defun viewport-resolution-y (viewport)
  (%viewport-resolution-y viewport))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun viewport-origin (viewport)
  (v! (%viewport-origin-x viewport)
      (%viewport-origin-y viewport)))

(defun (setf viewport-origin) (value viewport)
  (setf (%viewport-origin-x viewport) (floor (v:x value))
	(%viewport-origin-y viewport) (floor (v:y value)))
  value)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun %viewport (viewport)
  (gl:viewport
   (%viewport-origin-x viewport) (%viewport-origin-y viewport)
   (%viewport-resolution-x viewport) (%viewport-resolution-y viewport))
  viewport)

(defmacro with-viewport (viewport &body body)
  (alexandria:with-gensyms (old-viewport vp ctx)
    `(let* ((,ctx *cepl-context*)
            (,old-viewport (current-viewport))
            (,vp ,viewport))
       (%set-current-viewport ,ctx ,vp)
       (unwind-protect (progn ,@body)
         (%set-current-viewport ,ctx ,old-viewport)))))

;;{TODO} how are we meant to set origin?
;;       Well attachments dont have position so it wouldnt make sense
;;       you can however create other viewports and with with-viewport
;;       to make them current, then rendering with render to that viewport

(defmacro with-fbo-viewport ((fbo &optional (attachment 0)) &body body)
  `(with-viewport (cepl.fbos:attachment-viewport ,fbo ,attachment)
     ,@body))

(defmacro %with-fbo-viewport ((fbo &optional (attachment 0)) &body body)
  "To be used by code than is managing the viewport state itself.
   composed dispatch would be an example"
  `(%with-viewport (cepl.fbos:attachment-viewport ,fbo ,attachment)
     ,@body))


(defun viewport-params-to-vec4 (&optional (viewport (current-viewport)))
  (v! (%viewport-origin-x viewport)
      (%viewport-origin-y viewport)
      (%viewport-resolution-x viewport)
      (%viewport-resolution-y viewport)))
