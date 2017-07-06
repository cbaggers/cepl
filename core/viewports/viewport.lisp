(in-package :cepl.viewports)

;;------------------------------------------------------------

(defn-inline viewport-eql ((v0 viewport) (v1 viewport)) boolean
  (and (= (%viewport-resolution-x v0) (%viewport-resolution-x v1))
       (= (%viewport-resolution-y v0) (%viewport-resolution-y v1))
       (= (%viewport-origin-x v0) (%viewport-origin-x v1))
       (= (%viewport-origin-y v0) (%viewport-origin-y v1))))

;;------------------------------------------------------------

(defn-inline %set-current-viewport ((cepl-context cepl-context)
                                    (viewport viewport))
    boolean
  ;; the boolean tells with-viewport whether it needs to restore the
  ;; old value.
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (inline viewport-eql)
           (profile t))
  (%with-cepl-context-slots (current-viewport) cepl-context
    (unless (eq current-viewport viewport)
      (%gl:viewport
        (%viewport-origin-x viewport) (%viewport-origin-y viewport)
        (%viewport-resolution-x viewport) (%viewport-resolution-y viewport))
      (setf current-viewport viewport)
      t)))

(defn-inline %current-viewport ((cepl-context cepl-context)) (or null viewport)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (%with-cepl-context-slots (current-viewport) cepl-context
    current-viewport))

(defn current-viewport (&optional (cepl-context cepl-context (cepl-context)))
    viewport
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (inline %current-viewport)
           (profile t))
  (or (%current-viewport cepl-context)
      (error "No current viewport found ~a"
             (if (and (boundp '*gl-context*)
                      (symbol-value '*gl-context*))
                 "but we do have a gl context. This is a bug"
                 "because the GL context is not yet available"))))

;;------------------------------------------------------------

(defmethod dimensions ((viewport viewport))
  (viewport-dimensions viewport))

(defmethod (setf dimensions) (value (viewport viewport))
  (setf (viewport-dimensions viewport) value))

(defun+ viewport-dimensions (viewport)
  (list (%viewport-resolution-x viewport)
        (%viewport-resolution-y viewport)))

(defun+ (setf viewport-dimensions) (value viewport)
  (let ((dim (if (typep value 'viewport)
                 (viewport-dimensions value)
                 value)))
    (%set-resolution viewport (first dim) (second dim))
    value))

;;------------------------------------------------------------

(defmethod resolution ((viewport viewport))
  (viewport-resolution viewport))

(defmethod (setf resolution) (value (viewport viewport))
  (setf (viewport-resolution viewport) value))

(defn viewport-resolution ((viewport viewport)) rtg-math.types:vec2
  (declare (profile t))
  (v2:make (float (%viewport-resolution-x viewport))
           (float (%viewport-resolution-y viewport))))

(defn (setf viewport-resolution) ((value vec2) (viewport viewport)) vec2
  (%set-resolution viewport (floor (v:x value)) (floor (v:y value)))
  (when (eq viewport (current-viewport))
    (%gl:viewport
     (%viewport-origin-x viewport) (%viewport-origin-y viewport)
     (%viewport-resolution-x viewport) (%viewport-resolution-y viewport)))
  value)

(defun+ %set-resolution (viewport x y)
  (setf (%viewport-resolution-x viewport) x
        (%viewport-resolution-y viewport) y)
  (%with-cepl-context-slots (default-viewport) (cepl-context)
    (when (eq viewport default-viewport)
      (cepl.fbos::%update-default-framebuffer-dimensions x y)))
  (values))

(defun+ viewport-resolution-x (viewport)
  (%viewport-resolution-x viewport))

(defun+ viewport-resolution-y (viewport)
  (%viewport-resolution-y viewport))

;;------------------------------------------------------------

(defn viewport-origin ((viewport viewport)) vec2
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (inline %current-viewport)
           (profile t))
  (v2:make (float (%viewport-origin-x viewport) 0f0)
           (float (%viewport-origin-y viewport) 0f0)))

(defn (setf viewport-origin) ((value (or vec2 uvec2)) (viewport viewport))
    (or vec2 uvec2)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (inline %current-viewport)
           (profile t))
  (setf (%viewport-origin-x viewport) (floor (v:x value))
        (%viewport-origin-y viewport) (floor (v:y value)))
  value)

(defmethod origin ((viewport viewport))
  (viewport-origin viewport))

(defmethod (setf origin) (value (viewport viewport))
  (setf (viewport-origin viewport) value))

;;------------------------------------------------------------

(defmacro with-viewport (viewport &body body)
  (alexandria:with-gensyms (old-viewport vp ctx unbind)
    `(with-cepl-context (,ctx)
       (let* ((,old-viewport (current-viewport))
              (,vp ,viewport)
              (,unbind (%set-current-viewport ,ctx ,vp)))
         (unwind-protect (progn ,@body)
           (when ,unbind
             (%set-current-viewport ,ctx ,old-viewport)))))))

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


(defn viewport-params-to-vec4 (&optional (viewport viewport (current-viewport)))
    vec4
  (declare (profile t))
  (v4:make (float (%viewport-origin-x viewport) 0f0)
           (float (%viewport-origin-y viewport) 0f0)
           (float (%viewport-resolution-x viewport) 0f0)
           (float (%viewport-resolution-y viewport) 0f0)))
