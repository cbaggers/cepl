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
             (if (and (cepl-context)
                      (cepl.context::%cepl-context-gl-context (cepl-context)))
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


(defn-inline viewport-resolution-x ((viewport viewport)) single-float
  (float (%viewport-resolution-x viewport) 0f0))

(defn-inline viewport-resolution-y ((viewport viewport)) single-float
  (float (%viewport-resolution-y viewport) 0f0))

(defn-inline viewport-origin-x ((viewport viewport)) single-float
  (float (%viewport-origin-x viewport) 0f0))

(defn-inline viewport-origin-y ((viewport viewport)) single-float
  (float (%viewport-origin-y viewport) 0f0))

(defn viewport-resolution ((viewport viewport)) vec2
  (declare (profile t))
  (vec2 (viewport-resolution-x viewport)
        (viewport-resolution-y viewport)))

(defn (setf viewport-resolution) ((value vec2) (viewport viewport)) vec2
  (%set-resolution viewport
                   (floor (aref value 0))
                   (floor (aref value 1)))
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
      (%update-default-framebuffer-dimensions x y)))
  (values))

;; whilst this is an fbo function it lives here to avoid the circular
;; dependency that would result otherwise
(defun+ %update-default-framebuffer-dimensions (x y)
  (%with-cepl-context-slots (default-framebuffer) (cepl-context)
    (let ((dimensions (list x y))
          (fbo default-framebuffer))
      (map nil
           (lambda (x)
             (setf (gpu-array-dimensions (att-array x)) dimensions)
             (setf (viewport-dimensions (att-viewport x)) dimensions))
           (%fbo-color-arrays fbo))
      (when (%fbo-depth-array fbo)
        (let ((arr (%fbo-depth-array fbo)))
          (setf (gpu-array-dimensions (att-array arr)) dimensions)
          (setf (viewport-dimensions (att-viewport arr)) dimensions)))
      fbo)))

;;------------------------------------------------------------

(defn viewport-origin ((viewport viewport)) vec2
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (vec2 (float (%viewport-origin-x viewport) 0f0)
        (float (%viewport-origin-y viewport) 0f0)))

(defn (setf viewport-origin) ((value (or vec2 uvec2)) (viewport viewport))
    (or vec2 uvec2)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (etypecase value
    (vec2
     (locally
         #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         ;; muffled unavoidable checks that new value is in the
         ;; (unsigned-byte 16) range.
         (setf (%viewport-origin-x viewport) (floor (aref value 0))
               (%viewport-origin-y viewport) (floor (aref value 1)))))
    (uvec2
     (setf (%viewport-origin-x viewport) (aref value 0)
           (%viewport-origin-y viewport) (aref value 1))))
  value)

(defn viewport-origin-i ((viewport viewport)) uvec2
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (make-array 2 :element-type '(unsigned-byte 32)
              :initial-contents (list (%viewport-origin-x viewport)
                                      (%viewport-origin-y viewport))))

(defn (setf viewport-origin-i) ((value uvec2) (viewport viewport))
    (or vec2 uvec2)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (setf (%viewport-origin-x viewport) (aref value 0)
        (%viewport-origin-y viewport) (aref value 1))
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
         (release-unwind-protect (progn ,@body)
           (when ,unbind
             (%set-current-viewport ,ctx ,old-viewport)))))))

;;{TODO} how are we meant to set origin?
;;       Well attachments dont have position so it wouldnt make sense
;;       you can however create other viewports and with with-viewport
;;       to make them current, then rendering with render to that viewport

(defmacro with-fbo-viewport ((fbo &optional (attachment-for-size 0))
                             &body body)
  `(with-viewport (cepl.fbos::attachment-viewport-allowing-t
                   ,fbo
                   ,attachment-for-size)
     ,@body))


(defn viewport-params-to-vec4 (&optional (viewport viewport (current-viewport)))
    vec4
  (declare (profile t))
  (vec4 (float (%viewport-origin-x viewport) 0f0)
        (float (%viewport-origin-y viewport) 0f0)
        (float (%viewport-resolution-x viewport) 0f0)
        (float (%viewport-resolution-y viewport) 0f0)))
