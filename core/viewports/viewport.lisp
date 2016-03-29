(in-package :cepl.viewports)

(defvar %current-viewport nil)

(defun current-viewport ()
  (or %current-viewport
      (cepl.fbos:attachment-viewport
       (cepl.fbos:attachment
	(or %default-framebuffer
	    (error "No default framebuffer found ~a"
		   (if (and (boundp '*gl-context*)
			    (symbol-value '*gl-context*))
		       "but we do have a gl context. This is a bug"
		       "because the GL context is not yet available")))
	0))))

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

(defun viewport-resolution (viewport)
  (v! (%viewport-resolution-x viewport)
      (%viewport-resolution-y viewport)))

(defmethod resolution ((viewport viewport))
  (viewport-resolution viewport))

(defmethod (setf resolution) (value (viewport viewport))
  (setf (viewport-resolution viewport) value))

(defun viewport-origin (viewport)
  (v! (%viewport-origin-x viewport)
      (%viewport-origin-y viewport)))

(defun %set-resolution (viewport x y)
  (setf (%viewport-resolution-x viewport) x
        (%viewport-resolution-y viewport) y)
  (when (eq (current-viewport) viewport)
    (%viewport viewport)))

(defun (setf viewport-dimensions) (value viewport)
  (let ((value (if (typep value 'viewport)
                   (viewport-dimensions value)
                   value)))
    (%set-resolution viewport (first value) (second value))))

(defun (setf viewport-resolution) (value viewport)
  (let ((value (if (typep value 'rtg-math.types:vec2)
                   (list (floor (v:x value)) (floor (v:y value)))
                   (error "The value given to (setf viewport-resolution) must be a vec2"))))
    (%set-resolution viewport (first value) (second value))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun %viewport (viewport)
  (gl:viewport
   (%viewport-origin-x viewport) (%viewport-origin-y viewport)
   (%viewport-resolution-x viewport) (%viewport-resolution-y viewport)))

(defmacro %with-viewport (viewport &body body)
  `(progn
     (%viewport ,viewport)
     ,@body))

(defmacro with-viewport (viewport &body body)
  (let ((once (gensym "viewport")))
    `(prog1
         (let* ((,once ,viewport)
                (%current-viewport ,once))
           (%with-viewport ,once ,@body))
       (%viewport (current-viewport)))))

;;{TODO} how are we meant to set origin?
;;       Well attachments dont have position so it wouldnt make sense
;;       you can however create other viewports and with with-viewport
;;       to make them current, then rendering with render to that viewport

(defmacro with-fbo-viewport ((fbo &optional (attachment 0)) &body body)
  `(with-viewport (cepl.fbos:attachment-viewport (cepl.fbos::%attachment ,fbo ,attachment))
     ,@body))

(defmacro %with-fbo-viewport ((fbo &optional (attachment 0)) &body body)
  "To be used by code than is managing the viewport state itself.
   composed dispatch would be an example"
  `(%with-viewport (cepl.fbos:attachment-viewport (cepl.fbos::%attachment ,fbo ,attachment))
     ,@body))


(defun viewport-params-to-vec4 (&optional (viewport (current-viewport)))
  (v! (%viewport-origin-x viewport)
      (%viewport-origin-y viewport)
      (%viewport-resolution-x viewport)
      (%viewport-resolution-y viewport)))
