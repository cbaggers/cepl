(in-package :jungl)

(defvar %current-viewport nil)

(defun current-viewport ()
  (or %current-viewport
      (jungl::attachment-viewport
       (attachment
	(or jungl::%default-framebuffer
	    (error "No default framebuffer found ~a"
		   (if (and (boundp '*gl-context*)
			    (symbol-value '*gl-context*))
		       "but we do have a gl context. This is a bug"
		       "because the GL context is not yet available")))
	0))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;{NOTE} if optimization called for it this could easily be an
;;       array of 16bit ints (or whatever works)
(defstruct (viewport (:conc-name %viewport-) (:constructor %make-viewport))
  (resolution-x 320 :type fixnum)
  (resolution-y 240 :type fixnum)
  (origin-x 0 :type fixnum)
  (origin-y 0 :type fixnum))

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

(defun viewport-resolution (viewport)
  (list (%viewport-resolution-x viewport)
        (%viewport-resolution-y viewport)))

(defun viewport-resolution-v! (viewport)
  (v! (%viewport-resolution-x viewport)
      (%viewport-resolution-y viewport)))

(defun viewport-origin (viewport)
  (v! (%viewport-origin-x viewport)
      (%viewport-origin-y viewport)))

(defmethod cepl-generics:size ((object viewport))
  (v! (%viewport-resolution-x object)
      (%viewport-resolution-y object)))

(defmethod cepl-generics:pos ((object viewport))
  (v! (%viewport-origin-x object)
      (%viewport-origin-y object)))

(defun %set-resolution (viewport x y)
  (setf (%viewport-resolution-x viewport) x
        (%viewport-resolution-y viewport) y)
  (when (eq (current-viewport) viewport)
    (%viewport viewport)))

(defun (setf viewport-resolution) (value viewport)
  (let ((value (if (typep value 'viewport)
                   (viewport-resolution value)
                   value)))
    (%set-resolution viewport (first value) (second value))))

(defun (setf viewport-resolution-v!) (value viewport)
  (let ((value (if (typep value 'cl-game-math.types:vec2)
                   (list (floor (v:x value)) (floor (v:y value)))
                   (error "The value given to (setf viewport-resolution-v!) must be a vec2"))))
    (%set-resolution viewport (first value) (second value))))

(defmethod (setf cepl-generics:size) (value (object viewport))
  (%set-resolution object (ceiling (v:x value)) (ceiling (v:y value))))
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
  `(with-viewport (attachment-viewport (%attachment ,fbo ,attachment))
     ,@body))

(defmacro %with-fbo-viewport ((fbo &optional (attachment 0)) &body body)
  "To be used by code than is managing the viewport state itself.
   composed dispatch would be an example"
  `(%with-viewport (attachment-viewport (%attachment ,fbo ,attachment))
     ,@body))

(defun clone-viewport (viewport)
  (make-viewport (viewport-resolution viewport)
		 (viewport-origin viewport)))
