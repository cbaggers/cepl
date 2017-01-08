(in-package :cepl.context)

;;------------------------------------------------------------

(defvar *context-defaults* nil)

(defun set-context-defaults (context)
  (loop :for setting :in *context-defaults* :do
     (apply (symbol-function (first setting)) (cons context (rest setting)))))

;;------------------------------------------------------------

(defun split-float-version (float)
  (let* ((fix (round float .1)))
    (multiple-value-bind (maj min) (floor fix 10)
      (list maj min))))

;;------------------------------------------------------------

;; GL_READ_FRAMEBUFFER_BINDING (name, intially 0, see glBindFramebuffer)
;;     The framebuffer object currently bound to the GL_READ_FRAMEBUFFER target. If the default framebuffer is bound, this value will be zero.
(defun read-framebuffer-binding (context)
  (declare (ignore context))
  (cl-opengl:get* :read-framebuffer-binding))

(defun (setf read-framebuffer-binding) (id context)
  (declare (ignore context))
  (gl:bind-framebuffer :read-framebuffer id)
  id)

;; GL_DRAW_FRAMEBUFFER_BINDING (name, initially 0, see glBindFramebuffer)
;;     The framebuffer object currently bound to the GL_DRAW_FRAMEBUFFER target. If the default framebuffer is bound, this value will be zero.
(defun draw-framebuffer-binding (context)
  (declare (ignore context))
  (cl-opengl:get* :draw-framebuffer-binding))

(defun (setf draw-framebuffer-binding) (id context)
  (declare (ignore context))
  (gl:bind-framebuffer :draw-framebuffer id)
  id)

;; The GL_FRAMEBUFFER target sets both the read and the write to the same FBO.
(defun framebuffer-binding (context)
  (cons (read-framebuffer-binding context)
        (draw-framebuffer-binding context)))

(defun (setf framebuffer-binding) (id context)
  (declare (ignore context))
  (gl:bind-framebuffer :framebuffer id)
  id)

;;------------------------------------------------------------

;; GL_VERTEX_ARRAY_BINDING (GLint, initially 0, see glBindVertexArray)
;; The name of the vertex array object currently bound to the context, or 0 if
;; none is bound.

(defun vertex-array-binding (context)
  (declare (ignore context))
  (cl-opengl:get* :vertex-array-binding))

(defun (setf vertex-array-binding) (id context)
  (declare (ignore context))
  (gl:bind-vertex-array id)
  id)

;;------------------------------------------------------------

(defun texture-binding (context target)
  (declare (ignore context))
  (ecase target
    (:texture-1d (gl:get* :texture-binding-1d))
    (:texture-2d (gl:get* :texture-binding-2d))
    (:texture-3d (gl:get* :texture-binding-3d))
    (:texture-1d-array (gl:get* :texture-binding-1d-array))
    (:texture-2d-array (gl:get* :texture-binding-2d-array))
    (:texture-rectangle (gl:get* :texture-binding-rectangle))
    (:texture-cube-map (gl:get* :texture-binding-cube-map))
    (:texture-cube-map-array (gl:get* :texture-binding-cube-map-array))
    (:texture-buffer (gl:get* :texture-binding-buffer))
    (:texture-2d-multisample (gl:get* :texture-binding-2d-multisample))
    (:texture-2d-multisample-array (gl:get* :texture-binding-2d-multisample-array))))

(defun (setf texture-binding) (id context target)
  (declare (ignore context))
  (gl:bind-texture target id)
  id)

;;------------------------------------------------------------

;; GL_DRAW_BUFFERi (symbolic constant, see glDrawBuffers)
;;     params returns one value, a symbolic constant indicating which buffers are being drawn to by the corresponding output color. This is selected from the currently bound GL_DRAW_FRAMEBUFFER The initial value of GL_DRAW_BUFFER0 is GL_BACK if there are back buffers, otherwise it is GL_FRONT. The initial values of draw buffers for all other output colors is GL_NONE. i can be from 0 up to the value of MAX_DRAW_BUFFERS minus one.
(defgeneric draw-buffer-i (context buffer-num))

(defmethod draw-buffer-i ((context gl-context) (buffer-num integer))
  (declare (ignore context))
  (cl-opengl:get* (kwd :draw-buffer buffer-num)))
