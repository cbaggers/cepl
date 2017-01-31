(in-package :cepl.context)

;;------------------------------------------------------------

(defclass gl-context ()
  ((handle :initarg :handle :reader handle)
   (window :initarg :window :reader window)
   (version-major :initarg :version-major :reader major-version)
   (version-minor :initarg :version-minor :reader minor-version)
   (version-float :initarg :version-float :reader version-float)))

;;------------------------------------------------------------

(let ((available-extensions nil))
  (defun has-feature (x)
    (unless available-extensions
      (let* ((exts (if (>= (gl:major-version) 3)
                       (loop :for i :below (gl:get-integer :num-extensions)
                          :collect (%gl:get-string-i :extensions i))
                       ;; OpenGL version < 3
                       (cepl-utils:split-string
			#\space (gl:get-string :extensions))))
             (exts (append exts
                           (mapcar (lambda (x)
                                     (cepl-utils:kwd (string-upcase (subseq x 3))))
                                   exts))))
        (setf available-extensions exts)))
    (not (null (find x available-extensions :test #'equal)))))

;;------------------------------------------------------------

(defun ensure-cepl-compatible-setup ()
  (unless (or (> (gl:major-version) 3)
	      (and (= (gl:major-version) 3)
		   (>= (gl:minor-version) 1)))
    (error "Cepl requires OpenGL 3.1 or higher. Found: ~a.~a"
           (gl:major-version) (gl:minor-version))))

;;------------------------------------------------------------

(defun %set-default-gl-options ()
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :less)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))

;;------------------------------------------------------------
;; Homeless stuff

(let ((cache 0))
  (defun max-draw-buffers (context)
    (declare (ignore context))
    (if (= cache 0)
        (setf cache (cl-opengl:get* :max-draw-buffers))
        cache)))

(defvar *context-defaults* nil)

(defun set-context-defaults (context)
  (loop :for setting :in *context-defaults* :do
     (apply (symbol-function (first setting)) (cons context (rest setting)))))


;; GL_DRAW_BUFFERi (symbolic constant, see glDrawBuffers)
;;     params returns one value, a symbolic constant indicating which buffers are being drawn to by the corresponding output color. This is selected from the currently bound GL_DRAW_FRAMEBUFFER The initial value of GL_DRAW_BUFFER0 is GL_BACK if there are back buffers, otherwise it is GL_FRONT. The initial values of draw buffers for all other output colors is GL_NONE. i can be from 0 up to the value of MAX_DRAW_BUFFERS minus one.
(defgeneric draw-buffer-i (context buffer-num))

(defmethod draw-buffer-i ((context gl-context) (buffer-num integer))
  (declare (ignore context))
  (cl-opengl:get* (kwd :draw-buffer buffer-num)))
