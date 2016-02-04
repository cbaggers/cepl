(in-package :jungl)

(let (swap-func)
  (defun update-display ()
    (unless swap-func
      (setf swap-func (cepl.host:get-swap-func)))
    (funcall swap-func *gl-window*)))

(defun cls ()
  "This func is here because it makes me happy"
  (with-fbo-bound (%default-framebuffer :with-viewport nil :with-blending nil)
    (clear) (update-display)
    (clear) (update-display)))
