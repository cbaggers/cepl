(in-package :jungl)

(defun swap ()
  (cepl.host:host-swap))

(defun cls ()
  "This func is here because it makes me happy"
  (with-fbo-bound (%default-framebuffer :with-viewport nil :with-blending nil)
    (clear) (cepl.host:host-swap)
    (clear) (cepl.host:host-swap)))
