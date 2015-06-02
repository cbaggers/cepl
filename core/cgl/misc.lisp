(in-package :cgl)

(let (swap-func)
  (defun update-display ()
    (unless swap-func
      (setf swap-func (cepl-backend:get-swap-func cepl-backend:*backend*)))
    (funcall swap-func *gl-window*)))
