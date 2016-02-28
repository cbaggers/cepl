(in-package :cepl.host)

(let (step-func
      swap-func
      swap-arg)
  (defun set-step-func (func)
    (setf step-func func))
  (defun set-swap-func (func)
    (setf swap-func func))
  (defun set-default-swap-arg (win-handle)
    (setf swap-arg win-handle))
  (defun host-step ()
    (funcall step-func))
  (defun host-swap (&optional (win swap-arg))
    (funcall swap-func win)))
