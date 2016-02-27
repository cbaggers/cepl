(in-package :cepl.host)

(let (step-func
      swap-func
      swap-arg)
  (defun cache-step-func ()
    (setf step-func (get-step-func)))
  (defun cache-swap-func (win-handle)
    (setf swap-arg win-handle)
    (setf swap-func (get-swap-func)))
  (defun host-step ()
    (funcall step-func))
  (defun host-swap ()
    (funcall swap-func swap-arg)))
