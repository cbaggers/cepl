(in-package :cepl-backend)

(let (step-func)
  (defun cache-step-func ()
    (get-step-func *backend*))
  (defun step-backend ()
    (funcall step-func)))
