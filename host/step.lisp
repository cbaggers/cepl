(in-package :cepl.host)

(let (step-func)
  (defun cache-step-func ()
    (get-step-func))
  (defun step-backend ()
    (funcall step-func)))
