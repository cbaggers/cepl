(in-package :tiny-time-manager)

;; This is meant as an example rather than for serious use.

;;{TODO} The funcall should catch errors and offer to remove the function
;; from the time manager
(let ((entries (list t)))
  (defun expose () entries)
  (defun update ()
    (let ((last entries)
          (current (cdr entries)))
      (loop :until (null current) :do
         (if (restart-case (conditional-functions::expiredp
                            (funcall (car current)))
               (remove-managed-tfunction () t))
             (setf (cdr last) (cdr current)
                   last current)
             (setf (cdr last) current
                   last current))
         (setf current (cdr current)))
      t))
  (defun add (item) (setf entries (append entries (list item))) item)
  (defun release (item) (delete item entries))
  (defun clean () (setf entries (list t))))
