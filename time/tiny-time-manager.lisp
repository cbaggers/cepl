(in-package :tiny-time-manager)

;; This is meant as an example rather than for serious use.

;;{TODO} The funcall should catch errors and offer to remove the function
;;       from the time manager
(let ((entries (list t)))
  (defun update ()
    (let ((last entries)
          (current (cdr entries)))
      (loop :until (null current) :do
         (if (conditional-functions::expiredp (funcall (car current)))
             (setf (cdr last) (cdr current)
                   last current)
             (setf (cdr last) current
                   last current))
         (setf current (cdr current)))))
  (defun manage (item) (setf entries (append entries (list item))))
  (defun release (item) (delete item entries))
  (defun clean () (setf (cdr entries) (list t))))

