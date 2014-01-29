(in-package :tiny-time-manager)

;; This is meant as an example rather than for serious use.

;;{TODO} The funcall should catch errors and offer to remove the function
;;       from the time manager
(let ((entries (list t)))
  (defun update ()
    (let ((last entries)
          (current (cdr entries)))
      (loop :until (null current) :do
         (if (expiredp (funcall (car current)))
             (setf (cdr last) (cdr current)
                   last current)
             (setf (cdr last) current
                   last current))
         (setf current (cdr current)))))
  (defun manage (item) (setf (cdr entries) (list item)))
  (defun releaae (item) (delete item entries))
  (defun clean () (setf (cdr entries) nil)))
