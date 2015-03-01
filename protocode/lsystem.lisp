;; recurse n times
(defun rec-n (func n d)
  (if (> n 0)
      (rec-n func (- n 1) (funcall func d))
      d))



;; lsystem
(defun l (x lang) (mapcat λ(cdr (assoc % lang)) x))

;; one lsystem language
(setf l1 '((a a b) (b c c) (c a b)))

;; 6 recursions
(rec-n λ(l % l1) 6 '(a))
