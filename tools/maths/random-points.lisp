(in-package #:tools)

(defun rqpos (max &optional (min 0))
  (let* ((angle (/ (random 628) 100))
         (vec (v! (sin angle) (cos angle) 0))
         (q (q:make-quat-from-axis-angle vec (/ (random 628) 100)))
         (len (+ min (/ (random (* 100 (- max min))) 100))))
    (q:rotate (v! 0 0 len) q)))
