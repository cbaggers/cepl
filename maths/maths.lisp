(in-package :maths)

(defun lerp (start end ammount)
  (+ start (* ammount (- end start))))

(defun mix (start end ammount)
  (lerp start end ammount))


(defun stepv (threshold x)
  (if (< x threshold) 0 1))

(defun clamp (x min max)
  (alexandria:clamp x min max))

(defun smoothstep (a b x)
  (cond ((< x a) 0)
        ((>= x b) 1)
        (t (let ((x (/ (- x a) (- b a))))
             (* x x (- 3 (* 2 x)))))))

(defun pulse (a b x)
  (- (stepv a x) (stepv b x)))

