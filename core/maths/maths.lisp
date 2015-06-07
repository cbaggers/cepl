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

(defvar *coef*
  (make-array 16 :element-type 'single-float
              :initial-contents '(-0.5  1.5 -1.5  0.5
                                  1.0  -2.5  2.0 -0.5
                                  -0.5  0.0  0.5  0.0
                                  0.0   1.0  0.0  0.0)))
(defun spline (x knots)
  (let* ((nknots (length knots))
         (nspans (- nknots 3)))
    (unless (> nspans 0) (error "Spline has too few knots"))
    (let* ((x (* (clamp x 0 1) nspans))
           (span (if (>= x (- nknots 3))
                     (floor (- nknots 3))
                     (floor x)))
           (x (- x span)))
      (let ((c3 (+ (* (aref *coef* 0)  (elt knots span))
                   (* (aref *coef* 1)  (elt knots (+ 1 span)))
                   (* (aref *coef* 2)  (elt knots (+ 2 span)))
                   (* (aref *coef* 3)  (elt knots (+ 3 span)))))
            (c2 (+ (* (aref *coef* 4)  (elt knots span))
                   (* (aref *coef* 5)  (elt knots (+ 1 span)))
                   (* (aref *coef* 6)  (elt knots (+ 2 span)))
                   (* (aref *coef* 7)  (elt knots (+ 3 span)))))
            (c1 (+ (* (aref *coef* 8)  (elt knots span))
                   (* (aref *coef* 9)  (elt knots (+ 1 span)))
                   (* (aref *coef* 10) (elt knots (+ 2 span)))
                   (* (aref *coef* 11) (elt knots (+ 3 span)))))
            (c0 (+ (* (aref *coef* 12) (elt knots span))
                   (* (aref *coef* 13) (elt knots (+ 1 span)))
                   (* (aref *coef* 14) (elt knots (+ 2 span)))
                   (* (aref *coef* 15) (elt knots (+ 3 span))))))
        (+ c0 (* x (+ c1 (* x (+ c2 (* x c3))))))))))

(defun gamma-correct (gamma x)
  (expt x (/ 1 gamma)))

(defun bias (b x)
  (expt x (/ (log b) (log 0.5))))

(defun gain (g x)
  (if (< x 0.5)
      (/ (bias (- 1 g) (* 2 x)) 2)
      (- 1 (/ (bias (- 1 g) (- 2 (* 2 x))) 2))))
