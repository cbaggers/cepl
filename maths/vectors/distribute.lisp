(in-package :cepl)

(defun distrib (shape size num-of-points)
  (assert (> num-of-points 1))
  (case shape 
    (:square (distribute-wire-hypercube size num-of-points 2) )
    (:cube (distribute-wire-hypercube size num-of-points 3))
    (:tesseract (distribute-wire-hypercube size num-of-points 4))
    (:sphere (distribute-wire-sphere size num-of-points))))

;;--------------------------------------------------

(defun distribute-wire-sphere (radius num-of-points)
  (let ((count (ceiling (+ 1 (/ (- num-of-points 2) 10)))))
    (subseq 
     (remove-duplicates 
      (first (primitives:sphere-data
                    :radius (float radius)
                    :lines-of-latitude count :lines-of-latitude count
                    :normals nil :tex-coords nil))
      :test (lambda (x y) (< (v3:distance x y) 0.001)))     
     0 num-of-points)))

;;--------------------------------------------------

(defun distribute-wire-hypercube (len num-of-points dimensions)
  (let ((count (%hyper-cube-min-count-for-points num-of-points dimensions)))
    (sort-points 
     (subseq (%distribute-wire-hypercube len count dimensions)
             0 num-of-points))))

(defun %hyper-cube-min-count-for-points (x dimensions)
  (ceiling (+ 1 (/ (- (/ x (expt 2 dimensions)) 1) dimensions))))

(defun %distribute-wire-hypercube (len count dimensions)
  (let* ((corner (%create-corner len count dimensions)))    
    (loop for inverse-mask below (expt 2 dimensions) append
         (loop for p in corner collect
              (make-array dimensions :element-type 'single-float
                          :initial-contents
                          (loop for e across p for j from 0 collect
                               (if (= (ash inverse-mask (- j)) 1)
                                   (- e) 
                                   e)))))))

(defun %create-corner (len count dimensions)
  (let ((line (1d-line len count)))
    (cons (make-array dimensions :element-type 'single-float 
                      :initial-element (float len))
          (loop for i below dimensions append
               (loop for p in (rest line) collect
                    (let ((tmp (make-array dimensions
                                           :element-type 'single-float 
                                           :initial-element (float len))))
                      (setf (aref tmp i) (float p))
                      tmp))))))

(defun sort-points (points)
  (mapcar #'first
        (sort (mapcar (lambda (x) (list x (loop for i across x sum (* i i)))) 
                      points)
              #'> :key #'second)))

;;--------------------------------------------------

(defun fib-sphere (samples &optional randomize)
  (let ((rnd (if randomize (random (float samples)) 1))
        (offset (/ 2.0 samples))
        (increment (* (coerce pi 'single-float) (- 3.0 (sqrt 5)))))
    (loop :for i :below samples :collect
       (let* ((y (+ (- (* i offset) 1) (/ offset 2)))
              (r (sqrt (- 1 (expt y 2))))
              (phi (* (mod (+ i rnd) samples) increment))
              (x (* (cos phi) r))
              (z (* (sin phi) r)))
         (make-array 3 :element-type 'single-float
                     :initial-contents (list x y z))))))


;;--------------------------------------------------

;; (defun distrib-triangle (count)
;;   (let ((line (1d-line count)))
;;     (remove-duplicates
;;      (append (offset-line offset-vec angle line)
;;              (offset-line offset-vec angle line)
;;              (offset-line offset-vec angle line))
;;      :test #'equalp)))
;; should be v2:v=
