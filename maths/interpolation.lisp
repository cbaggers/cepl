(in-package :interpolation)

;;{TODO} Make generic version and make optimize the non generic ones

(defun lerp-number (start end progress) (+ start (* progress (- end start))))

(defun lerp3 (start end progress) 
  (v3:v+1 start (v3:v* (v3:v-1 end start) progress)))

(defun norm-lerp3 (start end progress) (v3:normalize (lerp3 start end progress)))

(defun slerp3 (start end progress)
  (let ((angle (* (acos (clampf -1.0 1.0 (v3:dot start end))) progress))
        (relative-vec (v3:normalize (v3:v-1 end (v3:v*vec start dot)))))
    (v3:v+1 (v3:v* start (cos angle)) (v3:v* relative-vec (sin angle)))))

;;{TODO} Make these use destructive functions to avoid memeory cons
;;       current this only saves typing 'setf'
(defun nlerp3 (target end progress) 
  (setf target (v3:v+1 target (v3:v* (v3:v-1 end target) progress))))

(defun nnorm-lerp3 (target end progress) 
  (setf target (v3:normalize (lerp3 target end progress))))

(defun nslerp3 (target end progress)
  (let ((angle (* (acos (clampf -1.0 1.0 (v3:dot target end))) progress))
        (relative-vec (v3:normalize (v3:v-1 end (v3:v*vec target dot)))))
    (setf target (v3:v+1 (v3:v* target (cos angle)) (v3:v* relative-vec (sin angle))))))
