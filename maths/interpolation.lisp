(in-package :interpolation)

;;{TODO} Make generic version and make optimize the non generic ones

(defun lerp-number (start end progress) (+ start (* progress (- end start))))

(defun lerp3 (start end progress) 
  (v3:v+1 start (v3:v* (v3:v-1 end start) progress)))

(defun nlerp3 (start end progress) (v3:normalize (lerp3 start end progress)))

(defun slerp3 (start end progress)
  (let ((angle (* (acos (clampf -1.0 1.0 (v3:dot start end))) progress))
        (relative-vec (v3:normalize (v3:v-1 end (v3:v*vec start dot)))))
    (v3:v+1 (v3:v* start (cos angle)) (v3:v* relative-vec (sin angle)))))
