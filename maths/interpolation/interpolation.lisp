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
(defmacro nlerp3 (target end progress) 
  (let ((gtarget (gensym "target"))
        (gend (gensym "gend"))
        (gprogress (gensym "progress")))
    `(let ((,gtarget ,target)
           (,gend ,end)
           (,gprogress ,progress))
       (setf ,target (v3:v+1 ,gtarget (v3:v* (v3:v-1 ,gend ,gtarget) ,gprogress))))))

(defmacro nnorm-lerp3 (target end progress) 
  (let ((gtarget (gensym "target"))
        (gend (gensym "gend"))
        (gprogress (gensym "progress")))
    `(let ((,gtarget ,target)
           (,gend ,end)
           (,gprogress ,progress))
       (setf ,target (v3:normalize (lerp3 ,gtarget ,gend ,gprogress))))))

(defmacro nslerp3 (target end progress)
  (let ((gtarget (gensym "target"))
        (gend (gensym "gend"))
        (gprogress (gensym "progress")))
    `(let ((,gtarget ,target)
           (,gend ,end)
           (,gprogress ,progress)
           (angle (* (acos (clampf -1.0 1.0 (v3:dot ,gtarget ,gend))) ,gprogress))
           (relative-vec (v3:normalize (v3:v-1 ,gend (v3:v*vec ,gtarget dot)))))
       (setf ,target (v3:v+1 (v3:v* ,gtarget (cos angle)) (v3:v* relative-vec (sin angle)))))))
