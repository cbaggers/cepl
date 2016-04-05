(in-package :cepl.memory)

(defmethod initialized-p ((object t)) t)

(defmethod initialized-p ((object texture))
  (not (eq (texture-type object) :uninitialized)))

(defmethod initialized-p ((object gpu-buffer))
  (not (equal (gpu-buffer-format object)
	      '(:uninitialized))))

(defmethod initialized-p ((object gpu-array-bb))
  (not (eq (gpu-array-bb-access-style object)
	   :uninitialized)))

(defmethod initialized-p ((object gpu-array-t))
  (not (eq (gpu-array-t-texture-type object)
	   :uninitialized)))

(defmethod initialized-p ((object fbo))
  (not (= (%fbo-clear-mask object) -13)))
