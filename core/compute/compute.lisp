(in-package :cepl.compute)

;;------------------------------------------------------------

(defn compute-space-dimensions ((space compute-space)) list
  (list (compute-space-size-x space)
        (compute-space-size-y space)
        (compute-space-size-z space)))

(defn (setf compute-space-dimensions) ((value list) (space compute-space)) list
  (destructuring-bind (x y z) value
    (setf (compute-space-size-x space) x
          (compute-space-size-y space) y
          (compute-space-size-z space) z)
    value))

(defmethod dimensions ((space compute-space))
  (compute-space-dimensions space))

(defmethod (setf dimensions) ((value list) (space compute-space))
  (setf (compute-space-dimensions space) value))

(defn compute-space-as-uvec3 ((space compute-space)) uvec3
  ;; same as ↓ inlined
  ;; (v!uint (compute-space-size-x space)
  ;;         (compute-space-size-y space)
  ;;         (compute-space-size-z space))
  (let ((arr (make-array 3 :element-type '(unsigned-byte 32))))
    (setf (aref arr 0) (compute-space-size-x space)
          (aref arr 1) (compute-space-size-y space)
          (aref arr 2) (compute-space-size-z space))
    arr))

(defn (setf compute-space-as-uvec3) ((value uvec3) (space compute-space))
    uvec3
  (setf (compute-space-size-x space) (aref value 0)
        (compute-space-size-y space) (aref value 1)
        (compute-space-size-z space) (aref value 2))
  value)

;;------------------------------------------------------------
