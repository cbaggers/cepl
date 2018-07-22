(in-package :%cepl.types)

(defun array-indexed-p (obj)
  (typep obj '(or array c-array gpu-array)))

(defun spatially-indexed-p (obj)
  (typep obj '(or texture compute-space viewport)))

(defun arrayd-dimensions (obj)
  (if (spatially-indexed-p obj)
      (reverse (cepl.measurements:dimensions obj))
      (cepl.measurements:dimensions obj)))

(defun spatial-dimensions (obj)
  (if (array-indexed-p obj)
      (reverse (cepl.measurements:dimensions obj))
      (cepl.measurements:dimensions obj)))
