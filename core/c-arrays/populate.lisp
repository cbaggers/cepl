(in-package :cepl.c-arrays)

(defun+ c-populate (c-array data &optional (check-sizes t))
  (let ((structp (c-array-struct-element-typep c-array)))
    (labels ((walk-to-dpop (data dimensions idx)
               (let ((dim-size (first dimensions))
                     (dims-rest (rest dimensions)))
                 (if dims-rest
                     (loop
                        :for elem :in data
                        :for i :below dim-size
                        :do (setf idx (walk-to-dpop elem dims-rest idx)))
                     (loop
                        :for elem :in data
                        :for i :below dim-size
                        :do (if structp
                                (populate (row-major-aref-c c-array idx) elem)
                                (progn
                                  (setf (row-major-aref-c c-array idx) elem)
                                  (incf idx)))))
                 idx))
             (dpop-with-array (data)
               (loop
                  :for i :below (array-total-size data)
                  :for elem := (row-major-aref data i)
                  :do (if structp
                          (populate (row-major-aref-c c-array i) elem)
                          (setf (row-major-aref-c c-array i) elem)))))
      (when check-sizes
        (unless (validate-dimensions data (c-array-dimensions c-array))
          (error "Dimensions of array differs from that of the data:~%~a~%~a"
                 c-array data)))
      (typecase data
        (sequence (walk-to-dpop data (c-array-dimensions c-array) 0))
        (array (dpop-with-array data)))
      c-array)))

(defun+ rm-index-to-coords (index subscripts)
  (let ((cur index))
    (nreverse
     (loop :for s :in subscripts :collect
        (multiple-value-bind (x rem) (floor cur s)
          (setq cur x)
          rem)))))

(defun+ validate-dimensions (data dimensions)
  (let* ((dimensions (listify dimensions))
         (r (typecase data
              (array (validate-arr-dimensions data dimensions))
              (sequence (validate-seq-dimensions data dimensions))
              (otherwise nil))))
    (when (and r (every #'identity r)) r)))

(defun+ validate-arr-dimensions (data dimensions)
  (equal (array-dimensions data)
         dimensions))

(defun+ validate-seq-dimensions (data dimensions &optional (orig-dim dimensions) accum)
  (if (null dimensions)
      (reverse accum)
      (typecase data
        (sequence
         (let* ((f (first dimensions))
                (data-len (length data))
                (d (when (= f data-len) f)))
           (validate-seq-dimensions (when (> data-len 0)
                                      (elt data 0))
                                    (rest dimensions)
                                    orig-dim
                                    (cons d accum))))
        (otherwise nil))))

;;------------------------------------------------------------

(defun+ c-array-byte-size (c-array)
  (%gl-calc-byte-size (c-array-element-byte-size c-array)
                      (c-array-dimensions c-array)))

(defun+ %gl-calc-byte-size (elem-size dimensions)
  (let* ((row-length (last1 dimensions))
         (row-byte-size (* row-length elem-size)))
    (values (* (reduce #'* dimensions) elem-size)
            row-byte-size)))

(defun+ gl-calc-byte-size (type dimensions)
  (%gl-calc-byte-size (gl-type-size type) (listify dimensions)))
