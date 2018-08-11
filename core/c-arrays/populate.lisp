(in-package :cepl.c-arrays)

(defn copy-lisp-data-to-c-array ((c-array c-array)
                                 (data (or list array))
                                 &optional
                                 (check-sizes boolean t))
    c-array
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
                        :do (progn
                              (if structp
                                  (populate (row-major-aref-c c-array idx) elem)
                                  (setf (row-major-aref-c c-array idx) elem))
                              (incf idx))))
                 idx))
             (dpop-with-array (data)
               (loop
                  :for i :below (array-total-size data)
                  :for elem := (row-major-aref data i)
                  :do (if structp
                          (populate (row-major-aref-c c-array i) elem)
                          (setf (row-major-aref-c c-array i) elem)))))
      (when check-sizes
        (unless (validate-dimensions data (c-array-dimensions c-array)
                                     structp)
          (error "Dimensions of array differs from that of the data:~%~a~%~a"
                 c-array data)))
      (typecase data
        (array (dpop-with-array data))
        (sequence (walk-to-dpop data
                                (reverse (c-array-dimensions c-array))
                                0)))
      c-array)))

(defun+ rm-index-to-coords (index subscripts)
  (let ((cur index))
    (nreverse
     (loop :for s :in subscripts :collect
        (multiple-value-bind (x rem) (floor cur s)
          (setq cur x)
          rem)))))

(defun+ validate-dimensions (data dimensions struct-elem-type-p)
  (labels ((validate-arr-dimensions (data dimensions)
             (when (equal (array-dimensions data)
                          dimensions)
               dimensions))
           (validate-seq-dimensions (data dimensions)
             (and (equal (length data) (first dimensions))
                  (cond
                    ((rest dimensions)
                     (validate-seq-dimensions (first data) (rest dimensions)))
                    ((listp (first data)) struct-elem-type-p)
                    (t t)))))
    (let* ((dimensions (listify dimensions)))
      (typecase data
        (array (validate-arr-dimensions data (reverse dimensions)))
        (sequence (validate-seq-dimensions data (reverse dimensions)))
        (otherwise nil)))))

;;------------------------------------------------------------

(defun+ c-array-byte-size (c-array)
  (%gl-calc-byte-size (c-array-element-byte-size c-array)
                      (c-array-dimensions c-array)
                      (c-array-row-alignment c-array)))

(defun+ %gl-calc-byte-size (elem-size dimensions row-alignment)
  (let* ((row-consumes (* (first dimensions) elem-size))
         (row-size-with-padding (* (ceiling row-consumes row-alignment)
                                   row-alignment)))
    (* (reduce #'* (rest dimensions))
       row-size-with-padding)))

(defun+ gl-calc-byte-size (type dimensions row-alignment)
  (%gl-calc-byte-size (gl-type-size type)
                      (listify dimensions)
                      row-alignment))
