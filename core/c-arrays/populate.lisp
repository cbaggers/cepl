(in-package :cepl.c-arrays)

(defun+ c-populate (c-array data &optional (check-sizes t))
  (labels ((walk-to-dpop (data dimensions &optional structp pos)
             (let ((still-to-walk (rest dimensions)))
               (map nil (lambda (sublist i)
                          (if still-to-walk
                              (walk-to-dpop sublist still-to-walk structp (cons i pos))
                              (if structp
                                  (populate (aref-c* c-array (reverse (cons i pos)))
                                            sublist)
                                  (setf (aref-c* c-array (reverse (cons i pos)))
                                        sublist))))
                    data (range (length data)))))
           (dpop-with-array (data dimensions &optional structp)
             (loop :for i :below (array-total-size data)
                :for coords = (rm-index-to-coords i dimensions) :do
                (if structp
                    (populate (aref-c* c-array coords) (row-major-aref data i))
                    (setf (aref-c* c-array coords) (row-major-aref data i))))))
    (when check-sizes
      (unless (validate-dimensions data (c-array-dimensions c-array))
        (error "Dimensions of array differs from that of the data:~%~a~%~a"
               c-array data)))
    (typecase data
      (sequence (walk-to-dpop data (c-array-dimensions c-array)
                              (c-array-struct-element-typep c-array)))
      (array (dpop-with-array data (c-array-dimensions c-array)
                              (c-array-struct-element-typep c-array))))
    c-array))

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
              (sequence (validate-seq-dimensions data dimensions))
              (array (validate-arr-dimensions data dimensions))
              (otherwise nil))))
    (when (and r (every #'identity r)) r)))

(defun+ validate-arr-dimensions (data dimensions)
  (let* ((actual-dims (array-dimensions data)))
    (if (= (length actual-dims) (length dimensions))
        (mapcar (lambda (d a) (if (eq d :?) a (when (= d a) d)))
                dimensions
                actual-dims)
        nil)))

(defun+ validate-seq-dimensions (data dimensions &optional (orig-dim dimensions) accum)
  (if (null dimensions)
      (reverse accum)
      (typecase data
        (sequence
         (let* ((f (first dimensions))
                (data-len (length data))
                (d (if (eq :? f) data-len (when (= f data-len) f))))
           (validate-seq-dimensions
            (when (> data-len 0)
              (elt data 0)) (rest dimensions) orig-dim
              (cons d accum))))
        (otherwise nil))))

;;------------------------------------------------------------

(defun+ c-array-byte-size (c-array)
  (%gl-calc-byte-size (c-array-element-byte-size c-array)
                      (c-array-dimensions c-array)))

(defun+ %gl-calc-byte-size (elem-size dimensions)
  (let* ((x-size (first dimensions)) (rest (rest dimensions))
         (row-byte-size (* x-size elem-size)))
    (values (* row-byte-size (max (reduce #'* rest) 1))
            row-byte-size)))

(defun+ gl-calc-byte-size (type dimensions)
  (%gl-calc-byte-size (gl-type-size type) (listify dimensions)))
