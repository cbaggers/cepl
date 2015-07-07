;; make-c-array
;; make-gpu-array

;; make from nested lists
;; make from nested arrays
;; make from multidimensional arrays

;; dimensions optional

;; ok so if they are provided

;; validate them unless no-validate flag is t

;; validate means:
;; if array
;;  - compare dimensions filling out pattern where :?
;; if list or vector
;;  - walk down and check sized (or get size if pattern is :?)
;;  - validate returns the correct dimensions OR nil

;; if result of validate is null then error else it is the dimensions

;; c-populate:
;; This also needs updating to assume that if array and not sequence
;; then loop over dimensions

 
(defun validate-dimensions (data dimensions)
  (let* ((dimensions (if (listp dimensions) dimensions (list dimensions)))
         (r (typecase data
              (sequence (validate-seq-dimensions data dimensions))
              (array (validate-arr-dimensions data dimensions))
              (otherwise nil))))
    (when (and r (every #'identity r)) r)))

(defun validate-arr-dimensions (data dimensions)
  (let* ((actual-dims (array-dimensions data)))
    (if (= (length actual-dims) (length dimensions))
        (mapcar (lambda (d a) (if (eq d :?) a (when (= d a) d)))
                dimensions
                actual-dims)
        nil)))

(defun validate-seq-dimensions (data dimensions &optional (orig-dim dimensions) accum)
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


(defun rm-index-to-coords (index subscripts)
  (let ((cur index))
    (loop :for s :in subscripts :collect
       (multiple-value-bind (x rem) (floor cur (car subscripts))           
         (setq cur x)
         rem))))

