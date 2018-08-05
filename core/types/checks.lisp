(in-package :%cepl.types)

(defn check-array-types-for-copy (src-elem-type
                                  dst-elem-type)
    null
  (assert (eq src-elem-type dst-elem-type) ()
          "CEPL: Array types must match to transfer data
Source array element type:      ~a
Destination array element type: ~a"
          src-elem-type
          dst-elem-type)
  nil)

(defn check-array-sizes-for-copy (src-dims
                                  dst-dims)
    null
  (unless (if (= 1 (length dst-dims) (length src-dims))
              (<= (first src-dims) (first dst-dims))
              (equal src-dims dst-dims))
    (error "CEPL: If the arrays are 1D then the length of the source array must
be <= length of the destination array. If the arrays have more than 1
dimension then their sizes must match exactly"))
  nil)
