(in-package :%cepl.types)

;; In this file we have code to handle the standard glsl layouts

;;----------------------------------------------------------------------
;; Std140

;; - slots in memory will be in same order as in definition
;; -

(deftype gl-basic-machine-unit () '(unsigned-byte 8))

(defun machine-unit-size (type)
  (cffi:foreign-type-size type))

(defun aligned-offset (type)
  ;; A structure and each structure member have a base offset and a base
  ;; alignment, from which an aligned offset is computed by rounding the
  ;; base offset up to a multiple of the base alignment
  (with-slots (base-offset base-alignment) type
    (round-to-next-multiple base-offset base-alignment)))


(defun base-offset-of-child (struct-type nth)
  (if (= nth 0)
      ;; The base offset of the first member of a structure is taken from the
      ;; aligned offset of the structure itself
      (aligned-offset struct-type)
      ;; The base offset of all other structure members is derived by taking
      ;; the offset of the last basic machine unit consumed by the previous
      ;; member and adding one
      ;;
      ;; {TODO} see rule 4 second paragraph, sounds relevent
      ;; {TODO} see rule 9 second paragraph, sounds relevent
      (+ (aligned-offset struct-type (- nth 1)) ;; which offset?
         (machine-unit-size (child (- nth 1)))
         1)))

(defun populate-type-tree-for-std140 (type)
  ;; The members of a top-level uniform block are laid out in buffer
  ;; storage by treating the uniform block as a structure with a base
  ;; offset of zero.
  (setf (base-offset node) 0)
  (loop :for child :in (children type) :do
     (blah .......))
  node)

(defun base-alignment (type)
  (cond
    ;; 1. If the member is a scalar consuming N basic machine units, the
    ;;    base alignment is N.
    ((scalar-type-p type) (machine-unit-size type))
    ;; 2. If the member is a two or four-component vector with components
    ;;    consuming N basic machine units, the base alignment is 2N or 4N,
    ;;    respectively.
    ;; 3. If the member is a three-component vector with components
    ;;    consuming N basic machine units, the base alignment is 4N .
    ((vector-type-p type)
     (case= (component-length type)
       (2 (* 2 (base-alignment (element-type type))))
       (3 (* 4 (base-alignment (element-type type)))) ;; 4 is not a typo
       (4 (* 4 (base-alignment (element-type type))))))
    ;; 5. If the member is a column-major matrix with C columns and R
    ;; rows, the matrix is stored identically to an array of C column
    ;; vectors with R components each, according to rule (4).
    ((matrix-type-p type)
     (base-alignment (:array (:vec (rows type)) (cols type))))
    ;; 9. If the member is a structure, the base alignment of the
    ;; structure is N , where N is the largest base alignment value of any
    ;; of its members, and rounded up to the base alignment of a vec4.
    ;; The structure may have padding at the end; the base offset of
    ;; the member following the sub-structure is rounded up to the
    ;; next multiple of the base alignment of the structure.
    ((struct-type-p type)
     (round-to-next-multiple
      (reduce max (mapcar #'base-alignment (children type)))
      (base-alignment :vec4)))
    ;; 4. If the member is an array of scalars or vectors, the base
    ;;    alignment and array stride are set to match the base alignment of
    ;;    a single array element, according to rules (1), (2), and (3), and
    ;;    rounded up to the base alignment of a vec4.
    ;;    The array may have padding at the end; the base offset of the member
    ;;    following the array is rounded up to the next multiple of the base
    ;;    alignment.
    ((and (array-type-p type)
          (or (scalar-type-p (element-type type))
              (vector-type-p (element-type type))))
     (round-to-next-multiple (base-alignment (element-type type))
                             (base-alignment :vec4)))
    ;; 6. If the member is an array of S column-major matrices with C
    ;; columns and R rows, the matrix is stored identically to a row of
    ;; S × C column vectors with R components each, according to rule (4).
    ((and (array-type-p type)
          (matrix-type-p (element-type type)))
     todo)
    ;; 10. If the member is an array of S structures, the S elements of
    ;; the array are laid out in order, according to rule (9).
    todo))

(defun array-stride (type)
  (assert (array-type-p type))
  (if (or (scalar-type-p (element-type type))
          (vector-type-p (element-type type)))
      ;; 4. If the member is an array of scalars or vectors, the base
      ;;    alignment and array stride are set to match the base alignment of
      ;;    a single array element, according to rules (1), (2), and (3), and
      ;;    rounded up to the base alignment of a vec4.
      ;;    The array may have padding at the end; the base offset of the member
      ;;    following the array is rounded up to the next multiple of the base
      ;;    alignment.
      (round-to-next-multiple (base-alignment (element-type type))
                              (base-alignment :vec4))))


;;- - - - - - -
;; Ignored

;; 7. If the member is a row-major matrix with C columns and R rows,
;; the matrix is stored identically to an array of R row vectors with
;; C components each, according to rule (4).
;;
;; 8. If the member is an array of S row-major matrices with C columns
;; and R rows, the matrix is stored identically to a row of S × R row
;; vectors with C components each, according to rule (4).

;;----------------------------------------------------------------------

(defun round-to-next-multiple (val multiple)
  (* (ceiling val multiple) multiple))
