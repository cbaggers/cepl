(in-package :%cepl.types)

;; In this file we have code to handle the standard glsl layouts

;; {TODO} automatic handling of sampler types (for bindless textures support)

;;----------------------------------------------------------------------
;; Std140
;;
;; - slots in memory will be in same order as in definition

(defun machine-unit-size (self)
  (if (slot-boundp self 'machine-unit-size)
      (slot-boundp self 'machine-unit-size)
      (setf (slot-value self 'machine-unit-size)
            (cffi:foreign-type-size (type self)))))

;;
;; Aligned Offset

(defun aligned-offset (parent previous-sibling self)
  (if (slot-boundp self 'aligned-offset)
      (slot-value self 'aligned-offset)
      (setf (slot-value self 'aligned-offset)
            (calc-aligned-offset parent previous-sibling self))))

(defun calc-aligned-offset (parent previous-sibling self)
  ;; A structure and each structure member have a base offset and a base
  ;; alignment, from which an aligned offset is computed by rounding the
  ;; base offset up to a multiple of the base alignment
  (round-to-next-multiple (base-offset parent previous-sibling self)
                          (base-alignment parent previous-sibling self)))

;;
;; Base Offset

(defun base-offset (parent previous-sibling self)
  (if (slot-boundp self 'base-offset)
      (slot-value self 'base-offset)
      (setf (slot-value self 'base-offset)
            (calc-base-offset parent previous-sibling self))))

(defun calc-base-offset (parent previous-sibling self)
  (if (not previous-sibling)
      ;; The base offset of the first member of a structure is taken from the
      ;; aligned offset of the structure itself
      (aligned-offset parent)
      ;; The base offset of all other structure members is derived by taking
      ;; the offset of the last basic machine unit consumed by the previous
      ;; member and adding one
      ;;
      ;; {TODO} see rule 4 second paragraph, sounds relevent
      ;; {TODO} see rule 9 second paragraph, sounds relevent
      (+ (aligned-offset parent :nope previous-sibling) ;; which offset?
         (machine-unit-size previous-sibling)
         1)))

;;
;; Base Alignment

(defun base-alignment (parent previous-sibling self)
  (if (slot-boundp self 'base-alignment)
      (slot-value self 'base-alignment)
      (setf (slot-value self 'base-alignment)
            (calc-base-alignment parent previous-sibling self))))

(defun calc-base-alignment (parent previous-sibling self)
  (cond
    ;; 1. If the member is a scalar consuming N basic machine units, the
    ;;    base alignment is N.
    ((scalar-type-p self) (machine-unit-size self))
    ;; 2. If the member is a two or four-component vector with components
    ;;    consuming N basic machine units, the base alignment is 2N or 4N,
    ;;    respectively.
    ;; 3. If the member is a three-component vector with components
    ;;    consuming N basic machine units, the base alignment is 4N .
    ((vector-type-p self)
     (case= (component-length self)
            (2 (* 2 (base-alignment
                     parent previous-sibling (element-type self))))
            (3 (* 4 (base-alignment ;; ← (* 4 …) is not a typo
                     parent previous-sibling (element-type self))))
            (4 (* 4 (base-alignment
                     parent previous-sibling (element-type self))))))
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
    ((struct-type-p self)
     (round-to-next-multiple
      (reduce #'max (mapcar (lambda (child)
                              ;; {TODO} need to main loop here, will fill
                              ;;        in when I've worked it out
                              (base-alignment self &&&&& child))
                            (children self)))
      (base-alignment :vec4)))
    ;; 4. If the member is an array of scalars or vectors, the base
    ;;    alignment and array stride are set to match the base alignment of
    ;;    a single array element, according to rules (1), (2), and (3), and
    ;;    rounded up to the base alignment of a vec4.
    ;;    The array may have padding at the end; the base offset of the member
    ;;    following the array is rounded up to the next multiple of the base
    ;;    alignment.
    ((and (array-type-p self)
          (or (scalar-type-p (element-type self))
              (vector-type-p (element-type self))))
     (round-to-next-multiple (base-alignment :nope :nope (element-type type))
                             (base-alignment :nope :nope :vec4)))
    ;; 6. If the member is an array of S column-major matrices with C
    ;; columns and R rows, the matrix is stored identically to a row of
    ;; S × C column vectors with R components each, according to rule (4).
    ((and (array-type-p type)
          (matrix-type-p (element-type type)))
     todo)
    ;; 10. If the member is an array of S structures, the S elements of
    ;; the array are laid out in order, according to rule (9).
    ((and (array-type-p type)
          (struct-type-p (element-type type)))
     todo)
    ;;
    (t (error "shiit"))))

;;
;; Array Stride

(defun array-stride (parent previous-sibling self)
  (assert (array-type-p type))
  (if (slot-boundp self 'array-stride)
      (slot-value self 'array-stride)
      (setf (slot-value self 'array-stride)
            (calc-array-stride parent previous-sibling self))))

(defun calc-array-stride (parent previous-sibling self)
  (if (or (scalar-type-p (element-type type))
          (vector-type-p (element-type type)))
      ;; 4. If the member is an array of scalars or vectors, the base
      ;;    alignment and array stride are set to match the base alignment of
      ;;    a single array element, according to rules (1), (2), and (3), and
      ;;    rounded up to the base alignment of a vec4.
      ;;    The array may have padding at the end; the base offset of the member
      ;;    following the array is rounded up to the next multiple of the base
      ;;    alignment.
      (base-alignment parent previous-sibling self)
      ;;
      (machine-unit-size self)))

;;
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

;;----------------------------------------------------------------------

(defclass layout-node ()
  ((type :initarg :type)
   (base-offset :initarg :base-offset)
   (aligned-offset :initarg :aligned-offset)
   (stride :initarg :stride)
   (children :initarg :children)))

;;----------------------------------------------------------------------
