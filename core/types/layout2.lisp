(in-package :%cepl.types)

;; In this file we have code to handle the standard glsl layouts

;; {TODO} automatic handling of sampler types (for bindless textures support)

;;----------------------------------------------------------------------
;; Std140
;;
;; - slots in memory will be in same order as in definition

(defclass gl-layout ()
  ((name :initarg :name
         :initform nil
         :reader layout-name)
   (base-offset :initarg :base-offset
                :initform nil
                :reader layout-base-offset)
   (base-alignment :initarg :base-alignment
                   :initform nil
                   :reader layout-base-alignment)
   (aligned-offset :initarg :aligned-offset
                   :initform nil
                   :reader layout-aligned-offset)
   (machine-unit-size :initarg :machine-unit-size
                      :initform nil
                      :reader layout-machine-unit-size)
   (members :initarg :members
            :initform nil
            :reader layout-members)))

(defclass std-140 (gl-layout) ())
(defclass std-430 (gl-layout) ())

(defun calc-block-layout (varjo-struct-type-name)
  (let ((type (type-spec->type varjo-struct-type-name))
        (parent-type-aligned-offset 0)
        (last-slot-base-offset nil)
        (last-slot-aligned-offset nil)
        (last-slot-machine-size nil))
    (assert (v-typep type 'v-struct))
    (calc-struct-layout 'block
                        parent-type-aligned-offset
                        last-slot-base-offset
                        last-slot-aligned-offset
                        last-slot-machine-size
                        type)))

(defun calc-struct-layout (name
                           parent-type-aligned-offset
                           last-slot-base-offset
                           last-slot-aligned-offset
                           last-slot-machine-size
                           type)
  ;; The members of a top-level uniform block are laid out in buffer
  ;; storage by treating the uniform block as a structure with a base
  ;; offset of zero.
  ;; --
  ;; 9. If the member is a structure, the base alignment of the
  ;; structure is N , where N is the largest base alignment value of any
  ;; of its members, and rounded up to the base alignment of a vec4.
  ;; The structure may have padding at the end; the base offset of
  ;; the member following the sub-structure is rounded up to the
  ;; next multiple of the base alignment of the structure.
  (let* ((base-offset (calc-base-offset parent-type-aligned-offset
                                        last-slot-base-offset
                                        last-slot-aligned-offset
                                        last-slot-machine-size))
         (slots (varjo.internals:v-slots type))
         (members (let ((aligned-offset parent-type-aligned-offset)
                        (last-slot-base-offset nil)
                        (last-slot-aligned-offset nil)
                        (last-slot-machine-size nil))
                    (loop :for (name stype) :in slots :collect
                       (let* ((layout (calc-layout name
                                                   base-offset
                                                   aligned-offset
                                                   last-slot-base-offset
                                                   last-slot-aligned-offset
                                                   last-slot-machine-size
                                                   stype)))
                         (with-slots (base-offset
                                      aligned-offset
                                      machine-unit-size)
                             layout
                           (setf last-slot-base-offset base-offset
                                 last-slot-aligned-offset aligned-offset
                                 last-slot-machine-size machine-unit-size))
                         layout))))
         (base-alignment
          (round-to-next-multiple
           (reduce #'max (mapcar #'layout-base-alignment members))
           (calc-vector-base-alignment (type-spec->type :vec4)))))
    (make-instance
     'std-140
     :name name
     :base-offset base-offset
     :base-alignment base-alignment
     :aligned-offset (calc-aligned-offset base-offset base-alignment)
     :machine-unit-size (machine-unit-size type)
     :members members)))

(defun calc-base-offset (parent-type-aligned-offset
                         last-slot-base-offset
                         last-slot-aligned-offset
                         last-slot-machine-size)
  (if (not last-slot-base-offset)
      ;; The base offset of the first member of a structure is taken from the
      ;; aligned offset of the structure itself
      parent-type-aligned-offset
    ;; The base offset of all other structure members is derived by taking
    ;; the offset of the last basic machine unit consumed by the previous
    ;; member and adding one
    ;;
    ;; {TODO} see rule 4 second paragraph, sounds relevent
    ;; {TODO} see rule 9 second paragraph, sounds relevent
    (+ last-slot-aligned-offset ;; which offset?
       last-slot-machine-size
       1)))

(defun calc-scalar-layout (name
                           parent-type-aligned-offset
                           last-slot-base-offset
                           last-slot-aligned-offset
                           last-slot-machine-size
                           type)
  ;; 1. If the member is a scalar consuming N basic machine units, the
  ;;    base alignment is N.
  (let ((n (machine-unit-size type))
        (base-offset (calc-base-offset parent-type-aligned-offset
                                       last-slot-base-offset
                                       last-slot-aligned-offset
                                       last-slot-machine-size)))
    (make-instance
     'std-140
     :name name
     :base-offset base-offset
     :aligned-offset (calc-aligned-offset base-offset n)
     :base-alignment n
     :machine-unit-size n)))

(defun calc-vector-base-alignment (type)
  ;; 2. If the member is a two or four-component vector with components
  ;;    consuming N basic machine units, the base alignment is 2N or 4N,
  ;;    respectively.
  ;; 3. If the member is a three-component vector with components
  ;;    consuming N basic machine units, the base alignment is 4N.
  (ecase= (first (v-dimensions type))
    (2 (* 2 (machine-unit-size type)))
    (3 (* 4 (machine-unit-size type))) ;; ← (* 4 …) is not a typo
    (4 (* 4 (machine-unit-size type)))))

(defun calc-vector-layout (name
                           parent-type-aligned-offset
                           last-slot-base-offset
                           last-slot-aligned-offset
                           last-slot-machine-size
                           type)
  (let ((base-offset
         (calc-base-offset parent-type-aligned-offset
                           last-slot-base-offset
                           last-slot-aligned-offset
                           last-slot-machine-size))
        (base-alignment (calc-vector-base-alignment type)))
    (make-instance
     'std-140
     :name name
     :base-offset base-offset
     :base-alignment base-alignment
     :aligned-offset (calc-aligned-offset base-offset base-alignment)
     :machine-unit-size (machine-unit-size type))))

(defun calc-layout (name
                    parent-type-base-offset
                    parent-type-aligned-offset
                    last-slot-base-offset
                    last-slot-aligned-offset
                    last-slot-machine-size
                    type)
  (cond
    ((scalar-type-p type)
     (calc-scalar-layout name
                         parent-type-aligned-offset
                         last-slot-base-offset
                         last-slot-aligned-offset
                         last-slot-machine-size
                         type))
    ((v-typep type 'v-vector)
     (calc-vector-layout name
                         parent-type-aligned-offset
                         last-slot-base-offset
                         last-slot-aligned-offset
                         last-slot-machine-size
                         type))
    ;; 5. If the member is a column-major matrix with C columns and R
    ;; rows, the matrix is stored identically to an array of C column
    ;; vectors with R components each, according to rule (4).
    ((v-typep type 'v-matrix)
     (let* ((array-type-spec
             (or (assocr type
                         '((v-mat2 . (v-vec2 2))
                           (v-mat3 . (v-vec3 3))
                           (v-mat4 . (v-vec4 4))
                           (v-dmat2 . (v-dvec2 2))
                           (v-dmat3 . (v-dvec3 3))
                           (v-dmat4 . (v-dvec4 4))))
                 (error "woops")))
            (array-type (type-spec->type array-type-spec)))
       (calc-layout name
                    parent-type-base-offset
                    parent-type-aligned-offset
                    last-slot-base-offset
                    last-slot-aligned-offset
                    last-slot-machine-size
                    array-type)))
    ;; 9. If the member is a structure, the base alignment of the
    ;; structure is N , where N is the largest base alignment value of any
    ;; of its members, and rounded up to the base alignment of a vec4.
    ;; The structure may have padding at the end; the base offset of
    ;; the member following the sub-structure is rounded up to the
    ;; next multiple of the base alignment of the structure.
    ((v-typep type 'v-struct)
     'TODO-PADDING
     (calc-struct-layout name
                         parent-type-aligned-offset
                         last-slot-base-offset
                         last-slot-aligned-offset
                         last-slot-machine-size
                         type))
    ;; 6. If the member is an array of S column-major matrices with C
    ;; columns and R rows, the matrix is stored identically to a row of
    ;; S × C column vectors with R components each, according to rule (4).
    ((and (v-typep type 'v-array)
          (v-typep (v-element-type type) 'v-matrix))
     (let ((len (first (v-dimensions type)))
           (elem-type (v-element-type type)))
       (destructuring-bind (rows cols) (v-dimensions elem-type)
         (let* ((new-elem-spec (ecase= rows
                                 (2 :vec2)
                                 (3 :vec3)
                                 (4 :vec4)))
                (equiv-array-type-spec (list new-elem-spec (* cols len)))
                (equiv-array-type (type-spec->type equiv-array-type-spec)))
           (calc-layout name
                        parent-type-base-offset
                        parent-type-aligned-offset
                        last-slot-base-offset
                        last-slot-aligned-offset
                        last-slot-machine-size
                        equiv-array-type)))))
    ;; 10. If the member is an array of S structures, the S elements of
    ;; the array are laid out in order, according to rule (9).
    ((and (v-typep type 'v-array)
          (v-typep (v-element-type type) 'v-struct))
     (let ((elem-layout (calc-layout :dummy
                                     parent-type-base-offset
                                     parent-type-aligned-offset
                                     last-slot-base-offset
                                     last-slot-aligned-offset
                                     last-slot-machine-size
                                     (v-element-type type))))
       (setf (slot-value elem-layout 'machine-unit-size)
             (* (slot-value elem-layout 'machine-unit-size)
                (first (v-dimensions type))))
       elem-layout))
    ;; 4. If the member is an array of scalars or vectors, the base
    ;;    alignment and array stride are set to match the base alignment of
    ;;    a single array element, according to rules (1), (2), and (3), and
    ;;    rounded up to the base alignment of a vec4.
    ;;    The array may have padding at the end; the base offset of the member
    ;;    following the array is rounded up to the next multiple of the base
    ;;    alignment.
    ((v-typep type 'v-array)
     'TODO-PADDING
     (let* ((base-offset (calc-base-offset parent-type-aligned-offset
                                           last-slot-base-offset
                                           last-slot-aligned-offset
                                           last-slot-machine-size))
            (dummy (calc-layout :dummy
                                parent-type-base-offset
                                parent-type-aligned-offset
                                last-slot-base-offset
                                last-slot-aligned-offset
                                last-slot-machine-size
                                (v-element-type type)))
            (base-alignment (round-to-next-multiple
                             (layout-base-alignment dummy)
                             (calc-vector-base-alignment
                              (type-spec->type :vec4)))))
       (make-instance
        'std-140
        :name name
        :base-offset base-offset
        :aligned-offset (calc-aligned-offset base-offset base-alignment)
        :base-alignment base-alignment
        :machine-unit-size (machine-unit-size type))))
    ;;
    (t (error "shiit"))))

(defun calc-aligned-offset (base-offset base-alignment)
  ;; A structure and each structure member have a base offset and a base
  ;; alignment, from which an aligned offset is computed by rounding the
  ;; base offset up to a multiple of the base alignment
  (round-to-next-multiple base-offset base-alignment))



;;----------------------------------------------------------------------
;; Common

(defun machine-unit-size (type)
  (let ((spec (type->type-spec type)))
    (if (listp spec)
        (* (machine-unit-size (first spec))
           (reduce #'* (second spec)))
        (let ((spec (or (first (find spec varjo.internals::*type-shorthand*
                                     :key #'second))
                        spec)))
          (cffi:foreign-type-size spec)))))

(defun round-to-next-multiple (val multiple)
  (* (ceiling val multiple) multiple))

(defun scalar-type-p (type)
  (or (v-typep type 'v-number)
      (v-typep type 'v-bool)))

;;----------------------------------------------------------------------
