(in-package :%cepl.types)

;; In this file we have code to handle the standard glsl layouts

;; {TODO} automatic handling of sampler types (for bindless textures support)

;;----------------------------------------------------------------------
;; Std140
;;
;; - slots in memory will be in same order as in definition

(defun calc-block-layout (varjo-struct-type-name)
  (let ((type (type-spec->type varjo-struct-type-name))
        (base-offset 0)
        (aligned-offset 0))
    (assert (v-typep type 'v-struct))
    (calc-struct-layout varjo-struct-type-name
                        base-offset
                        aligned-offset)))

(defun calc-struct-layout (name
                           base-offset
                           aligned-offset)
  ;; The members of a top-level uniform block are laid out in buffer
  ;; storage by treating the uniform block as a structure with a base
  ;; offset of zero.
  (let* ((slots (varjo.internals:v-slots
                 (varjo:type-spec->type 'cepl:g-pnt)))
         (members (let ((last-slot-base-offset nil)
                        (last-slot-aligned-offset nil)
                        (last-slot-machine-size nil))
                    (loop :for (name stype) :in slots :collect
                       (let* ((layout (calc-layout base-offset
                                                   aligned-offset
                                                   last-slot-base-offset
                                                   last-slot-aligned-offset
                                                   last-slot-machine-size
                                                   stype)))
                         (destructuring-bind
                               (name
                                &key base-offset aligned-offset
                                machine-unit-size members)
                             layout
                           (declare (ignore name members))
                           (setf last-slot-base-offset base-offset
                                 last-slot-aligned-offset aligned-offset
                                 last-slot-machine-size machine-unit-size))
                         layout)))))
    (list name
          :base-offset
          :aligned-offset 0
          :machine-unit-size :TODO
          :members members)))

(defun calc-layout (parent-type-base-offset
                    parent-type-aligned-offset
                    last-slot-base-offset
                    last-slot-aligned-offset
                    last-slot-machine-size
                    type)
  (let ((base-offset (calc-base-offset parent-type-base-offset
                                       last-slot-base-offset
                                       last-slot-aligned-offset
                                       last-slot-machine-size))
        ;; {TODO} needs the aligned-offset
        ;; (aligned-offset
        ;;  (calc-aligned-offset slot-base-offset
        ;;                       slot-aligned-offset))
        (slot-machine-size &&&))
    (cond
      ;; 1. If the member is a scalar consuming N basic machine units, the
      ;;    base alignment is N.
      ((scalar-type-p type) (machine-unit-size type))
      ;; 2. If the member is a two or four-component vector with components
      ;;    consuming N basic machine units, the base alignment is 2N or 4N,
      ;;    respectively.
      ;; 3. If the member is a three-component vector with components
      ;;    consuming N basic machine units, the base alignment is 4N .
      ((v-typep type 'v-vector)
       (ecase= (first (v-dimensions type))
         (2 (* 2 (machine-unit-size type)))
         (3 (* 4 (machine-unit-size type))) ;; ← (* 4 …) is not a typo
         (4 (* 4 (machine-unit-size type)))))
      ;; 5. If the member is a column-major matrix with C columns and R
      ;; rows, the matrix is stored identically to an array of C column
      ;; vectors with R components each, according to rule (4).
      ((v-typep type 'v-matrix)
       (let* ((array-type-spec
               (or (assocr (type->type-spec type)
                           '((v-mat2 . (v-vec2 2))
                             (v-mat3 . (v-vec3 3))
                             (v-mat4 . (v-vec4 4))
                             (v-dmat2 . (v-dvec2 2))
                             (v-dmat3 . (v-dvec3 3))
                             (v-dmat4 . (v-dvec4 4))))
                   (error "woops")))
              (array-type (type-spec->type array-type-spec)))
         (calc-base-alignment parent-type-base-offset
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
       (let ((layout (calc-struct-layout :??????
                                         base-offset
                                         aligned-offset)))
         (destructuring-bind (name
                              &key base-offset aligned-offset machine-unit-size
                              members)
             layout
           (declare (ignore name base-offset aligned-offset))
           members))
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
      (t (error "shiit")))))

(defun calc-aligned-offset (base-offset base-alignment)
  ;; A structure and each structure member have a base offset and a base
  ;; alignment, from which an aligned offset is computed by rounding the
  ;; base offset up to a multiple of the base alignment
  (round-to-next-multiple base-offset base-alignment))

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

;;----------------------------------------------------------------------
;; Common

(defun machine-unit-size (name)
  (cffi:foreign-type-size name))

(defun round-to-next-multiple (val multiple)
  (* (ceiling val multiple) multiple))

(defun scalar-type-p (type)
  (or (v-typep type 'v-number)
      (v-typep type 'v-bool)))

;;----------------------------------------------------------------------
