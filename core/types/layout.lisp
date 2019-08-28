(in-package :%cepl.types)

;; In this file we have code to handle the standard glsl layouts

;; {TODO} automatic handling of sampler types (for bindless textures support)

;;----------------------------------------------------------------------

(defvar *valid-layout-specifiers*
  '(std-140 std-430))

;;----------------------------------------------------------------------

(defclass gl-layout ()
  ((name :initarg :name
         :initform nil
         :reader layout-name)
   (varjo-type :initarg :type
               :initform nil
               :reader layout-varjo-type)
   (base-offset :initarg :base-offset
                :initform nil
                :reader layout-base-offset)
   (base-alignment :initarg :base-alignment
                   :initform nil
                   :reader layout-base-alignment)
   (aligned-offset :initarg :aligned-offset
                   :initform nil
                   :reader layout-aligned-offset)
   ;; includes padding
   (machine-unit-size :initarg :machine-unit-size
                      :initform nil
                      :reader layout-machine-unit-size)
   (members :initarg :members
            :initform nil
            :reader layout-members)
   (element-layout :initarg :element-layout
                   :initform nil
                   :reader layout-element-layout)))

(defclass std-140 (gl-layout) ())
(defclass std-430 (gl-layout) ())


(defmethod make-load-form ((layout gl-layout) &optional environment)
  (declare (ignore environment))
  (with-slots (name
               varjo-type
               base-offset
               base-alignment
               aligned-offset
               machine-unit-size
               members
               element-layout)
      layout
    `(make-instance ',(class-name (class-of layout))
                    :name ',name
                    :type ',varjo-type
                    :base-offset ',base-offset
                    :base-alignment ',base-alignment
                    :aligned-offset ',aligned-offset
                    :machine-unit-size ',machine-unit-size
                    :members ',members
                    :element-layout ,element-layout)))

;;----------------------------------------------------------------------

(defmethod print-object ((obj gl-layout) stream)
  (print-unreadable-object ((vec3 1f0 2f0 3f0) stream :identity t)
    (format stream "~a ~a"
            (symbol-name (class-name (class-of obj)))
            (type->type-spec (layout-varjo-type obj)))))

;;----------------------------------------------------------------------

(defun process-layout-specifier (layout-specifier)
  (let ((specifier (find layout-specifier *valid-layout-specifiers*
                         :test #'string=)))
    (assert specifier ()
            'invalid-data-layout-specifier
            :specifier layout-specifier
            :valid-specifiers *valid-layout-specifiers*)
    specifier))

(defun calc-block-layout (layout-specifier varjo-struct-type-name)
  (let* ((layout-specifier (process-layout-specifier layout-specifier))
         (type (type-spec->type varjo-struct-type-name))
         (parent-type-aligned-offset 0)
         (last-slot-base-offset nil)
         (last-slot-aligned-offset nil)
         (last-slot-machine-size nil))
    (assert (v-typep type 'v-struct))
    (calc-struct-layout layout-specifier
                        'block
                        parent-type-aligned-offset
                        last-slot-base-offset
                        last-slot-aligned-offset
                        last-slot-machine-size
                        type)))

(defun calc-struct-layout-from-name-type-pairs (layout-specifier
                                                name
                                                name-type-pairs)
  (let* ((layout-specifier (process-layout-specifier layout-specifier))
         (parent-type-aligned-offset 0)
         (last-slot-base-offset nil)
         (last-slot-aligned-offset nil)
         (last-slot-machine-size nil)
         (pairs (loop :for (name type/spec) :in name-type-pairs :collect
                   (let ((type (etypecase type/spec
                                 (v-type type/spec)
                                 (t (type-spec->type type/spec)))))
                     (list name type)))))
    (multiple-value-bind (base-offset
                          base-alignment
                          aligned-offset
                          machine-unit-size
                          members)
        (calc-struct-member-layout layout-specifier
                                   pairs
                                   parent-type-aligned-offset
                                   last-slot-base-offset
                                   last-slot-aligned-offset
                                   last-slot-machine-size)
      (make-instance
       layout-specifier
       :name name
       :type (type-spec->type t)
       :base-offset base-offset
       :base-alignment base-alignment
       :aligned-offset aligned-offset
       :machine-unit-size machine-unit-size
       :members members))))

(defun calc-layout (layout-specifier
                    name
                    parent-type-base-offset
                    parent-type-aligned-offset
                    last-slot-base-offset
                    last-slot-aligned-offset
                    last-slot-machine-size
                    type)
  (cond
    ((scalar-type-p type)
     (calc-scalar-layout layout-specifier
                         name
                         parent-type-aligned-offset
                         last-slot-base-offset
                         last-slot-aligned-offset
                         last-slot-machine-size
                         type))
    ((v-typep type 'v-vector)
     (calc-vector-layout layout-specifier
                         name
                         parent-type-aligned-offset
                         last-slot-base-offset
                         last-slot-aligned-offset
                         last-slot-machine-size
                         type))
    ((v-typep type 'v-matrix)
     (calc-col-mat-layout layout-specifier
                          name
                          parent-type-base-offset
                          parent-type-aligned-offset
                          last-slot-base-offset
                          last-slot-aligned-offset
                          last-slot-machine-size
                          type))
    ((v-typep type 'v-struct)
     (calc-struct-layout layout-specifier
                         name
                         parent-type-aligned-offset
                         last-slot-base-offset
                         last-slot-aligned-offset
                         last-slot-machine-size
                         type))
    ((and (v-typep type 'v-array)
          (v-typep (v-element-type type) 'v-matrix))
     (calc-array-of-col-mat-layout layout-specifier
                                   name
                                   parent-type-base-offset
                                   parent-type-aligned-offset
                                   last-slot-base-offset
                                   last-slot-aligned-offset
                                   last-slot-machine-size
                                   type))
    ((and (v-typep type 'v-array)
          (v-typep (v-element-type type) 'v-struct))
     (calc-array-of-structs-layout layout-specifier
                                   name
                                   parent-type-base-offset
                                   parent-type-aligned-offset
                                   last-slot-base-offset
                                   last-slot-aligned-offset
                                   last-slot-machine-size
                                   type))

    ((v-typep type 'v-array)
     (calc-array-of-scalar-or-vectors-layout layout-specifier
                                             name
                                             parent-type-base-offset
                                             parent-type-aligned-offset
                                             last-slot-base-offset
                                             last-slot-aligned-offset
                                             last-slot-machine-size
                                             type))
    ;;
    (t (error 'could-not-layout-type :type type))))

(defun calc-scalar-layout (layout-specifier
                           name
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
     layout-specifier
     :name name
     :type type
     :base-offset base-offset
     :aligned-offset (calc-aligned-offset base-offset n)
     :base-alignment n
     :machine-unit-size n)))

(defun calc-struct-layout (layout-specifier
                           name
                           parent-type-aligned-offset
                           last-slot-base-offset
                           last-slot-aligned-offset
                           last-slot-machine-size
                           type)
  (multiple-value-bind (base-offset
                        base-alignment
                        aligned-offset
                        machine-unit-size
                        members)
      (calc-struct-member-layout layout-specifier
                                 (varjo.internals:v-slots type)
                                 parent-type-aligned-offset
                                 last-slot-base-offset
                                 last-slot-aligned-offset
                                 last-slot-machine-size)
    (make-instance
     layout-specifier
     :name name
     :type type
     :base-offset base-offset
     :base-alignment base-alignment
     :aligned-offset aligned-offset
     :machine-unit-size machine-unit-size
     :members members)))

(defun calc-struct-member-layout (layout-specifier
                                  member-name-type-pairs
                                  parent-type-aligned-offset
                                  last-slot-base-offset
                                  last-slot-aligned-offset
                                  last-slot-machine-size)
  ;; This is seperate from calc-struct-layout so that it can more
  ;; easily be used by defstruct-g which has the names and types
  ;; of the slots but has not yet got a type for the struct itself
  ;;
  ;; The members of a top-level uniform block are laid out in buffer
  ;; storage by treating the uniform block as a structure with a base
  ;; offset of zero.
  ;;
  ;; Slots in memory will be in same order as in definition.
  ;;
  ;; 9. If the member is a structure, the base alignment of the
  ;; structure is N , where N is the largest base alignment value of any
  ;; of its members, and rounded up to the base alignment of a vec4.
  ;; The structure may have padding at the end; the base offset of
  ;; the member following the sub-structure is rounded up to the
  ;; next multiple of the base alignment of the structure.
  ;;
  ;; When using the std-430 storage layout, shader storage blocks will be
  ;; laid out in buffer storage identically to uniform and shader
  ;; storage blocks using the std-140 layout, except that the base
  ;; alignment and stride of arrays of scalars and vectors in rule 4 and
  ;; of structures in rule 9 are not rounded up a multiple of the base
  ;; alignment of a vec4.
  (let* ((base-offset (calc-base-offset parent-type-aligned-offset
                                        last-slot-base-offset
                                        last-slot-aligned-offset
                                        last-slot-machine-size))
         (members (let ((aligned-offset parent-type-aligned-offset)
                        (last-slot-base-offset nil)
                        (last-slot-aligned-offset nil)
                        (last-slot-machine-size nil))
                    (loop :for (name stype) :in member-name-type-pairs :collect
                       (let* ((layout (calc-layout layout-specifier
                                                   name
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
         (max-member-base-alignment
          (reduce #'max (mapcar #'layout-base-alignment members)))
         (base-alignment
          (ecase layout-specifier
            (std-140
             (round-to-next-multiple
              max-member-base-alignment
              (calc-vector-base-alignment (type-spec->type :vec4))))
            (std-430
             max-member-base-alignment)))
         (size (round-to-next-multiple
                (reduce #'+ (mapcar #'layout-machine-unit-size members))
                base-alignment)))
    (values base-offset
            base-alignment
            (calc-aligned-offset base-offset base-alignment)
            size
            members)))

(defun calc-vector-layout (layout-specifier
                           name
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
     layout-specifier
     :name name
     :type type
     :base-offset base-offset
     :base-alignment base-alignment
     :aligned-offset (calc-aligned-offset base-offset base-alignment)
     :machine-unit-size (machine-unit-size type))))

(defun calc-array-of-col-mat-layout (layout-specifier
                                     name
                                     parent-type-base-offset
                                     parent-type-aligned-offset
                                     last-slot-base-offset
                                     last-slot-aligned-offset
                                     last-slot-machine-size
                                     type)
  ;; 6. If the member is an array of S column-major matrices with C
  ;; columns and R rows, the matrix is stored identically to a row of
  ;; S × C column vectors with R components each, according to rule (4).
  (let ((len (first (v-dimensions type)))
        (elem-type (v-element-type type)))
    (destructuring-bind (rows cols) (v-dimensions elem-type)
      (let* ((new-elem-spec (ecase= rows
                              (2 :vec2)
                              (3 :vec3)
                              (4 :vec4)))
             (equiv-array-type-spec (list new-elem-spec (* cols len)))
             (equiv-array-type (type-spec->type equiv-array-type-spec))
             (equiv-layout (calc-layout layout-specifier
                                        name
                                        parent-type-base-offset
                                        parent-type-aligned-offset
                                        last-slot-base-offset
                                        last-slot-aligned-offset
                                        last-slot-machine-size
                                        equiv-array-type)))
        (with-slots (varjo-type) equiv-layout
          (setf varjo-type type))
        equiv-layout))))

(defun calc-col-mat-layout (layout-specifier
                            name
                            parent-type-base-offset
                            parent-type-aligned-offset
                            last-slot-base-offset
                            last-slot-aligned-offset
                            last-slot-machine-size
                            type)
  ;; 5. If the member is a column-major matrix with C columns and R
  ;; rows, the matrix is stored identically to an array of C column
  ;; vectors with R components each, according to rule (4).
  (let* ((array-type-spec
          (typecase type
            (v-mat2 '(v-vec2 2))
            (v-mat3 '(v-vec3 3))
            (v-mat4 '(v-vec4 4))
            (v-dmat2 '(v-dvec2 2))
            (v-dmat3 '(v-dvec3 3))
            (v-dmat4 '(v-dvec4 4))
            (t (error 'could-not-layout-type :type type))))
         (array-type (type-spec->type array-type-spec)))
    (calc-layout layout-specifier
                 name
                 parent-type-base-offset
                 parent-type-aligned-offset
                 last-slot-base-offset
                 last-slot-aligned-offset
                 last-slot-machine-size
                 array-type)))

(defun calc-array-of-structs-layout (layout-specifier
                                     name
                                     parent-type-base-offset
                                     parent-type-aligned-offset
                                     last-slot-base-offset
                                     last-slot-aligned-offset
                                     last-slot-machine-size
                                     type)
  ;; 10. If the member is an array of S structures, the S elements of
  ;; the array are laid out in order, according to rule (9).
  (let ((elem-layout (calc-layout layout-specifier
                                  (list name '*)
                                  parent-type-base-offset
                                  parent-type-aligned-offset
                                  last-slot-base-offset
                                  last-slot-aligned-offset
                                  last-slot-machine-size
                                  (v-element-type type))))
    (make-instance
     layout-specifier
     :name name
     :type type
     :base-offset (layout-base-offset elem-layout)
     :base-alignment (layout-base-alignment elem-layout)
     :aligned-offset (layout-aligned-offset elem-layout)
     :machine-unit-size (* (slot-value elem-layout 'machine-unit-size)
                           (first (v-dimensions type)))
     :element-layout elem-layout)))

(defun calc-array-of-scalar-or-vectors-layout (layout-specifier
                                               name
                                               parent-type-base-offset
                                               parent-type-aligned-offset
                                               last-slot-base-offset
                                               last-slot-aligned-offset
                                               last-slot-machine-size
                                               type)
  ;; 4. If the member is an array of scalars or vectors, the base
  ;;    alignment and array stride are set to match the base alignment of
  ;;    a single array element, according to rules (1), (2), and (3), and
  ;;    rounded up to the base alignment of a vec4.
  ;;    The array may have padding at the end; the base offset of the member
  ;;    following the array is rounded up to the next multiple of the base
  ;;    alignment.
  ;;
  ;; When using the std-430 storage layout, shader storage blocks will be
  ;; laid out in buffer storage identically to uniform and shader
  ;; storage blocks using the std-140 layout, except that the base
  ;; alignment and stride of arrays of scalars and vectors in rule 4 and
  ;; of structures in rule 9 are not rounded up a multiple of the base
  ;; alignment of a vec4.
  (let* ((base-offset (calc-base-offset parent-type-aligned-offset
                                        last-slot-base-offset
                                        last-slot-aligned-offset
                                        last-slot-machine-size))
         (elem-layout (calc-layout layout-specifier
                                   (list name '*)
                                   parent-type-base-offset
                                   parent-type-aligned-offset
                                   last-slot-base-offset
                                   last-slot-aligned-offset
                                   last-slot-machine-size
                                   (v-element-type type)))
         (base-alignment (ecase layout-specifier
                           (std-140 (round-to-next-multiple
                                     (layout-base-alignment elem-layout)
                                     (calc-vector-base-alignment
                                      (type-spec->type :vec4))))
                           (std-430 (layout-base-alignment elem-layout))))
         (size (round-to-next-multiple
                (machine-unit-size type base-alignment)
                base-alignment)))
    (setf (slot-value elem-layout 'machine-unit-size)
          base-alignment)
    (make-instance
     layout-specifier
     :name name
     :type type
     :element-layout elem-layout
     :base-offset base-offset
     :aligned-offset (calc-aligned-offset base-offset base-alignment)
     :base-alignment base-alignment
     :machine-unit-size size)))

;;----------------------------------------------------------------------
;; Common

(defun machine-unit-size (type &optional stride)
  (if (typep type 'v-array)
      (progn
        (assert stride)
        (* (round-to-next-multiple (machine-unit-size (v-element-type type))
                                   stride)
           (reduce #'* (v-dimensions type))))
      (let* ((spec (type->type-spec type))
             (spec (or (first (find spec varjo.internals::*type-shorthand*
                                    :key #'cdr))
                       spec)))
        (cffi:foreign-type-size spec))))

(defun round-to-next-multiple (val multiple)
  (* (ceiling val multiple) multiple))

(defun scalar-type-p (type)
  (or (v-typep type 'v-real)
      (v-typep type 'v-bool)))

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
      (+ last-slot-aligned-offset
         last-slot-machine-size)))

(defun calc-vector-base-alignment (type)
  ;; 2. If the member is a two or four-component vector with components
  ;;    consuming N basic machine units, the base alignment is 2N or 4N,
  ;;    respectively.
  ;; 3. If the member is a three-component vector with components
  ;;    consuming N basic machine units, the base alignment is 4N.
  (let ((elem-type (v-element-type type)))
    (ecase= (first (v-dimensions type))
      (2 (* 2 (machine-unit-size elem-type)))
      (3 (* 4 (machine-unit-size elem-type))) ;; ← (* 4 …) is not a typo
      (4 (* 4 (machine-unit-size elem-type))))))

;;----------------------------------------------------------------------
