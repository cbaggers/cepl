(in-package :cepl.types)

;;------------------------------------------------------------

(define-const +cpu->gpu-vec-mappings+
    '((:uint8-vec2 . :vec2)
      (:uint8-vec3 . :vec3)
      (:uint8-vec4 . :vec4))
  :type list)

;;------------------------------------------------------------

(defvar *struct-slot-defs* (make-hash-table))

(defun g-struct-slots (name)
  (gethash name *struct-slot-defs*))

(defun (setf g-struct-slots) (slots name)
  (setf (gethash name *struct-slot-defs*)
        slots))

;;------------------------------------------------------------

(defgeneric s-arrayp (object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gl-struct-slot ()
    ((name :initarg :name :reader s-name)
     (type :initarg :type :reader s-type)
     (lisp-type :initarg :lisp-type :reader s-lisp-type)
     (element-type :initarg :element-type :reader s-element-type)
     (dimensions :initarg :dimensions :initform 1 :reader s-dimensions)
     (normalized :initarg :normalized :reader s-normalizedp)
     (accessor :initarg :accessor :reader s-accessor)
     (uses-method-p :initarg :uses-method-p :reader s-uses-method-p)
     (layout :initarg :layout :reader s-layout)
     (parent-ffi-name :initarg :parent-ffi-name :reader s-parent-ffi-name))))

(defmethod make-load-form ((slot gl-struct-slot) &optional environment)
  (declare (ignore environment))
  (with-slots (name
               type
               lisp-type
               element-type
               dimensions
               normalized
               accessor
               uses-method-p
               layout
               parent-ffi-name)
      slot
    `(make-instance 'gl-struct-slot
                    :name ',name
                    :type ',type
                    :lisp-type ',lisp-type
                    :normalized ',normalized
                    :accessor ',accessor
                    :element-type ',element-type
                    :dimensions ',dimensions
                    :layout ,layout
                    :parent-ffi-name ',parent-ffi-name)))

(defmethod v-type-of ((gss gl-struct-slot))
  (if (eq (s-type gss) :array)
      (type-spec->type (list (s-element-type gss) (s-dimensions gss)))
      (type-spec->type (s-type gss))))

(defmethod s-arrayp ((object gl-struct-slot))
  (eq (s-type object) :array))

;;------------------------------------------------------------

(defun+ nest-simple-loops (dimensions index-vars body &optional (loop-op :do))
  (labels ((inner (dimensions index-vars body loop-op)
             (if (or (null dimensions) (null index-vars))
                 body
                 (inner (rest dimensions) (rest index-vars)
                        `(loop :for ,(car index-vars) :below ,(car dimensions)
                            ,loop-op ,body)
                        loop-op))))
    (inner (reverse dimensions) (reverse index-vars) body loop-op)))

(defun+ type->spec (type)
  ;; Well this is janky, I guess this'll break at some point if we don't sort
  ;; it before then.. what is odd is that it seems this is really just for
  ;; getting a spec using the keyword names where it makes sense.
  (let ((spec (type->type-spec type)))
    (if (listp spec)
        `(,(if (core-typep (type-spec->type (first spec)))
               (kwd (subseq (symbol-name (first spec)) 2))
               (first spec))
           ;; When would this ↓↓↓↓-----↓↓↓↓ happen?
           ,(if (and (listp (second spec))
                     (= (length (second spec)) 1))
                (first (second spec))
                (second spec)))
        (if (core-typep type)
            (kwd (subseq (symbol-name spec) 2))
            spec))))

;;------------------------------------------------------------

(defun potential-struct-layout (name layout-specifier slot-descriptions)
  (if (string= layout-specifier :default)
      (n-of nil (length slot-descriptions))
      (layout-members
       (calc-struct-layout-from-name-type-pairs
        layout-specifier
        name
        (mapcar (lambda (x)
                  (dbind (name type &rest rest) x
                    (declare (ignore rest))
                    (list name (type-spec->type type))))
                slot-descriptions)))))

(defmacro defstruct-g (name-and-options &body slot-descriptions)
  (dbind (name &key accesors constructor readers writers populate pull-push
               (attribs t) (layout :default))
      (listify name-and-options)
    (declare (ignore constructor readers writers accesors populate pull-push))
    (let* ((foreign-struct-name (hidden-symb name :foreign))
           (qualified-struct-name `(:struct ,foreign-struct-name))
           (slot-layouts (potential-struct-layout name layout slot-descriptions))
           (slots (mapcar (lambda (d l)
                            (normalize-slot-description qualified-struct-name d l))
                          slot-descriptions
                          slot-layouts))
           (typed-populate (symb :populate- name)))
      (assert (= (length slot-layouts) (length slots)))
      (when (validate-defstruct-g-form name slots)
        (setf (g-struct-slots name) slots)
        `(progn
           (setf (g-struct-slots ',name) ',slots)
           (eval-when (:compile-toplevel :load-toplevel :execute)
             ,(make-varjo-struct-def name slots))
           ,@(make-instance-wrapper-def name
                                        foreign-struct-name
                                        slots
                                        typed-populate)
           ,(when attribs (make-struct-attrib-assigner name slots))
           ,(make-struct-pixel-format name slots)
           ,(make-varjo-struct-lookup name)
           ',name)))))

(defun+ normalize-slot-description (qualified-struct-name
                                    slot-description
                                    slot-layout)
  (destructuring-bind (name type &key normalized accessor) slot-description
    (let* ((type (listify type))
           (dimensions (listify (or (second type) 1)))
           (arrayp (> (first dimensions) 1))
           (element-type (when arrayp (first type)))
           (type (if arrayp :array (first type)))
           (lisp-type
            (if element-type
                `(simple-array
                  ,(or (lisp-equivalent-of-keyword-cffi-type element-type)
                       element-type)
                  ,dimensions)
                (or (lisp-equivalent-of-keyword-cffi-type type)
                    type))))
      (make-instance 'gl-struct-slot
                     :name name
                     :type type
                     :lisp-type lisp-type
                     :normalized normalized
                     :accessor accessor
                     :element-type element-type
                     :dimensions dimensions
                     :layout slot-layout
                     :parent-ffi-name qualified-struct-name))))

;; put all cepl's errors definitions in one place (like varjo)
(defun+ validate-defstruct-g-form (name slots)
  (when (keywordp name) (error "glstruct names cannot be keywords"))
  (when (null slots) (error "glstruct must have at least 1 slot"))
  t)

;;------------------------------------------------------------

(defun+ make-varjo-struct-def (name slots)
  (let ((hidden-name (symb-package (symbol-package name)
                                   'v_ name )))
    `(v-defstruct
         (,name :shadowing ,hidden-name)
         ()
       ,@(mapcar #'format-slot-for-varjo slots))))

;;{TODO} make varjo support readers and writers and then remove this hack
(defun+ format-slot-for-varjo (slot)
  (let ((accessor (s-accessor slot)))
    (if (s-arrayp slot)
        `(,(s-name slot)
           ,(validate-varjo-type-spec
             (list (s-element-type slot) (s-dimensions slot)))
           ,@(when accessor `(:accessor ,accessor)))
        `(,(s-name slot)
           ,(validate-varjo-type-spec (s-type slot))
           ,@(when accessor `(:accessor ,accessor))))))

(defun+ validate-varjo-type-spec (spec)
  (let ((spec (or (cdr (assoc spec +cpu->gpu-vec-mappings+))
                  spec)))
    (type->spec (type-spec->type spec))))

;;------------------------------------------------------------

(defun+ make-varjo-struct-lookup (name)
  `(defmethod cepl.internals:symbol-names-cepl-structp ((sym (eql ',name)))
     t))

;;------------------------------------------------------------

(defclass redefinable-gstruct () ())
(defstruct static-gstruct)

(defmacro define-gstruct-lisp-equiv ((name &key static) &body slots)
  (let* ((pkg (symbol-package name))
         (constructor-name (symb-package pkg :make- name))
         (copy-name (symb-package pkg :copy- name))
         (predicate-name (symb-package pkg name :-p))
         (slot-names (mapcar #'first slots))
         (accessor-names (mapcar (lambda (x) (symb-package pkg name :- x))
                                 slot-names)))
    (if static
        `(defstruct (,name (:include static-gstruct))
           ,@(loop :for (a-name a-type) :in slots :collect
                `(,a-name (error "") :type a-type)))
        `(progn
           (defclass ,name (redefinable-gstruct) ,slot-names)

           ,@(loop :for (a-name a-type) :in slots
                :for accessor-name :in accessor-names
                :append
                `((defn-inline ,accessor-name ((x ,name)) t
                    (slot-value x ',a-name))
                  (defn-inline (setf ,accessor-name) ((val t) (x ,name)) t
                    (setf (slot-value x ',a-name) val))))

           (defn ,constructor-name (&key ,@slot-names) ,name
             #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
             ,@(loop :for (s-name s-type) :in slots
                  :collect `(check-type ,s-name ,s-type))
             (let ((res (make-instance ',name)))
               ,@(loop :for s-name :in slot-names
                    :for a-name :in accessor-names
                    :collect `(setf (,a-name res) ,s-name))
               res))

           (defn ,copy-name ((instance ,name)) ,name
             (let ((res (make-instance ',name)))
               ,@(loop :for s-name :in slot-names
                    :for a-name :in accessor-names
                    :collect `(setf (,a-name res) (,a-name instance)))
               res))

           (defn-inline ,predicate-name (x) boolean
             (typep x ',name))))))

(defun+ make-instance-wrapper-def (name
                                   foreign-struct-name
                                   slots
                                   typed-populate)
  (let* ((constructor-name (symb :make- name))
         (foreign-type-name (hidden-symb name :cffi-ct-type))
         (slot-defs (mapcar #'format-slot-for-cstruct slots))
         ;;
         (from (hidden-symb name :from-foreign))
         (to (hidden-symb name :to-foreign))
         (pkg (symbol-package name))
         (accessor-names (mapcar (lambda (x)
                                   (symb-package pkg name :- (s-name x)))
                                 slots)))
    `((eval-when (:compile-toplevel :load-toplevel :execute)
        (defcstruct ,foreign-struct-name
          ,@slot-defs)

        (define-foreign-type ,foreign-type-name nil nil
                             (:actual-type :struct ,foreign-struct-name)
                             (:simple-parser ,name))

        (define-gstruct-lisp-equiv (,name :static nil)
          ,@(loop :for slot :in slots :collect
               `(,(s-name slot) ,(s-lisp-type slot))))

        (defn-inline ,from ((ptr foreign-pointer)) ,name
          (declare (optimize (speed 3) (safety 0) (debug 0)))
          #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (,constructor-name
           ,@(loop :for slot :in slots
                :collect (kwd (s-name slot))
                :collect (slot-getter 'ptr slot))))

        (defmethod translate-from-foreign (ptr (type ,foreign-type-name))
          #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (,from ptr))

        (defmethod expand-from-foreign (ptr (type ,foreign-type-name))
          (list ',from ptr)))

      (defmethod print-object ((obj ,name) stream)
        (print-unreadable-object (obj stream :type t :identity t)))

      ,@(loop :for slot :in slots
           :for accessor :in accessor-names
           :when (s-accessor slot)
           :append
           `((defmethod ,(s-accessor slot) ((obj ,name))
               (,accessor obj))
             (defmethod (setf ,(s-accessor slot)) (value (obj ,name))
               (setf (,accessor obj) value))))

      (defn ,typed-populate ((ptr foreign-pointer) (data ,name)) ,name
        #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        ,@(loop :for slot :in slots
             :for i :from 0
             :for a-name :in accessor-names
             :collect
             (slot-setter 'ptr `(,a-name data) slot))
        data)

      (defmethod cepl.internals:populate (ptr (data ,name))
        (,typed-populate ptr data))

      (defn-inline ,to ((ptr foreign-pointer) (value ,name)) ,name
        (declare (optimize (speed 3) (safety 0) (debug 0)))
        (,typed-populate ptr value))

      (defmethod get-typed-from-foreign ((type-name (eql ',name)))
        #',from)

      (defmethod get-typed-to-foreign ((type-name (eql ',name)))
        #',to))))

(defun+ format-slot-for-cstruct (slot)
  (if (s-arrayp slot)
      (format-array-slot-for-cstruct slot)
      `(,(s-name slot)
         ,(s-type slot)
         ,@(when (s-layout slot)
                 `(:offset ,(layout-base-offset (s-layout slot)))))))

(defun+ format-array-slot-for-cstruct (slot)
  (when (> (length (s-dimensions slot)) 1)
    (error "Cannot currently support multi dimensional arrays"))
  ;; As we cant encode stride in defcstruct we instead make a byte array of
  ;; the correct size and rely on c-array to handle the stride correctly.
  (if (s-layout slot)
      `(,(s-name slot)
         (:array :uint8 ,(layout-machine-unit-size (s-layout slot)))
         :offset ,(layout-base-offset (s-layout slot)))
      `(,(s-name slot)
         (:array ,(s-element-type slot) ,(reduce #'* (s-dimensions slot))))))

;;------------------------------------------------------------

(defun+ slot-getter (ptr slot &optional
                         return-c-arrays)
  (cond
    ((not (s-arrayp slot)) (make-t-slot-getter ptr slot))
    ((s-arrayp slot) (make-array-slot-getter ptr slot return-c-arrays))
    (t (error "Dont know what to do with slot ~a" slot))))

(defun+ make-t-slot-getter (ptr slot)
  `(foreign-slot-value ,ptr
                       ',(s-parent-ffi-name slot)
                       ',(s-name slot)))

(defun+ make-array-slot-getter (ptr slot return-c-arrays)
  `(locally
       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (let ((carr (cepl.c-arrays::make-c-array-from-pointer
                    ',(s-dimensions slot)
                    ',(s-element-type slot)
                    (foreign-slot-pointer ,ptr ',(s-parent-ffi-name slot)
                                          ',(s-name slot))
                    ,@(when (s-layout slot)
                            `(:element-byte-size
                              ,(layout-machine-unit-size
                                (layout-element-layout
                                 (s-layout slot))))))))
         ,(if return-c-arrays
              'carr
              '(pull1-g carr)))))

;;------------------------------------------------------------

(defun+ slot-setter (ptr value slot)
  (cond
    ((or (member (s-type slot) cffi:*built-in-foreign-types*)
         (assoc (s-type slot) cffi::*extra-primitive-types*))
     (make-eprim-slot-setter ptr value slot))
    ((not (s-arrayp slot))
     (make-t-slot-setter ptr value slot))
    ((s-arrayp slot)
     (make-array-slot-setter ptr value slot))
    (t (error "Dont know what to do with slot ~a" slot))))

(defun+ make-eprim-slot-setter (ptr
                                value
                                slot)
  `(setf (mem-ref (foreign-slot-pointer ,ptr
                                        ',(s-parent-ffi-name slot)
                                        ',(s-name slot))
                  ,(s-type slot))
         ,value))

(defun+ make-t-slot-setter (ptr value slot)
  (let ((typed-populate (symb :populate- (s-type slot))))
    `(let ((slot-ptr (foreign-slot-pointer ,ptr
                                           ',(s-parent-ffi-name slot)
                                           ',(s-name slot))))
       (,typed-populate slot-ptr ,value))))

(defun+ make-array-slot-setter (ptr value slot)
  `(let* ((slot-ptr (foreign-slot-pointer ,ptr
                                          ',(s-parent-ffi-name slot)
                                          ',(s-name slot)))
          (carr (cepl.c-arrays:make-c-array-from-pointer
                 ',(s-dimensions slot) ',(s-type slot) slot-ptr)))
     (cepl.c-arrays::c-populate carr ,value)))

;;------------------------------------------------------------

(defun+ buffer-stream-comptible-typep (slot)
  (let* ((type-spec (s-type slot)))
    (or (not (null (cdr (assoc type-spec +cpu->gpu-vec-mappings+))))
        (core-typep (type-spec->type type-spec)))))

(defun+ make-struct-attrib-assigner (type-name slots)
  (when (every #'buffer-stream-comptible-typep slots)
    (let* ((stride (if (> (length slots) 1)
                       `(cepl.internals:gl-type-size ',type-name)
                       0))
           (stride-sym (gensym "stride"))
           (def-sets (mapcat #'expand-slot-to-layout slots))
           (definitions
            (loop :for (len cffi-type normalized gl-type)
               :in def-sets
               :for i :from 0 :with offset = 0 :append
               `((%gl:enable-vertex-attrib-array (+ attrib-offset ,i))
                 (when instance-divisor
                   (%gl:vertex-attrib-divisor (+ attrib-offset ,i)
                                              instance-divisor))
                 (%gl:vertex-attrib-pointer
                  (+ attrib-offset ,i)
                  ,len
                  ,(or gl-type cffi-type)
                  ,normalized
                  ,stride-sym
                  (cffi:make-pointer (+ ,offset pointer-offset))))
               :do (incf offset (* len (cepl.internals:gl-type-size cffi-type))))))
      (when definitions
        `(progn
           (defmethod cepl.internals:gl-assign-attrib-pointers
               ((array-type (EQL ',type-name))
                &optional (attrib-offset 0) (pointer-offset 0)
                  stride-override normalized instance-divisor)
             (declare (ignore array-type normalized))
             (let ((,stride-sym (or stride-override ,stride)))
               ,@definitions
               ,(length def-sets))))))))

(defun+ expand-slot-to-layout (slot &optional type normalize)
  ;; if it one of the types in +cpu->gpu-vec-mappings+ then it is one
  ;; where the gpu side type will be different from the cpu-side one
  ;; this means we cant rely on the varjo type introspection
  (if (and (not type) (assoc (s-type slot) +cpu->gpu-vec-mappings+))
      (expand-unmappable-slot-to-layout slot type normalize)
      (expand-mappable-slot-to-layout slot type normalize)))

(defun+ expand-unmappable-slot-to-layout (slot type normalize)
  (assert (not type))
  (ecase (s-type slot)
    (:uint8-vec2 (list (list 2 :uint8 normalize :unsigned-byte)))
    (:uint8-vec3 (list (list 3 :uint8 normalize :unsigned-byte)))
    (:uint8-vec4 (list (list 4 :uint8 normalize :unsigned-byte)))))

(defun+ expand-mappable-slot-to-layout (slot type normalize)
  (let ((type (or type (type-spec->type (s-type slot))))
        (normalize (or normalize (when slot (s-normalizedp slot)))))
    ;;
    (cond ((v-typep type 'v-matrix)
           (let* ((base (etypecase type
                          (v-dmatrix :dvec)
                          (v-matrix :vec)))
                  (v-type (type-spec->type
                           (kwd base (second (v-dimensions type))))))
             (loop for i below (first (v-dimensions type))
                :append (expand-mappable-slot-to-layout nil v-type normalize))))
          ;;
          ((v-typep type 'v-vector)
           (list (list (apply #'* (v-dimensions type))
                       (type->spec (v-element-type type))
                       normalize)))
          ;;
          ((v-typep type 'v-array)
           (loop for i below (apply #'* (v-dimensions type))
              :append (expand-mappable-slot-to-layout
                       nil (v-element-type type) normalize)))
          ;;
          (t `((1 ,(type->spec type) ,normalize))))))

;;------------------------------------------------------------

(defun+ make-struct-pixel-format (name slots)
  (let* ((type (s-type (first slots)))
         (len (length slots)))
    (when (< len 5)
      (let ((components (cepl-utils:kwd (subseq "RGBA" 0 len))))
        (when (and (loop for i in slots always (eql (s-type i) type))
                   (cepl.pixel-formats::valid-pixel-format-p
                    components type t nil))
          `(defmethod lisp-type->pixel-format ((type (eql ',name)))
             (cepl.pixel-formats::pixel-format! ,components ',type)))))))

;;------------------------------------------------------------

;; (defmacro with-g-struct (type destructuring-form pointer &body body)
;;   )

;; (with-struct g-pnt (pos (norm x z))
;;   (* pos x z))
