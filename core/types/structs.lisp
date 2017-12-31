(in-package :cepl.types)

;;------------------------------------------------------------

(define-const +cpu->gpu-vec-mappings+
    '((:uint8-vec2 . :vec2)
      (:uint8-vec3 . :vec3)
      (:uint8-vec4 . :vec4))
  :type list)

;;------------------------------------------------------------

(defvar *struct-slot-defs* (make-hash-table))

(defun g-struct-info (name)
  (values (gethash name *struct-slot-defs*)))

(defun (setf g-struct-info) (slots name)
  (setf (gethash name *struct-slot-defs*)
        slots))

;;------------------------------------------------------------

(defclass cepl-struct-definition ()
  ((name :initarg :name :reader s-name)
   (layout :initarg :layout :reader s-layout)
   (slots :initarg :slots :reader s-slots)))

(defmethod make-load-form ((obj cepl-struct-definition) &optional environment)
  (declare (ignore environment))
  (with-slots (name layout slots) obj
    `(make-instance 'cepl-struct-definition
                    :name ',name
                    :layout ',layout
                    :slots ',slots)))

;;------------------------------------------------------------

(defgeneric s-arrayp (object))
(defgeneric s-def (object))
(defgeneric s-slot-args (slot args))

(defclass gl-struct-slot ()
  ((name :initarg :name :reader s-name)
   (type :initarg :type :reader s-type)
   (element-type :initarg :element-type :reader s-element-type)
   (dimensions :initarg :dimensions :initform 1 :reader s-dimensions)
   (normalized :initarg :normalized :reader s-normalizedp)
   (reader :initarg :reader :reader s-reader)
   (writer :initarg :writer :reader s-writer)
   (uses-method-p :initarg :uses-method-p :reader s-uses-method-p)
   (layout :initarg :layout :reader s-layout)
   (parent-ffi-name :initarg :parent-ffi-name :reader s-parent-ffi-name)))

(defmethod make-load-form ((slot gl-struct-slot) &optional environment)
  (declare (ignore environment))
  (with-slots (name
               type
               element-type
               dimensions
               normalized
               reader
               writer
               uses-method-p
               layout
               parent-ffi-name)
      slot
    `(make-instance 'gl-struct-slot
                    :name ',name
                    :type ',type
                    :normalized ',normalized
                    :reader ',reader
                    :writer ',writer
                    :element-type ',element-type
                    :dimensions ',dimensions
                    :layout ',layout
                    :parent-ffi-name ',parent-ffi-name)))

(defmethod s-arrayp ((object gl-struct-slot))
  (eq (s-type object) :array))

(defmethod v-type-of ((gss gl-struct-slot))
  (if (s-arrayp gss)
      (type-spec->type (list (s-element-type gss) (s-dimensions gss)))
      (type-spec->type (s-type gss))))

(defmethod s-def ((object gl-struct-slot))
  (if (s-uses-method-p object)
      'defmethod
      'defun))

(defmethod s-slot-args ((slot gl-struct-slot) (args list))
  (labels ((fun-arg (x) (if (listp x) (first x) x)))
    (if (s-uses-method-p slot)
        args
        (mapcar #'fun-arg args))))

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

;;------------------------------------------------------------

(defun potential-struct-layout (name layout-specifier slot-descriptions)
  (unless (string= layout-specifier :default)
    (calc-struct-layout-from-name-type-pairs
     layout-specifier
     name
     (mapcar (lambda (x)
               (dbind (name type &rest rest) x
                 (declare (ignore rest))
                 (list name (type-spec->type type))))
             slot-descriptions))))

(defmacro defstruct-g (name-and-options &body slot-descriptions)
  (dbind (name &key (accessors t) (constructor (symb 'make- name))
               (readers t) (writers t) (pull-push t) (attribs t) (populate t)
               (layout :default))
      (listify name-and-options)
    (let* (;; names
           (foreign-struct-name (hidden-symb name :foreign))
           (qualified-struct-name `(:struct ,foreign-struct-name))
           (get-ptr-name (hidden-symb name :ptr))
           (typed-populate (symb :populate- name))
           (wrapper-constructor-name (hidden-symb name :make))
           ;; slots
           (layout-specifier layout)
           (layout (potential-struct-layout name
                                            layout-specifier
                                            slot-descriptions))
           (slot-layouts (if layout
                             (layout-members layout)
                             (n-of nil (length slot-descriptions))))
           (slots (mapcar (lambda (desc layout)
                            (normalize-slot-description qualified-struct-name
                                                        desc
                                                        layout
                                                        name
                                                        (and readers accessors)
                             (and writers accessors)))
                          slot-descriptions
                          slot-layouts))
           (struct-info (make-instance 'cepl-struct-definition
                                       :name name
                                       :layout layout
                                       :slots slots)))
      (assert (= (length slot-layouts) (length slots)))
      (when (validate-defstruct-g-form name slots)
        (setf (g-struct-info name) struct-info)
        `(progn
           (setf (g-struct-info ',name) ',struct-info)
           (eval-when (:compile-toplevel :load-toplevel :execute)
             ,(make-varjo-struct-def name slots))
           ,@(make-instance-wrapper-def name
                                        foreign-struct-name
                                        wrapper-constructor-name
                                        get-ptr-name
                                        slots typed-populate slot-layouts)
           ,@(when (and readers accessors)
               (remove nil (mapcar (lambda (slot slot-layout)
                                     (make-slot-getter get-ptr-name slot name
                                                       qualified-struct-name
                                                       slot-layout))
                                   slots
                                   slot-layouts)))
           ,@(when (and writers accessors)
               (remove nil (mapcar (lambda (slot slot-layout)
                                     (make-slot-setter get-ptr-name slot name
                                                       qualified-struct-name
                                                       slot-layout))
                                   slots
                                   slot-layouts)))
           ,(when constructor (make-make-struct constructor
                                                wrapper-constructor-name
                                                name
                                                slots))
           ,(when attribs (make-struct-attrib-assigner name slots))
           ,@(when populate (make-populate name typed-populate slots))
           ,(make-struct-pixel-format name slots)
           ,@(when pull-push (make-pull-push name slots))
           ,(make-varjo-struct-lookup name)
           ',name)))))

(defun+ normalize-slot-description (qualified-struct-name
                                    slot-description
                                    slot-layout
                                    type-name
                                    readers
                                    writers)
  (destructuring-bind (name type &key normalized accessor) slot-description
    (let* ((type (listify type))
           (dimensions (listify (or (second type) 1)))
           (arrayp (> (first dimensions) 1))
           (element-type (when arrayp (first type)))
           (type (if arrayp :array (first type))))
      (make-instance 'gl-struct-slot
                     :name name
                     :type type
                     :normalized normalized
                     :reader (when readers
                               (or accessor (symb type-name '- name)))
                     :writer (when writers
                               (or accessor (symb type-name '- name)))
                     :uses-method-p (not (null accessor))
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
  (let ((accessor (or (s-reader slot) (s-writer slot))))
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
    (type->type-spec (type-spec->type spec))))

;;------------------------------------------------------------

(defun+ make-varjo-struct-lookup (name)
  `(defmethod cepl.internals:symbol-names-cepl-structp ((sym (eql ',name)))
     t))

;;------------------------------------------------------------

(defun+ make-instance-wrapper-def (name
                                   foreign-struct-name
                                   wrapper-constructor-name
                                   get-ptr
                                   slots
                                   typed-populate
                                   slot-layouts)
  (let* ((foreign-type-name (hidden-symb name :cffi-ct-type))
         (slot-defs (mapcar #'format-slot-for-cstruct
                            slots
                            slot-layouts))
         ;;
         (from (hidden-symb name :from-foreign))
         (to (hidden-symb name :to-foreign)))
    `((eval-when (:compile-toplevel :load-toplevel :execute)
        (defcstruct ,foreign-struct-name
          ,@slot-defs)

        (define-foreign-type ,foreign-type-name nil nil
                             (:actual-type :struct ,foreign-struct-name)
                             (:simple-parser ,name))

        (defmethod translate-from-foreign (ptr (type ,foreign-type-name))
          (,wrapper-constructor-name ptr))

        (defmethod expand-from-foreign (ptr (type ,foreign-type-name))
          (list ',wrapper-constructor-name ptr)))

      (defstruct (,name (:conc-name nil)
                        (:constructor ,wrapper-constructor-name (,get-ptr))
                        (:copier nil))
        (,get-ptr (null-pointer) :type foreign-pointer :read-only t))

      (defmethod print-object ((obj ,name) stream)
        (format stream "#<~a {~x}>"
                ',name
                (cffi:pointer-address (,get-ptr obj))))

      (defn-inline ,from ((ptr foreign-pointer)) ,name
        (declare (optimize (speed 3) (safety 0) (debug 0)))
        (,wrapper-constructor-name ptr))

      (defn-inline ,to ((ptr foreign-pointer) (value t)) t
        (declare (optimize (speed 3) (safety 0) (debug 0)))
        (,typed-populate (,wrapper-constructor-name ptr) value))

      (defmethod get-typed-from-foreign ((type-name (eql ',name)))
        #',from)

      (defmethod get-typed-to-foreign ((type-name (eql ',name)))
        #',to))))

(defun+ format-slot-for-cstruct (slot layout)
  (if (s-arrayp slot)
      (format-array-slot-for-cstruct slot layout)
      `(,(s-name slot)
         ,(s-type slot)
         ,@(when layout `(:offset ,(layout-base-offset layout))))))

(defun+ format-array-slot-for-cstruct (slot layout)
  (when (> (length (s-dimensions slot)) 1)
    (error "Cannot currently support multi dimensional arrays"))
  ;; As we cant encode stride in defcstruct we instead make a byte array of
  ;; the correct size and rely on c-array to handle the stride correctly.
  (if layout
      `(,(s-name slot)
         (:array :uint8 ,(layout-machine-unit-size layout))
         :offset ,(layout-base-offset layout))
      `(,(s-name slot)
         (:array ,(s-element-type slot) ,(reduce #'* (s-dimensions slot))))))

;;{TODO} should be setting fields here
(defun+ make-make-struct (constructor-name
                          wrapper-constructor-name
                          awrap-type-name
                          slots)
  (let ((vars (loop :for s :in slots :collect (s-name s))))
    `(defun+ ,constructor-name ,(cons '&key vars)
       (let ((result (,wrapper-constructor-name
                      (foreign-alloc ',awrap-type-name))))
         ,@(loop :for s :in slots :for v :in vars :collect
              `(when ,v (setf (,(s-writer s) result) ,v)))
         result))))

;;------------------------------------------------------------

(defun+ make-slot-getter (get-ptr slot type-name foreign-struct-name layout)
  (when (s-reader slot)
    (cond
      ((not (s-arrayp slot)) (make-t-slot-getter
                              get-ptr slot type-name foreign-struct-name))
      ((s-arrayp slot) (make-array-slot-getter
                        get-ptr slot type-name foreign-struct-name layout))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun+ make-t-slot-getter (get-ptr slot type-name foreign-struct-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,type-name)))
     (foreign-slot-value (,get-ptr wrapped-object)
                         ',foreign-struct-name
                         ',(s-name slot))))

(defun+ make-array-slot-getter (get-ptr slot type-name foreign-struct-name
                                        layout)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,type-name)))
     (cepl.c-arrays::make-c-array-from-pointer
      ',(s-dimensions slot) ',(s-element-type slot)
      (foreign-slot-pointer (,get-ptr wrapped-object)
                            ',foreign-struct-name
                            ',(s-name slot))
      ,@(when layout `(:element-byte-size
                       ,(layout-machine-unit-size
                         (layout-element-layout layout)))))))

(defun+ make-slot-setter (get-ptr slot type-name foreign-struct-name layout)
  (when (s-writer slot)
    (cond
      ((or (member (s-type slot) cffi:*built-in-foreign-types*)
           (member (s-type slot) '(:uint))
           (assoc (s-type slot) cffi::*extra-primitive-types*))
       (make-eprim-slot-setter get-ptr slot type-name foreign-struct-name layout))
      ((not (s-arrayp slot))
       (make-t-slot-setter slot type-name layout))
      ((s-arrayp slot)
       (make-array-slot-setter slot type-name layout))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun+ make-eprim-slot-setter (get-ptr
                                slot
                                type-name
                                foreign-struct-name
                                layout)
  (declare (ignore layout))
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `(value (wrapped-object ,type-name)))
     (setf (mem-ref (foreign-slot-pointer (,get-ptr wrapped-object)
                                          ',foreign-struct-name
                                          ',(s-name slot))
                    ,(s-type slot))
           value)))

(defun+ make-t-slot-setter (slot type-name layout)
  (declare (ignore layout))
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,type-name)))
     (cepl.internals:populate (,(s-reader slot) wrapped-object) value)))

(defun+ make-array-slot-setter (slot type-name layout)
  (declare (ignore layout))
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,type-name)))
     (cepl.c-arrays::c-populate (,(s-reader slot) wrapped-object) value)))

;;------------------------------------------------------------

(defun+ buffer-stream-comptible-typep (slot)
  (when (not (s-arrayp slot))
    (let* ((type-spec (s-type slot)))
      (or (not (null (cdr (assoc type-spec +cpu->gpu-vec-mappings+))))
          (core-typep (type-spec->type type-spec))))))

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
                       (type->type-spec (v-element-type type))
                       normalize)))
          ;;
          ((v-typep type 'v-array)
           (loop for i below (apply #'* (v-dimensions type))
              :append (expand-mappable-slot-to-layout
                       nil (v-element-type type) normalize)))
          ;;
          (t `((1 ,(type->type-spec type) ,normalize))))))

;;------------------------------------------------------------

(defun+ make-pull-push (name slots)
  (let ((pull-body `(list
                     ,@(loop :for slot :in slots :for i :from 0 :collect
                          (if (s-arrayp slot)
                              `(pull-g (,(s-reader slot) object))
                              `(,(s-reader slot) object))))))
    `((defmethod pull-g ((object ,name)) ,pull-body)
      (defmethod pull1-g ((object ,name)) ,pull-body)
      (defmethod push-g ((object list) (destination ,name))
        (cepl.internals:populate destination object)))))

;;------------------------------------------------------------

(defun+ make-populate (name typed-populate slots)
  `((defun+ ,typed-populate (object data)
      (declare (type ,name object))
      (unless (or (vectorp data) (listp data))
        (error "can only populate a struct of type ~a with a list or an array"
               ',name))
      ,@(loop :for slot :in slots :for i :from 0 :collect
           `(setf (,(s-writer slot) object) (elt data ,i)))
      object)
    (defmethod cepl.internals:populate ((object ,name) data)
      (,typed-populate object data))))

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
