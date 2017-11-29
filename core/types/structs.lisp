(in-package :cepl.types)

(define-const +cpu->gpu-vec-mappings+
    '((:uint8-vec2 . :vec2)
      (:uint8-vec3 . :vec3)
      (:uint8-vec4 . :vec4))
  :type list)

;;------------------------------------------------------------

(defgeneric s-arrayp (object))
(defgeneric s-prim-p (object))
(defgeneric s-extra-prim-p (object))
(defgeneric s-def (object))
(defgeneric s-slot-args (slot args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gl-struct-slot ()
    ((name :initarg :name :reader s-name)
     (type :initarg :type :reader s-type)
     (element-type :initarg :element-type :reader s-element-type)
     (dimensions :initarg :dimensions :initform 1 :reader s-dimensions)
     (normalized :initarg :normalized :reader s-normalizedp)
     (reader :initarg :reader :reader s-reader)
     (writer :initarg :writer :reader s-writer)
     (uses-method-p :initarg :uses-method-p :reader s-uses-method-p))))

(defmethod s-arrayp ((object gl-struct-slot))
  (eq (s-type object) :array))

(defmethod s-prim-p ((spec t))
  (not (null (member spec cffi::*built-in-foreign-types*))))
(defmethod s-extra-prim-p ((spec t))
  (not (null (assoc spec cffi::*extra-primitive-types*))))

(defmethod s-prim-p ((object gl-struct-slot))
  (s-prim-p (s-type object)))
(defmethod s-extra-prim-p ((object gl-struct-slot))
  (s-extra-prim-p (s-type object)))

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

(defmacro defstruct-g (name-and-options &body slot-descriptions)
  (dbind (name &key (accesors t) (constructor (symb 'make- name))
               (readers t) (writers t) (pull-push t) (attribs t) (populate t))
      (listify name-and-options)
    (let* ((slots (mapcar (lambda (_) (normalize-slot-description
                                       _ name (and readers accesors)
                                       (and writers accesors)))
                          slot-descriptions))
           (foreign-struct-name (hidden-symb name :foreign))
           (qualified-struct-name `(:struct ,foreign-struct-name))
           (get-ptr-name (hidden-symb name :ptr))
           (typed-populate (symb :populate- name)))
      (when (validate-defstruct-g-form name slots)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             ,(make-varjo-struct-def name slots))
           ,@(make-instance-wrapper-def name foreign-struct-name get-ptr-name
                                        slots typed-populate)
           ,@(when (and readers accesors)
               (remove nil (mapcar (lambda (_)
                                     (make-slot-getter
                                      get-ptr-name _ name qualified-struct-name))
                                   slots)))
           ,@(when (and writers accesors)
               (remove nil (mapcar (lambda (_)
                                     (make-slot-setter
                                      get-ptr-name _ name qualified-struct-name))
                                   slots)))
           ,(when constructor (make-make-struct constructor name slots))
           ,(when attribs (make-struct-attrib-assigner name slots))
           ,@(when populate (make-populate name typed-populate slots))
           ,(make-struct-pixel-format name slots)
           ,@(when pull-push (make-pull-push name slots))
           ,(make-varjo-struct-lookup name)
           ',name)))))

(defun+ normalize-slot-description (slot-description type-name readers writers)
  (destructuring-bind (name type &key normalized accessor) slot-description
    (let* ((type (listify type))
           (dimensions (listify (or (second type) 1)))
           (arrayp (> (first dimensions) 1))
           (element-type (when arrayp (first type)))
           (type (if arrayp :array (first type))))
      (make-instance 'gl-struct-slot :name name :type type :normalized normalized
                     :reader (when readers
                               (or accessor (symb type-name '- name)))
                     :writer (when writers
                               (or accessor (symb type-name '- name)))
                     :uses-method-p (not (null accessor))
                     :element-type element-type :dimensions dimensions))))

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
    (type->spec (type-spec->type spec))))

;;------------------------------------------------------------

(defun+ make-varjo-struct-lookup (name)
  `(defmethod cepl.internals:symbol-names-cepl-structp ((sym (eql ',name)))
     t))

;;------------------------------------------------------------

(defun+ make-instance-wrapper-def (name
                                   foreign-struct-name
                                   get-ptr
                                   slots
                                   typed-populate)
  (let* ((foreign-type-name (hidden-symb name :cffi-ct-type))
         (make-name (hidden-symb name :make))
         (slot-defs (mapcar #'format-slot-for-cstruct slots))
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
          (,make-name ptr))

        (defmethod expand-from-foreign (ptr (type ,foreign-type-name))
          (list ',make-name ptr)))

      (defstruct (,name (:conc-name nil)
                        (:constructor ,make-name (,get-ptr))
                        (:copier nil))
        (,get-ptr (null-pointer) :type foreign-pointer :read-only t))

      (defn-inline ,from ((ptr foreign-pointer)) ,name
        (declare (optimize (speed 3) (safety 0) (debug 0)))
        (,make-name ptr))

      (defn-inline ,to ((ptr foreign-pointer) (value t)) t
        (declare (optimize (speed 3) (safety 0) (debug 0)))
        (,typed-populate (,make-name ptr) value))

      (defmethod get-typed-from-foreign ((type-name (eql ',name)))
        #',from)

      (defmethod get-typed-to-foreign ((type-name (eql ',name)))
        #',to))))

(defun+ format-slot-for-cstruct (slot)
  (if (s-arrayp slot)
      (format-array-slot-for-cstruct slot)
      (list (s-name slot) (s-type slot))))

(defun+ format-array-slot-for-cstruct (slot)
  (when (> (length (s-dimensions slot)) 1)
    (error "Cannot currently support multi dimensional arrays"))
  (list (s-name slot)
        `(:array ,(s-element-type slot) ,(reduce #'* (s-dimensions slot)))))

;;{TODO} should be setting fields here
(defun+ make-make-struct (constructor-name awrap-type-name slots)
  (let ((vars (loop :for s :in slots :collect (s-name s))))
    `(defun+ ,constructor-name ,(cons '&key vars)
       (let ((result (foreign-alloc ',awrap-type-name)))
         ,@(loop :for s :in slots :for v :in vars :collect
              `(when ,v (setf (,(s-writer s) result) ,v)))
         result))))

;;------------------------------------------------------------

(defun+ make-slot-getter (get-ptr slot type-name foreign-struct-name)
  (when (s-reader slot)
    (cond
      ((not (s-arrayp slot)) (make-t-slot-getter
                              get-ptr slot type-name foreign-struct-name))
      ((s-arrayp slot) (make-array-slot-getter
                        get-ptr slot type-name foreign-struct-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun+ make-t-slot-getter (get-ptr slot type-name foreign-struct-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,type-name)))
     (foreign-slot-value (,get-ptr wrapped-object)
                         ',foreign-struct-name
                         ',(s-name slot))))

(defun+ make-array-slot-getter (get-ptr slot type-name foreign-struct-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,type-name)))
     (cepl.c-arrays::make-c-array-from-pointer
      ',(s-dimensions slot) ',(s-element-type slot)
      (foreign-slot-pointer (,get-ptr wrapped-object)
                            ',foreign-struct-name
                            ',(s-name slot)))))

(defun+ make-slot-setter (get-ptr slot type-name foreign-struct-name)
  (when (s-writer slot)
    (cond
      ((or (member (s-type slot) cffi:*built-in-foreign-types*)
           (assoc (s-type slot) cffi::*extra-primitive-types*))
       (make-eprim-slot-setter get-ptr slot type-name foreign-struct-name))
      ((not (s-arrayp slot)) (make-t-slot-setter slot type-name))
      ((s-arrayp slot) (make-array-slot-setter slot type-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun+ make-eprim-slot-setter (get-ptr slot type-name foreign-struct-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `(value (wrapped-object ,type-name)))
     (setf (mem-ref (foreign-slot-pointer (,get-ptr wrapped-object)
                                          ',foreign-struct-name
                                          ',(s-name slot))
                    ,(s-type slot))
           value)))

(defun+ make-t-slot-setter (slot type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,type-name)))
     (cepl.internals:populate (,(s-reader slot) wrapped-object) value)))

(defun+ make-array-slot-setter (slot type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,type-name)))
     (cepl.c-arrays::c-populate (,(s-reader slot) wrapped-object) value)))

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

(defun+ make-pull-push (name slots)
  `((defmethod pull-g ((object ,name))
      (list
       ,@(loop :for slot :in slots :for i :from 0 :collect
            (if (s-arrayp slot)
                `(pull-g (,(s-reader slot) object))
                `(,(s-reader slot) object)))))
    (defmethod pull1-g ((object ,name))
      (list
       ,@(loop :for slot :in slots :for i :from 0 :collect
            (if (s-arrayp slot)
                `(pull-g (,(s-reader slot) object))
                `(,(s-reader slot) object)))))
    (defmethod push-g ((object list) (destination ,name))
      (cepl.internals:populate destination object))))

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
