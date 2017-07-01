(in-package :cepl.types)

(defvar *cpu->gpu-vec-mappings*
  '((:uint8-vec2 . :vec2)
    (:uint8-vec3 . :vec3)
    (:uint8-vec4 . :vec4)))

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


(defun2 nest-simple-loops (dimensions index-vars body &optional (loop-op :do))
  (labels ((inner (dimensions index-vars body loop-op)
             (if (or (null dimensions) (null index-vars))
                 body
                 (inner (rest dimensions) (rest index-vars)
                        `(loop :for ,(car index-vars) :below ,(car dimensions)
                            ,loop-op ,body)
                        loop-op))))
    (inner (reverse dimensions) (reverse index-vars) body loop-op)))

(defun2 type->spec (type)
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

;;{TODO} Autowrap-name is name now...can clean up lots of code
(defmacro defstruct-g (name-and-options &body slot-descriptions)
  (dbind (name &key (accesors t) (constructor (symb 'make- name))
               (readers t) (writers t) (pull-push t) (attribs t) (populate t))
      (listify name-and-options)
    (let ((slots (mapcar (lambda (_) (normalize-slot-description
                                      _ name (and readers accesors)
                                      (and writers accesors)))
                         slot-descriptions)))
      (when (validate-defstruct-g-form name slots)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             ,@(make-autowrap-record-def name slots))
           (autowrap:define-wrapper* (:struct (,name)) ,name
             :constructor ,(symb '%make- name))
           (eval-when (:compile-toplevel :load-toplevel :execute)
             ,(make-varjo-struct-def name slots))
           ,(make-varjo-struct-lookup name)
           ,@(when (and readers accesors)
                   (remove nil (mapcar (lambda (_)
                                         (make-slot-getter _ name))
                                       slots)))
           ,@(when (and writers accesors)
                   (remove nil (mapcar (lambda (_)
                                         (make-slot-setter _ name name))
                                       slots)))
           ,(when constructor (make-make-struct constructor name slots))
           ,(when attribs (make-struct-attrib-assigner name slots))
           ,(make-struct-pixel-format name slots)
           ,@(when populate (make-populate name slots name))
           ,@(make-foreign-conversions name)
           ,@(when pull-push (make-pull-push name slots))
           ',name)))))

(defun2 normalize-slot-description (slot-description type-name readers writers)
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
(defun2 validate-defstruct-g-form (name slots)
  (when (keywordp name) (error "glstruct names cannot be keywords"))
  (when (null slots) (error "glstruct must have at least 1 slot"))
  t)

;;------------------------------------------------------------

(defun2 make-varjo-struct-def (name slots)
  (let ((hidden-name (symb-package (symbol-package name)
                                   'v_ name )))
    `(v-defstruct
         (,name :shadowing ,hidden-name)
         ()
       ,@(mapcar #'format-slot-for-varjo slots))))

;;{TODO} make varjo support readers and writers and then remove this hack
(defun2 format-slot-for-varjo (slot)
  (let ((accessor (or (s-reader slot) (s-writer slot))))
    (if (s-arrayp slot)
        `(,(s-name slot)
           ,(validate-varjo-type-spec
             (list (s-element-type slot) (s-dimensions slot)))
           ,@(when accessor `(:accessor ,accessor)))
        `(,(s-name slot)
           ,(validate-varjo-type-spec (s-type slot))
           ,@(when accessor `(:accessor ,accessor))))))

(defun2 validate-varjo-type-spec (spec)
  (let ((spec (or (cdr (assoc spec *cpu->gpu-vec-mappings*))
                  spec)))
    (type->spec (type-spec->type spec))))

;;------------------------------------------------------------

(defun2 make-varjo-struct-lookup (name)
  `(defmethod cepl.internals:symbol-names-cepl-structp ((sym (eql ',name)))
     t))

;;------------------------------------------------------------

(defun2 make-autowrap-record-def (name slots)
  (let ((slot-defs (mapcar #'format-slot-for-autowrap slots)))
    (list
     `(autowrap:define-foreign-record
          ',name
          :struct
        ,(loop :for i :in slot-defs :summing (nth (1+ (position :bit-size i)) i))
        8
        ',(loop :for i :in slot-defs :with offset = 0
             :collect (subst offset :-offset- i)
             :do (incf offset (nth (1+ (position :bit-size i)) i))))
     `(autowrap:define-foreign-alias ',name '(:struct (,name))))))

(defun2 format-slot-for-autowrap (slot)
  (if (s-arrayp slot)
      (format-array-slot-for-autowrap slot)
      (%format-slot-for-autowrap slot)))

(defun2 %format-slot-for-autowrap (slot)
  (let* ((s-type (if (assoc (s-type slot) cffi::*extra-primitive-types*)
                     (symb-package :cffi :cepl- (s-type slot))
                     (s-type slot)))
         (a-type (autowrap:find-type s-type)))
    (list (kwd (s-name slot))
          (s-type slot)
          :bit-size (* 8 (autowrap:foreign-type-size a-type))
          :bit-offset :-offset-
          :bit-alignment 8)))

(defun2 format-array-slot-for-autowrap (slot)
  (when (> (length (s-dimensions slot)) 1)
    (error "Cannot currently support multi dimensional autowrap arrays"))
  (let* ((e-type (if (assoc (s-element-type slot) cffi::*extra-primitive-types*)
                     (symb-package :cffi :cepl- (s-element-type slot))
                     (s-element-type slot)))
         (a-type (autowrap:find-type e-type)))
    (list (kwd (s-name slot))
          `(:array ,(s-element-type slot) ,(reduce #'* (s-dimensions slot)))
          :bit-size (* (autowrap:foreign-type-size a-type)
                       (reduce #'* (s-dimensions slot))
                       8)
          :bit-offset :-offset-
          :bit-alignment 8)))

;;{TODO} should be setting fields here
(defun2 make-make-struct (constructor-name awrap-type-name slots)
  (let ((vars (loop :for s :in slots :collect (s-name s))))
    `(defun ,constructor-name ,(cons '&key vars)
       (let ((result (autowrap:alloc ',awrap-type-name)))
         ,@(loop :for s :in slots :for v :in vars :collect
              `(when ,v (setf (,(s-writer s) result) ,v)))
         result))))

;;------------------------------------------------------------

(defun2 make-slot-getter (slot type-name)
  (when (s-reader slot)
    (cond
      ((member (s-type slot) cffi:*built-in-foreign-types*)
       (make-prim-slot-getter slot type-name))
      ((assoc (s-type slot) cffi::*extra-primitive-types*)
       (make-eprim-slot-getter slot type-name))
      ((not (s-arrayp slot)) (make-t-slot-getter slot type-name))
      ((s-arrayp slot) (make-array-slot-getter slot type-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun2 make-prim-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (plus-c:c-ref wrapped-object ,awrap-type-name ,(kwd (s-name slot)))))

(defun2 make-eprim-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (mem-ref (plus-c:c-ref wrapped-object ,awrap-type-name
                            ,(kwd (s-name slot)) plus-c::&)
              ,(s-type slot))))

(defun2 make-t-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (plus-c:c-ref wrapped-object ,awrap-type-name
                   ,(kwd (s-name slot)))))

(defun2 make-array-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (cepl.c-arrays::make-c-array-from-pointer ',(s-dimensions slot) ,(s-element-type slot)
                                               (plus-c:c-ref wrapped-object ,awrap-type-name
                                                             ,(kwd (s-name slot)) plus-c::&))))

(defun2 make-slot-setter (slot type-name awrap-type-name)
  (when (s-writer slot)
    (cond
      ((member (s-type slot) cffi:*built-in-foreign-types*)
       (make-prim-slot-setter slot awrap-type-name))
      ((assoc (s-type slot) cffi::*extra-primitive-types*)
       (make-eprim-slot-setter slot awrap-type-name))
      ((not (s-arrayp slot)) (make-t-slot-setter slot awrap-type-name))
      ((s-arrayp slot) (make-array-slot-setter slot type-name awrap-type-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun2 make-prim-slot-setter (slot awrap-type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `(value (wrapped-object ,awrap-type-name)))
     (setf (plus-c:c-ref wrapped-object ,awrap-type-name ,(kwd (s-name slot)))
           value)))

(defun2 make-eprim-slot-setter (slot awrap-type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `(value (wrapped-object ,awrap-type-name)))
     (let ((ptr (plus-c:c-ref wrapped-object ,awrap-type-name
                              ,(kwd (s-name slot)) plus-c::&)))
       (setf (mem-ref ptr ,(s-type slot)) value))))

(defun2 make-t-slot-setter (slot awrap-type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,awrap-type-name)))
     (cepl.internals:populate (,(s-reader slot) wrapped-object) value)))

(defun2 make-array-slot-setter (slot type-name awrap-type-name)
  (declare (ignore type-name))
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,awrap-type-name)))
     (cepl.c-arrays::c-populate (,(s-reader slot) wrapped-object) value)))

;;------------------------------------------------------------

(defun2 buffer-stream-comptible-typep (slot)
  (let* ((type-spec (s-type slot)))
    (or (not (null (cdr (assoc type-spec *cpu->gpu-vec-mappings*))))
        (core-typep (type-spec->type type-spec)))))

(defun2 make-struct-attrib-assigner (type-name slots)
  (when (every #'buffer-stream-comptible-typep slots)
    (let* ((stride (if (> (length slots) 1)
                       `(cepl.internals:gl-type-size ',type-name)
                       0))
           (stride-sym (gensym "stride"))
           (definitions
            (loop :for (len cffi-type normalized gl-type)
               :in (mapcat #'expand-slot-to-layout slots)
               :for i :from 0 :with offset = 0 :append
               `((%gl:enable-vertex-attrib-array (+ attrib-offset ,i))
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
                  stride-override normalized)
             (declare (ignore array-type normalized))
             (let ((,stride-sym (or stride-override ,stride)))
               ,@definitions
               ,(length definitions))))))))

(defun2 expand-slot-to-layout (slot &optional type normalize)
  ;; if it one of the types in *cpu->gpu-vec-mappings* then it is one
  ;; where the gpu side type will be different from the cpu-side one
  ;; this means we cant rely on the varjo type introspection
  (if (and (not type) (assoc (s-type slot) *cpu->gpu-vec-mappings*))
      (expand-unmappable-slot-to-layout slot type normalize)
      (expand-mappable-slot-to-layout slot type normalize)))

(defun2 expand-unmappable-slot-to-layout (slot type normalize)
  (assert (not type))
  (ecase (s-type slot)
    (:uint8-vec2 (list (list 2 :uint8 normalize :unsigned-byte)))
    (:uint8-vec3 (list (list 3 :uint8 normalize :unsigned-byte)))
    (:uint8-vec4 (list (list 4 :uint8 normalize :unsigned-byte)))))

(defun2 expand-mappable-slot-to-layout (slot type normalize)
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

(defun2 make-pull-push (autowrap-name slots)
  `((defmethod pull-g ((object ,autowrap-name))
      (list
       ,@(loop :for slot :in slots :for i :from 0 :collect
            `(,(s-writer slot) object))))
    (defmethod pull1-g ((object ,autowrap-name))
      (list
       ,@(loop :for slot :in slots :for i :from 0 :collect
            `(,(s-writer slot) object))))
    (defmethod push-g ((object list) (destination ,autowrap-name))
      (cepl.internals:populate destination object))))

;;------------------------------------------------------------

(defun2 make-populate (autowrap-name slots struct-name)
  (let ((typed-name (symb :populate- struct-name)))
    `((defun ,typed-name (object data)
        (declare (type ,autowrap-name object))
        (unless (or (vectorp data) (listp data))
          (error "can only populate a struct of type ~a with a list or an array"
                 ',autowrap-name))
        ,@(loop :for slot :in slots :for i :from 0 :collect
             `(setf (,(s-writer slot) object) (elt data ,i)))
        object)
      (defmethod cepl.internals:populate ((object ,autowrap-name) data)
          (,typed-name object data)))))

;;------------------------------------------------------------

(defun2 make-foreign-conversions (type)
  (let* ((from (cepl-utils:symb type '-from-foreign))
         (to (cepl-utils:symb type '-to-foreign))
         (typed-populate (symb :populate- type)))
    `((declaim (inline ,from))
      (declaim (inline ,to))
      (defun ,from (ptr)
        (declare (type cffi:foreign-pointer ptr)
                 (optimize (speed 3) (safety 0) (debug 0)))
        (autowrap:wrap-pointer ptr ',type))
      (defun ,to (ptr value)
        (declare (type cffi:foreign-pointer ptr)
                 (optimize (speed 3) (safety 0) (debug 0)))
        (,typed-populate (autowrap:wrap-pointer ptr ',type) value))
      (defmethod get-typed-from-foreign ((type-name (eql ',type)))
        #',from)
      (defmethod get-typed-to-foreign ((type-name (eql ',type)))
        #',to))))

;;------------------------------------------------------------


(defun2 make-struct-pixel-format (name slots)
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
