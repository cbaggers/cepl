(in-package :jungl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gl-struct-slot ()
    ((name :initarg :name :reader s-name)
     (type :initarg :type :reader s-type)
     (element-type :initarg :element-type :reader s-element-type)
     (dimensions :initarg :dimensions :initform 1 :reader s-dimensions)
     (normalised :initarg :normalised :reader s-normalisedp)
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

(defgeneric s-slot-args (slot args))
(defmethod s-slot-args ((slot gl-struct-slot) (args list))
  (labels ((fun-arg (x) (if (listp x) (first x) x)))
    (if (s-uses-method-p slot)
	args
	(mapcar #'fun-arg args))))

;;------------------------------------------------------------


(defun nest-simple-loops (dimensions index-vars body &optional (loop-op :do))
  (labels ((inner (dimensions index-vars body loop-op)
             (if (or (null dimensions) (null index-vars))
                 body
                 (inner (rest dimensions) (rest index-vars)
                        `(loop :for ,(car index-vars) :below ,(car dimensions)
                            ,loop-op ,body)
                        loop-op))))
    (inner (reverse dimensions) (reverse index-vars) body loop-op)))

(defun type->spec (type)
  (let ((spec (type->type-spec type)))
    (if (listp spec)
        `(,(if (core-typep (type-spec->type (first spec)))
               (symb-package "KEYWORD" (subseq (symbol-name (first spec)) 2))
               (first spec))
           ,(if (and (listp (second spec)) (= (length (second spec)) 1))
                (first (second spec)) (second spec)))
        (if (core-typep type)
            (symb-package "KEYWORD" (subseq (symbol-name spec) 2))
            spec))))

;;------------------------------------------------------------

;;{TODO} Autowrap-name is name now...can clean up lots of code
(defmacro defstruct-g (name (&key (accesors t) (constructor t) varjo-constructor
                                  (readers t) (writers t) (pull-push t)
                                  (attribs t) (populate t))
                       &body slot-descriptions)
  (let ((slots (mapcar (lambda (_) (normalize-slot-description
                                    _ name (and readers accesors)
                                    (and writers accesors)))
                       slot-descriptions))
        (autowrap-name name))
    (when (validate-defstruct-g-form name slots)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(make-autowrap-record-def autowrap-name slots))
         (autowrap:define-wrapper* (:struct (,autowrap-name)) ,name
           :constructor ,(symb '%make- name))
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   ,(make-varjo-struct-def name slots varjo-constructor))
     ,(make-varjo-struct-lookup name)
     ,@(when (and readers accesors)
                 (remove nil (mapcar (lambda (_)
                                       (make-slot-getter _ name autowrap-name))
                                     slots)))
         ,@(when (and writers accesors)
                 (remove nil (mapcar (lambda (_)
                                       (make-slot-setter _ name autowrap-name))
                                     slots)))
         ,(when constructor (make-make-struct name autowrap-name slots))
         ,(when attribs (make-struct-attrib-assigner name slots))
         ,(make-struct-pixel-format name slots)
         ,(when populate (make-populate autowrap-name slots))
         ,@(when pull-push (make-pull-push autowrap-name slots))
         ',name))))

(defun normalize-slot-description (slot-description type-name readers writers)
  (destructuring-bind (name type &key normalised accessor) slot-description
    (let* ((type (listify type))
           (dimensions (listify (or (second type) 1)))
           (arrayp (> (first dimensions) 1))
           (element-type (when arrayp (first type)))
           (type (if arrayp :array (first type))))
      (make-instance 'gl-struct-slot :name name :type type :normalised normalised
                     :reader (when readers
                               (or accessor (symb type-name '- name)))
                     :writer (when writers
                               (or accessor (symb type-name '- name)))
		     :uses-method-p (not (null accessor))
                     :element-type element-type :dimensions dimensions))))

;; put all cepl's errors definitions in one place (like varjo)
(defun validate-defstruct-g-form (name slots)
  (when (keywordp name) (error "glstruct names cannot be keywords"))
  (when (null slots) (error "glstruct must have at least 1 slot"))
  t)

;;------------------------------------------------------------

(defun make-varjo-struct-def (name slots varjo-constructor)
  (let ((hidden-name (symb-package (symbol-package name)
                                   'v_ name )))
    `(v-defstruct
         (,name
          :shadowing ,hidden-name
          ,@(when varjo-constructor `(:constructor ,varjo-constructor))
          )
         ()
       ,@(mapcar #'format-slot-for-varjo slots))))

;;{TODO} make varjo support readers and writers and then remove this hack
(defun format-slot-for-varjo (slot)
  (let ((accessor (or (s-reader slot) (s-writer slot))))
    (if (s-arrayp slot)
        `(,(s-name slot)
           ,(validate-varjo-type-spec
             (list (s-element-type slot) (s-dimensions slot)))
           ,@(when accessor `(:accessor ,accessor)))
        `(,(s-name slot)
           ,(validate-varjo-type-spec (s-type slot))
           ,@(when accessor `(:accessor ,accessor))))))

(defun validate-varjo-type-spec (spec)
  (type->spec (type-spec->type spec)))

;;------------------------------------------------------------

(defun make-varjo-struct-lookup (name)
  `(defmethod symbol-names-cepl-structp ((sym (eql ',name)))
     t))

;;------------------------------------------------------------

(defun make-autowrap-record-def (name slots)
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

(defun format-slot-for-autowrap (slot)
  (if (s-arrayp slot)
      (format-array-slot-for-autowrap slot)
      (%format-slot-for-autowrap slot)))

(defun %format-slot-for-autowrap (slot)
  (let* ((s-type (if (assoc (s-type slot) cffi::*extra-primitive-types*)
                     (symb-package :cffi :jungl- (s-type slot))
                     (s-type slot)))
         (a-type (autowrap:find-type s-type)))
    (list (kwd (s-name slot))
          (s-type slot)
          :bit-size (* 8 (autowrap:foreign-type-size a-type))
          :bit-offset :-offset-
          :bit-alignment 8)))

(defun format-array-slot-for-autowrap (slot)
  (when (> (length (s-dimensions slot)) 1)
    (error "Cannot currently support multi dimensional autowrap arrays"))
  (let* ((e-type (if (assoc (s-element-type slot) cffi::*extra-primitive-types*)
                     (symb-package :cffi :jungl- (s-element-type slot))
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
(defun make-make-struct (name awrap-type-name slots)
  (let ((vars (loop :for s :in slots :collect (s-name s))))
    `(defun ,(symb 'make- name) ,(cons '&key vars)
       (let ((result (autowrap:alloc ',awrap-type-name)))
         ,@(loop :for s :in slots :for v :in vars :collect
              `(when ,v (setf (,(s-writer s) result) ,v)))
         result))))

;;------------------------------------------------------------

(defun make-slot-getter (slot type-name awrap-type-name)
  (declare (ignore type-name))
  (when (s-reader slot)
    (cond
      ((member (s-type slot) cffi:*built-in-foreign-types*)
       (make-prim-slot-getter slot awrap-type-name))
      ((assoc (s-type slot) cffi::*extra-primitive-types*)
       (make-eprim-slot-getter slot awrap-type-name))
      ((not (s-arrayp slot)) (make-t-slot-getter slot awrap-type-name))
      ((s-arrayp slot) (make-array-slot-getter slot awrap-type-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun make-prim-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (plus-c:c-ref wrapped-object ,awrap-type-name ,(kwd (s-name slot)))))

(defun make-eprim-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (mem-ref (plus-c:c-ref wrapped-object ,awrap-type-name
                            ,(kwd (s-name slot)) plus-c::&)
              ,(s-type slot))))

(defun make-t-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (plus-c:c-ref wrapped-object ,awrap-type-name
                   ,(kwd (s-name slot)))))

(defun make-array-slot-getter (slot awrap-type-name)
  `(,(s-def slot) ,(s-reader slot)
     ,(s-slot-args slot `((wrapped-object ,awrap-type-name)))
     (make-c-array-from-pointer ',(s-dimensions slot) ,(s-element-type slot)
                                (plus-c:c-ref wrapped-object ,awrap-type-name
                                              ,(kwd (s-name slot)) plus-c::&))))

(defun make-slot-setter (slot type-name awrap-type-name)
  (when (s-writer slot)
    (cond
      ((member (s-type slot) cffi:*built-in-foreign-types*)
       (make-prim-slot-setter slot awrap-type-name))
      ((assoc (s-type slot) cffi::*extra-primitive-types*)
       (make-eprim-slot-setter slot awrap-type-name))
      ((not (s-arrayp slot)) (make-t-slot-setter slot awrap-type-name))
      ((s-arrayp slot) (make-array-slot-setter slot type-name awrap-type-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun make-prim-slot-setter (slot awrap-type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `(value (wrapped-object ,awrap-type-name)))
     (setf (plus-c:c-ref wrapped-object ,awrap-type-name ,(kwd (s-name slot)))
           value)))

(defun make-eprim-slot-setter (slot awrap-type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `(value (wrapped-object ,awrap-type-name)))
     (let ((ptr (plus-c:c-ref wrapped-object ,awrap-type-name
                              ,(kwd (s-name slot)) plus-c::&)))
       (setf (mem-ref ptr ,(s-type slot)) value))))

(defun make-t-slot-setter (slot awrap-type-name)
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,awrap-type-name)))
     (populate (,(s-reader slot) wrapped-object) value)))

(defun make-array-slot-setter (slot type-name awrap-type-name)
  (declare (ignore type-name))
  `(,(s-def slot) (setf ,(s-writer slot))
     ,(s-slot-args slot `((value list) (wrapped-object ,awrap-type-name)))
     (c-populate (,(s-reader slot) wrapped-object) value)))

;;------------------------------------------------------------

(defun vertex-stream-comptible-typep (slot)
  (let ((type (type-spec->type (s-type slot))))
    (core-typep type)))

(defun make-struct-attrib-assigner (type-name slots)
  (when (every #'vertex-stream-comptible-typep slots)
    (let* ((stride (if (> (length slots) 1)
                       `(gl-type-size ',type-name)
                       0))
           (stride-sym (gensym "stride"))
           (definitions
            (loop :for attr :in (mapcat #'expand-slot-to-layout slots)
               :for i :from 0 :with offset = 0 :append
               `((gl:enable-vertex-attrib-array (+ attrib-offset ,i))
                 (%gl:vertex-attrib-pointer
                  (+ attrib-offset ,i) ,@attr ,stride-sym
                  (cffi:make-pointer (+ ,offset pointer-offset))))
               :do (setf offset (+ offset
                                   (* (first attr)
                                      (gl-type-size (second attr))))))))
      (when definitions
        `(progn
           (defmethod gl-assign-attrib-pointers ((array-type (EQL ',type-name))
                                                 &optional (attrib-offset 0)
                                                   (pointer-offset 0)
                                                   stride-override normalised)
             (declare (ignore array-type normalised))
             (let ((,stride-sym (or stride-override ,stride)))
               ,@definitions
               ,(length definitions))))))))

(defun expand-slot-to-layout (slot &optional type normalise)
  (let ((type (or type (type-spec->type (s-type slot))))
        (normalise (or normalise (when slot (s-normalisedp slot)))))
    (cond ((v-typep type 'v-matrix)
           (let ((v-type (type-spec->type
                          (kwd :vec (second (v-dimensions type))))))
             (setf (slot-value v-type 'varjo::element-type)
                   (type->spec (v-element-type type))) ;ugh
             (loop for i below (first (v-dimensions type))
                :append (expand-slot-to-layout nil v-type normalise))))
          ((v-typep type 'v-vector)
           `((,(apply #'* (v-dimensions type))
               ,(type->spec (v-element-type type))
               ,normalise)))
          ((v-typep type 'v-array)
           (loop for i below (apply #'* (v-dimensions type))
              :append (expand-slot-to-layout
                       nil (v-element-type type) normalise)))
          (t `((1 ,(type->spec type) ,normalise))))))

;;------------------------------------------------------------

(defun make-pull-push (autowrap-name slots)
  `((defmethod pull-g ((object ,autowrap-name))
      (list
       ,@(loop :for slot :in slots :for i :from 0 :collect
            `(,(s-writer slot) object))))
    (defmethod pull1-g ((object ,autowrap-name))
      (list
       ,@(loop :for slot :in slots :for i :from 0 :collect
            `(,(s-writer slot) object))))
    (defmethod push-g ((object list) (destination ,autowrap-name))
      (populate destination object))))

;;------------------------------------------------------------

(defun make-populate (autowrap-name slots)
  `(defmethod populate ((object ,autowrap-name) data)
     (unless (or (vectorp data) (listp data))
       (error "can only populate a struct of type ~a with a list or an array"
              ',autowrap-name))
     ,@(loop :for slot :in slots :for i :from 0 :collect
          `(setf (,(s-writer slot) object) (elt data ,i)))
     object))

;;------------------------------------------------------------

(defun make-struct-pixel-format (name slots)
  (let* ((type (s-type (first slots)))
         (len (length slots)))
    (when (< len 5)
      (let ((components (cepl-utils:kwd (subseq "RGBA" 0 len))))
        (when (and (loop for i in slots always (eql (s-type i) type))
                   (valid-pixel-format-p components type t nil))
          `(defmethod lisp-type->pixel-format ((type (eql ',name)))
             (pixel-format ,components ',type)))))))

;;------------------------------------------------------------
