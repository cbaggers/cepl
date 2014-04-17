(in-package :cgl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gl-struct-slot ()
    ((name :initarg :name :reader s-name) 
     (type :initarg :type :reader s-type)
     (element-type :initarg :element-type :reader s-element-type)
     (dimensions :initarg :dimensions :initform 1 :reader s-dimensions)
     (normalised :initarg :normalised :reader s-normalisedp) 
     (reader :initarg :reader :reader s-reader)
     (writer :initarg :writer :reader s-writer))))
(defmethod s-arrayp ((object gl-struct-slot)) 
  (eq (s-type object) :array))
(defmethod s-prim-p ((object gl-struct-slot))
  (not (null (member (s-type object) cffi::*built-in-foreign-types*))))
(defmethod s-extra-prim-p ((object gl-struct-slot))
  (not (null (assoc (s-type object) cffi::*extra-primitive-types*))))

;;------------------------------------------------------------

(defmacro mapcar1 (function list &rest args)
  (let ((i (gensym "i")))
    `(loop :for ,i :in ,list :collect (funcall ,function ,i ,@args))))
(defun type->spec (type)
  (let ((spec (type->type-spec type)))
    (if (listp spec)
        `(,(if (core-typep (type-spec->type (first spec)))
               (symbolicate-package "KEYWORD" (subseq (symbol-name (first spec)) 2))
               (first spec))
           ,(if (and (listp (second spec)) (= (length (second spec)) 1))
                (first (second spec)) (second spec)))
        (if (core-typep type)
            (symbolicate-package "KEYWORD" (subseq (symbol-name spec) 2))
            spec))))

;;------------------------------------------------------------

;; (defglstruct jeff () (z :mat2))
;; (defglstruct toast () (a :float))
;; (defglstruct jim ()
;;   (a :float)
;;   (b :vec2))

(defmacro defglstruct (name (&key no-accesors) &body slot-descriptions)  
  (let ((slots (mapcar1 #'normalize-slot-description slot-descriptions 
                        name no-accesors))
        (autowrap-name (symb name '_)))
    (when (validate-defglstruct-form name slots)      
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(make-autowrap-record-def autowrap-name slots))
         (autowrap:define-wrapper (:struct (,autowrap-name)))
         ,(make-varjo-struct-def name slots)
         ,(make-cstruct-def name slots) ; {TODO} what do we need this for?
         ,(make-make-struct name autowrap-name)
         ,@(remove nil (mapcar1 #'make-slot-getter slots name autowrap-name))
         ,@(remove nil (mapcar1 #'make-slot-setter slots name autowrap-name))
         ,(make-struct-attrib-assigner name slots)
         ,(make-struct-pixel-format name slots)
         ;; ,(make-populate slots)
         ',name))))

(defun normalize-slot-description (slot-description type-name no-accesors)
  (destructuring-bind (name type &key normalised accessor) slot-description
    (let* ((type (listify type))
           (dimensions (listify (or (second type) 1)))
           (arrayp (> (first dimensions) 1))
           (element-type (when arrayp (first type)))
           (type (if arrayp :array (first type))))
      (make-instance 'gl-struct-slot :name name :type type :normalised normalised
                     :reader (unless no-accesors 
                               (or accessor (symb type-name '- name)))
                     :writer (unless no-accesors 
                               (or accessor (symb type-name '- name)))
                     :element-type element-type :dimensions dimensions))))

;; put all cepl's errors defintions in one place (like varjo)
(defun validate-defglstruct-form (name slots)
  (when (keywordp name) (error "glstruct names cannot be keywords"))
  (when (null slots) (error "glstruct must have at least 1 slot"))
  t)

;;------------------------------------------------------------

(defun make-varjo-struct-def (name slots)
  `(v-defstruct ,name () ,@(mapcar #'format-slot-for-varjo slots)))

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

(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     ,@(mapcar #'format-slot-for-cstruct slots)))

(defun format-slot-for-cstruct (slot)
  (when (> (length (s-dimensions slot)) 1) 
    (error "cannot produce multidimensional array slots yet"))
  (cond ((s-arrayp slot) (format-array-slot-for-cstruct slot))
        ((or (s-extra-prim-p slot) (s-prim-p slot)) 
         (format-regular-slot-for-cstruct slot))
        (t (format-struct-slot-for-cstruct slot))))

(defun format-regular-slot-for-cstruct (slot)
  `(,(s-name slot)
     ,(s-type slot)
     :count 1
     ;; :offset {TODO} when do we need offset?
     ))

(defun format-struct-slot-for-cstruct (slot)
  `(,(s-name slot)
     (:struct ,(s-type slot))
     :count 1
     ;; :offset {TODO} when do we need offset?
     ))

(defun format-array-slot-for-cstruct (slot)
  `(,(s-name slot)
     ,(s-element-type slot)
     :count ,(reduce #'* (s-dimensions slot))
     ;; :offset {TODO} when do we need offset?
     ))

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
                     (symbolicate-package :cffi :cgl- (s-type slot))
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
                     (symbolicate-package :cffi :cgl- (s-element-type slot))
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
(defun make-make-struct (name awrap-type-name)
  `(defun ,(symb 'make- name) ()
     (autowrap:alloc ',awrap-type-name)))

;;------------------------------------------------------------

(defun make-slot-getter (slot type-name awrap-type-name)
  (when (s-reader slot)
    (cond 
      ((member (s-type slot) cffi:*built-in-foreign-types*)
       (make-prim-slot-getter slot type-name awrap-type-name))
      ((assoc (s-type slot) cffi::*extra-primitive-types*)
       (make-eprim-slot-getter slot type-name awrap-type-name))
      ((not (s-arrayp slot)) (make-t-slot-getter slot type-name awrap-type-name))
      ((s-arrayp slot) (make-array-slot-getter slot type-name awrap-type-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun make-prim-slot-getter (slot type-name awrap-type-name)  
  `(defmethod ,(s-reader slot) ((wrapped-object ,type-name))
     (plus-c:c-ref wrapped-object ,awrap-type-name ,(kwd (s-name slot)))))

(defun make-eprim-slot-getter (slot type-name awrap-type-name)
  `(defmethod ,(s-reader slot) ((wrapped-object ,type-name))
     (mem-ref (plus-c:c-ref wrapped-object ,awrap-type-name 
                            ,(kwd (s-name slot)) plus-c::&)
              ,(s-type slot))))

(defun make-t-slot-getter (slot type-name awrap-type-name)
  `(defmethod ,(s-reader slot) ((wrapped-object ,type-name))
     (plus-c:c-ref wrapped-object ,awrap-type-name 
                   ,(kwd (s-name slot)))))

(defun make-array-slot-getter (slot type-name awrap-type-name)
  `(defmethod ,(s-reader slot) ((wrapped-object ,type-name)) 
     t)) ;;{TODO} return a c-array, not a lisp array

;; cffi prim - if in cffi::*built-in-foreign-types* then ok
;; extra-prims - 
;; structure -
;; array prim - 
;; array extra-prims - 
;; array array - not currently supported
;; array structure

(defun make-slot-setter (slot type-name awrap-type-name)
  (when (s-reader slot)
    (cond 
      ((member (s-type slot) cffi:*built-in-foreign-types*)
       (make-prim-slot-setter slot type-name awrap-type-name))
      ((assoc (s-type slot) cffi::*extra-primitive-types*)
       (make-eprim-slot-setter slot type-name awrap-type-name))
      ((not (s-arrayp slot)) (make-t-slot-setter slot type-name awrap-type-name))
      ((s-arrayp slot) (make-array-slot-setter slot type-name awrap-type-name))
      (t (error "Dont know what to do with slot ~a" slot)))))

(defun make-prim-slot-setter (slot type-name awrap-type-name)
  `(defmethod (setf ,(s-reader slot)) (value (wrapped-object ,type-name))
     (setf (plus-c:c-ref wrapped-object ,awrap-type-name ,(kwd (s-name slot)))
           value)))

(defun make-eprim-slot-setter (slot type-name awrap-type-name)
  `(defmethod (setf ,(s-reader slot)) (value (wrapped-object ,type-name))
     (setf (mem-ref (plus-c:c-ref wrapped-object ,awrap-type-name 
                                  ,(kwd (s-name slot)) plus-c::&)
                    ,(s-type slot))
           value)))

(defun make-t-slot-setter (slot type-name awrap-type-name)
  `(defmethod (setf ,(s-reader slot)) (value (wrapped-object ,type-name))
     (error "Setting of 'slots with a type of struct' is not currently implemented ")))

(defun make-array-slot-setter (slot type-name awrap-type-name)
  `(defmethod (setf ,(s-reader slot)) ((wrapped-object ,type-name)) 
     t)) ;;{TODO} implement me


;;------------------------------------------------------------

(defun vertex-stream-comptible-typep (slot)
  (let ((type (type-spec->type (s-type slot))))
    (and (core-typep type) ;(not (v-typep type 'v-matrix))
         )))

(defun make-struct-attrib-assigner (type-name slots)
  (when (every #'vertex-stream-comptible-typep slots)
    (let* ((stride (if (> (length slots) 1)
                       `(cffi:foreign-type-size ',type-name)
                       0))
           (stride-sym (gensym "stride"))
           (definitions             
            (loop :for attr :in (mapcan #'expand-slot-to-layout slots)
               :for i :from 0 :with offset = 0 :append
               `((gl:enable-vertex-attrib-array (+ attrib-offset ,i))
                 (%gl:vertex-attrib-pointer 
                  (+ attrib-offset ,i) ,@attr ,stride-sym 
                  (cffi:make-pointer (+ ,offset pointer-offset))))
               :do (setf offset (+ offset
                                   (* (first attr) 
                                      (cffi:foreign-type-size
                                       (second attr))))))))
      (when definitions
        `(defmethod gl-assign-attrib-pointers ((array-type (EQL ',type-name)) 
                                               &optional (attrib-offset 0)
                                                 (pointer-offset 0)
                                                 stride-override normalised)
           (declare (ignore array-type normalised))
           (let ((,stride-sym (or stride-override ,stride)))
             ,@definitions
             ,(length definitions)))))))

(defun expand-slot-to-layout (slot &optional type normalise)
  (let ((type (or type (type-spec->type (s-type slot))))
        (normalise (or normalise (when slot (s-normalisedp slot)))))
    (cond ((v-typep type 'v-matrix)
           (let ((v-type (type-spec->type
                          (kwd :vec (second (v-dimensions type))))))
             (setf (slot-value v-type 'varjo::element-type) 
                   (type->spec (v-element-type type))) ;ugh
             (loop for i below (first (v-dimensions type))
                :append (expand-slot-to-layout
                         nil 
                         v-type 
                         normalise))))
          ((v-typep type 'v-vector)  
           `((,(apply #'* (v-dimensions type))
               ,(type->spec (v-element-type type)) 
               ,normalise)))
          ((v-typep type 'v-array) 
           (loop for i below (apply #'* (v-dimensions type))
              :append (expand-slot-to-layout 
                       nil (v-element-type type) normalise)))
          (t `((1 ,(type->spec (type-principle type)) ,normalise))))))

;;------------------------------------------------------------

(defun make-struct-pixel-format (name slots)
  (let* ((type (s-type (first slots)))
         (len (length slots))
         (components (utils:kwd (subseq "RGBA" 0 len))))
    (when (and (< len 5) (loop for i in slots always (eql (s-type i) type))
               (valid-pixel-format-p components type t nil))
      `(defmethod pixel-format-of ((type (eql ',name)))
         (pixel-format ,components ',type)))))
