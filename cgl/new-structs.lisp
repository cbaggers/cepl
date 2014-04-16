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

(defmacro mapcar1 (function list &rest args)
  (let ((i (gensym "i")))
    `(loop :for ,i :in ,list :collect (funcall ,function ,i ,@args))))

(defmacro defglstruct (name (&key no-accesors) &body slot-descriptions)  
  (let ((slots (mapcar1 #'normalize-slot-description slot-descriptions 
                        name no-accesors)))
    (unless (validate-defglstruct-form name slots))
    `(progn
       ,(make-varjo-struct-def name slots)
       ,(make-cstruct-def name slots) ; {TODO} what do we need this for?
       ,@(make-autowrap-record-def name slots)
       ,(make-make-struct name)
       ,@(remove nil (mapcar1 #'make-slot-getter slots name))
       ,@(remove nil (mapcar1 #'make-slot-setter slots name))
       ;; ,(make-populate slots)
       )))

(defglstruct jim () 
  (a :float)
  (b (:int 4)))

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
    (if (eq (s-type slot) :array)
        `(,(s-name slot)
           ,(validate-varjo-type-spec
             (list (s-element-type slot) (s-dimensions slot)))
           ,@(when accessor `(:accessor ,accessor)))
        `(,(s-name slot)
           ,(validate-varjo-type-spec (s-type slot))        
           ,@(when accessor `(:accessor ,accessor))))))

(defun validate-varjo-type-spec (spec)
  (type->type-spec (type-spec->type spec)))

;;------------------------------------------------------------

(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     ,@(mapcar #'format-slot-for-cstruct slots)))

(defun format-slot-for-cstruct (slot)
  (when (> (length (s-dimensions slot)) 1) 
    (error "cannot produce multidimensional array slots yet"))
  (if (eq (s-type slot) :array)
      `(,(s-name slot)
         ,(s-element-type slot)
         :count ,(reduce #'* (s-dimensions slot))
         ;; :offset {TODO} when do we need offset?
         )
      `(,(s-name slot)
         ,(s-type slot)
         :count 1
         ;; :offset {TODO} when do we need offset?
         )))

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
  (if (eq (s-type slot) :array)
      (format-array-slot-for-autowrap slot)
      (%format-slot-for-autowrap slot)))

(defun %format-slot-for-autowrap (slot)
  (let ((a-type (autowrap:find-type (s-type slot))))
    (list (kwd (s-name slot))
          (s-type slot)
          :bit-size (* 8 (autowrap:foreign-type-size a-type))
          :bit-offset :-offset-
          :bit-alignment 8)))

(defun format-array-slot-for-autowrap (slot)
  (when (> (length (s-dimensions slot)) 1)
    (error "Cannot currently support multi dimensional autowrap arrays"))
  (let ((a-type (autowrap:find-type (s-element-type slot))))    
    (list (kwd (s-name slot))
          `(:array ,(s-element-type slot) ,(reduce #'* (s-dimensions slot)))
          :bit-size (* (autowrap:foreign-type-size a-type)
                       (reduce #'* (s-dimensions slot))
                       8)
          :bit-offset :-offset-
          :bit-alignment 8)))

;;------------------------------------------------------------

(defglstruct jim () 
  (a :float)
  (b (:int 4)))

(V-DEFSTRUCT JIM
    NIL
  (A V-FLOAT :ACCESSOR JIM-A)
  (B (V-INT (4)) :ACCESSOR JIM-B))
(DEFCSTRUCT JIM
  (A :FLOAT :COUNT 1)
  (B :INT :COUNT 4))
(AUTOWRAP:DEFINE-FOREIGN-RECORD 'JIM :STRUCT 160 8
                                '((:A :FLOAT :BIT-SIZE 32 :BIT-OFFSET 0
                                   :BIT-ALIGNMENT 8)
                                  (:B (:ARRAY :INT 4) :BIT-SIZE 128
                                   :BIT-OFFSET 32 :BIT-ALIGNMENT 8)))
(AUTOWRAP:DEFINE-FOREIGN-ALIAS 'JIM '(:STRUCT (JIM)))
(DEFUN MAKE-JIM ()
  (LET ((TYPE (AUTOWRAP:FIND-TYPE 'JIM)))
    (AUTOWRAP:ALLOC TYPE 1)))
(DEFUN JIM-A (WRAPPED-OBJECT) (PLUS-C:C-REF WRAPPED-OBJECT 'JIM 'A))
(DEFUN JIM-B (WRAPPED-OBJECT) (PLUS-C:C-REF WRAPPED-OBJECT 'JIM 'B))

(defun make-make-struct (name)
  `(defun ,(symb 'make- name) ()
     (let ((type (autowrap:find-type ',name)))
       (autowrap:alloc type 1))))

(defun make-slot-getter (slot type-name)
  (when (s-reader slot)
    `(defun ,(s-reader slot) (wrapped-object)
       (plus-c:c-ref wrapped-object ',type-name ',(s-name slot)))))

(defun make-slot-setter (slot type-name)
  (when (s-writer slot)
    ;; `(defun ,(s-reader slot) (wrapped-object)
    ;;    (plus-c:c-ref wrapped-object '(:struct (,type-name)) ',(s-name slot)))
    ))



(AUTOWRAP:DEFINE-FOREIGN-RECORD 'SDL-VERSION :STRUCT 24 8
                                '((:MAJOR UINT8 :BIT-SIZE 8 :BIT-OFFSET 0
                                   :BIT-ALIGNMENT 8)
                                  (:MINOR UINT8 :BIT-SIZE 8 :BIT-OFFSET 8
                                   :BIT-ALIGNMENT 8)
                                  (:PATCH UINT8 :BIT-SIZE 8 :BIT-OFFSET 16
                                   :BIT-ALIGNMENT 8)))
(AUTOWRAP:DEFINE-FOREIGN-ALIAS 'SDL-VERSION '(:STRUCT (SDL-VERSION)))





;;------------------------------------------------------------
