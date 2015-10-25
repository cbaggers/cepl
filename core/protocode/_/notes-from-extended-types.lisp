(defparameter test-type '(-> (-> 10) -> (:float 10)))

;; [TODO] is there anything else we may want from other slots?
;; (-> :int)
;; (-> (:int 10))
;; (-> -> (:int 10))
;; ((-> 10) :int)
;; ((-> slot-name) ())

;; (-> (-> 10) -> (:FLOAT 10))
;; what does it mean to have (-> 10) as the second item? which one do we pick?
;; seems that this should then spit out an enhanced array with the rest of the 
;; spec as its etype

;; it should be invalid to have any non pointer type is any pre-end position

;; Given up on this, so below is the dump

(defun compile-etype-part (parent-type type-spec)
  (labels ((swap-pointer (x) (if (sn-equal x '->) :pointer x)))
    (if (symbolp type-spec) 
        (list (swap-pointer type-spec))
        (dbind (type len) type-spec
          `(,(swap-pointer type) 
             ,(if (symbolp len) 
                  `(foreign-slot-value 
                    (pointer val) (foreign-slot-type ',parent-type 
                                                     ',len) ',len)
                  (or len 0)))))))

(defun compile-etype-spec (parent-type slot-type-spec)
  (unless (all-early-pointer-p slot-type-spec)
    (error "invalid extended-type spec"))
  (let* ((split-index (or (position-if #'listp slot-type-spec)
                          (length slot-type-spec)))         
         (type-spec (reverse (subseq slot-type-spec 0 split-index)))
         (final-type (or (subseq slot-type-spec split-index)
                         (first (last slot-type-spec))))
         (get-ptr-form 'ptr))
    (loop :for part :in type-spec :for i :from 0 :do
       (let ((new (compile-etype-part parent-type part)))         
         (setf get-ptr-form `(mem-ref ,get-ptr-form ,@new))))
    (list get-ptr-form 
          (if (listp final-type) 
              (compile-etype-part parent-type (first final-type))
              final-type))))

;;-- from c values
(defclass extended-c-value (c-value)
  ((pointer :initform nil :initarg :pointer :reader pointer)
   (extended-type :initform nil :initarg :extended-type :reader extended-type)
   (element-type :initarg :element-type :reader element-type)
   (lisp-payload :initform nil)))

;; what the fuck are we doing
;; ok so we have array of pointers to ints ((-> 10) :ints)
;; we take aref of this, what do we get? 
;; what is the goal, I'm assuming access to indirect data as if not indirect.
;; so we should get int.
;;  (aref-c (aref-c val n))
;; next array of pointer to structs ((-> 10) vert-data)
;; aref gives us what? well we should get a c-value pointing to the start of the
;; actual data.
;;  (aref-c (aref-c val n))
;; I'm not sure we need etypes at all, I think we just need to compile the type
;; spec into the get functions

;; right back to the start then
