(in-package :cepl)
;;------------------------------------------------------------

(defstruct (foooo (:conc-name nil)
                  (:constructor cepl.hidden::cepl.types.foooo.make
                                (cepl.hidden::cepl.types.foooo.ptr))
                  (:copier nil))
  (cepl.hidden::cepl.types.foooo.ptr (null-pointer) :type foreign-pointer
                                     :read-only t))

(defn populate-foooo-ptr (ptr (data foooo)) foreign-pointer
  (setf (mem-ref (foreign-slot-pointer ptr '(:struct cepl.hidden::cepl.types.foooo.foreign) 'a) :int)
        (foooo-a value))
  (setf (mem-ref (foreign-slot-pointer ptr '(:struct cepl.hidden::cepl.types.foooo.foreign) 'a) :vec4)
        (foooo-b value))
  (let* ((slot-ptr (foreign-slot-pointer  ptr '(:struct cepl.hidden::cepl.types.foooo.foreign) 'c))
         (c-arr (cepl.c-arrays:make-c-array-from-pointer '(10) ':vec2 slot-ptr)))
    (cepl.c-arrays::c-populate c-arr (foooo-c value)))
  ptr)

;;------------------------------------------------------------

(defcstruct cepl.hidden::cepl.types.foooo.foreign
  (a :int)
  (b :vec4)
  (c (:array :vec2 10)))

(define-foreign-type cepl.hidden::cepl.types.foooo.cffi-ct-type nil nil
                     (:actual-type :struct
                                   cepl.hidden::cepl.types.foooo.foreign)
                     (:simple-parser foooo))

(defmethod translate-from-foreign (ptr (type cepl.hidden::cepl.types.foooo.cffi-ct-type))
  (cepl.hidden::cepl.types.foooo.from-foreign ptr))

(defmethod expand-from-foreign
    (ptr (type cepl.hidden::cepl.types.foooo.cffi-ct-type))
  (list 'cepl.hidden::cepl.types.foooo.from-foreign ptr))

(defn-inline cepl.hidden::cepl.types.foooo.from-foreign ((ptr foreign-pointer)) foooo
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-foooo
   :a (foreign-slot-value ptr '(:struct cepl.hidden::cepl.types.foooo.foreign) 'a)
   :b (foreign-slot-value ptr '(:struct cepl.hidden::cepl.types.foooo.foreign) 'b)
   :c (let ((slot-ptr (foreign-slot-pointer ptr '(:struct cepl.hidden::cepl.types.foooo.foreign) 'c)))
        (cepl.c-arrays:make-c-array-from-pointer '(10) ':vec2 slot-ptr))))

(defn-inline cepl.hidden::cepl.types.foooo.to-foreign
    ((ptr foreign-pointer) (value t))
    t
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (populate-foooo-ptr ptr value))

(defmethod get-typed-from-foreign ((type-name (eql 'foooo)))
  #'cepl.hidden::cepl.types.foooo.from-foreign)

(defmethod get-typed-to-foreign ((type-name (eql 'foooo)))
  #'cepl.hidden::cepl.types.foooo.to-foreign)

;;------------------------------------------------------------

(defstruct-g blerp
  (a :vec2)
  (b (:vec3 10)))

;; (qualified-struct-name `(:struct ,foreign-struct-name))
