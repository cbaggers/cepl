(in-package :cgl)

;; [TODO] payload is an uncommited value, so if you have no pointer and
;;        you set the object you will populate the payload.
;;        if you then (setf (aref-c a 0) gl-val) you would apply the 
;;        payload. If there is a pointer you set and get straight from
;;        the foreign data.
(defclass c-value ()
  ((pointer :initform nil :initarg :pointer :reader pointer)
   (element-type :initarg :element-type :reader element-type)
   (lisp-payload :initform nil)))

(defclass extended-c-value (c-value)
  ((pointer :initform nil :initarg :pointer :reader pointer)
   (extended-type :initform nil :initarg :extended-type :reader extended-type)
   (element-type :initarg :element-type :reader element-type)
   (lisp-payload :initform nil)))

(defmethod print-object ((object c-value) stream)
  (format stream "#<C-VALUE type: ~a>"
          (slot-value object 'element-type)))

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
