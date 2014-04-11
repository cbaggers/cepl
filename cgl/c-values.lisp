(in-package :cgl)

;; [TODO] payload is an uncommited value, so if you have no pointer and
;;        you set the object you will populate the payload.
;;        if you then (setf (aref-c a 0) gl-val) you would apply the 
;;        payload. If there is a pointer you set and get straight from
;;        the foreign data.
(defclass c-value ()
  ((pointer :initform nil :initarg :pointer :reader pointer)
   (element-type :initarg :element-type :reader element-type)
   ;;(lisp-payload :initform nil)
   ))

(defmethod print-object ((object c-value) stream)
  (format stream "#<C-VALUE type: ~a>"
          (slot-value object 'element-type)))
