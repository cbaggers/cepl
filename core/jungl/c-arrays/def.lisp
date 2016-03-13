(in-package :jungl)

;;------------------------------------------------------------

(defstruct (c-array (:constructor %make-c-array))
  (pointer
   (error "cepl: c-array must be created with a pointer")
   :type cffi-sys:foreign-pointer)
  (dimensions
   (error "cepl: c-array must be created with dimensions")
   :type list)
  (element-type
   (error "cepl: c-array must be created with an element-type")
   :type symbol)
  (element-byte-size
   (error "cepl: c-array must be created with an element-byte-size")
   :type fixnum)
  (struct-element-typep nil :type boolean)
  (row-byte-size
   (error "cepl: c-array must be created with a pointer")
   :type fixnum)
  (element-pixel-format nil :type (or null pixel-format))
  (element-from-foreign
   (error "cepl: c-array must be created with a from-foreign function")
   :type function)
  (element-to-foreign
   (error "cepl: c-array must be created with a to-foreign function")
   :type (function (foreign-pointer t) t)))

(defmethod pointer ((array c-array))
  (c-array-pointer array))

(defmethod dimensions ((array c-array))
  (c-array-dimensions array))

(defmethod element-type ((array c-array))
  (c-array-element-type array))

(defmethod element-byte-size ((array c-array))
  (c-array-element-byte-size array))

;; (defmethod row-byte-size ((array c-array))
;;   (c-array-row-byte-size array))

;; (defmethod element-pixel-format ((array c-array))
;;   (c-array-element-pixel-format array))


(defun blank-c-array-object (c-array)
  (setf (c-array-pointer c-array) (cffi:null-pointer))
  (setf (c-array-dimensions c-array) nil)
  (setf (c-array-element-type c-array) nil)
  (setf (c-array-element-byte-size c-array) 0)
  (setf (c-array-row-byte-size c-array) 0)
  (setf (c-array-element-pixel-format c-array) nil))

(defmethod free ((object c-array))
  (free-c-array object))

(defun free-c-array (c-array)
  "Frees the specified c-array."
  (foreign-free (c-array-pointer c-array))
  (blank-c-array-object c-array))

(defmethod print-object ((object c-array) stream)
  (format stream "#<C-ARRAY :element-type ~s :dimensions ~a>"
          (c-array-element-type object)
          (c-array-dimensions object)))

(defmethod print-mem ((thing c-array) &optional (size-in-bytes 64) (offset 0))
  (cepl-utils::%print-mem
   (cffi:inc-pointer (c-array-pointer thing) offset)
   size-in-bytes))

;;------------------------------------------------------------

(declaim (ftype (function (c-array) fixnum) c-array-rank))
(defun c-array-rank (c-array)
  (the fixnum (length (c-array-dimensions c-array))))
