(in-package :cepl.c-arrays)

;;------------------------------------------------------------

(defgeneric pointer (c-array))
(defmethod pointer ((array c-array))
  (c-array-pointer array))

(defmethod dimensions ((array c-array))
  (c-array-dimensions array))

(defmethod element-type ((array c-array))
  (c-array-element-type array))

(defmethod element-byte-size ((array c-array))
  (c-array-element-byte-size array))

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
  (let ((ptr (c-array-pointer c-array)))
    (unless (cffi:null-pointer-p ptr)
      (foreign-free ptr)
      (blank-c-array-object c-array))))

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
