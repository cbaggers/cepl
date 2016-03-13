(in-package :jungl)

(defmacro make-typed-from-foreign ()
  (let ((types (append (list :int8 :uint8 :byte :ubyte
			     :short :ushort :int :uint
			     :float :double)
		       (mapcar #'first cffi::*extra-primitive-types*))))
    `(progn
       ,@(loop :for type :in types
	    :for from = (cepl-utils:symb type '-from-foreign)
	    :for to = (cepl-utils:symb type '-to-foreign) :append
	    `((declaim (inline ,from))
	      (defun ,from (ptr)
		(declare (type cffi:foreign-pointer ptr)
			 (optimize (speed 3) (safety 0) (debug 0)))
		(mem-aref ptr ,type))
	      (defun ,to (ptr value)
		(declare (type cffi:foreign-pointer ptr)
			 (optimize (speed 3) (safety 0) (debug 0)))
		(setf (mem-aref ptr ,type) value))
	      (defmethod get-typed-from-foreign ((type-name (eql ',type)))
		#',from)
	      (defmethod get-typed-to-foreign ((type-name (eql ',type)))
		#',to))))))

(make-typed-from-foreign)

;;----------------------------------------------------------------
;; handle types with longhand names

(defmethod get-typed-from-foreign ((type-name (eql (quote :unsigned-int))))
  #'uint-from-foreign)
(defmethod get-typed-to-foreign ((type-name (eql (quote :unsigned-int))))
  #'uint-to-foreign)

(defmethod get-typed-from-foreign ((type-name (eql (quote :unsigned-byte))))
  #'ubyte-from-foreign)
(defmethod get-typed-to-foreign ((type-name (eql (quote :unsigned-byte))))
  #'ubyte-to-foreign)

(defmethod get-typed-from-foreign ((type-name (eql (quote :unsigned-short))))
  #'ushort-from-foreign)
(defmethod get-typed-to-foreign ((type-name (eql (quote :unsigned-short))))
  #'ushort-to-foreign)

;;----------------------------------------------------------------

(defcfun (%memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))
