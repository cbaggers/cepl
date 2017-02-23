(in-package :cepl.types)

(defgeneric get-typed-from-foreign (type-name))
(defgeneric get-typed-to-foreign (type-name))

(defmethod get-typed-from-foreign ((type-name t))
  (format t "~%No optimized from-foreign found for ~a~%" type-name)
  (lambda (ptr) (mem-ref ptr type-name)))

(defmethod get-typed-to-foreign ((type-name t))
  (error "~%No optimized from-foreign found for ~a~%" type-name))

;;----------------------------------------------------------------

(defmacro make-typed-from-foreign ()
  (let ((types (append `((:int8 0 nil nil)
                         (:uint8 0 nil nil)
                         (:short 0 nil nil)
                         (:ushort 0 nil nil)
                         (:int 0 nil nil)
                         (:uint 0 nil nil)
                         (:float 0 nil nil)
                         (:double 0 nil nil)
                         (:half-float 0 nil nil))
                       cffi::*extra-primitive-types*)))
    `(progn
       ,@(loop :for (type len nil comp-lisp-type) :in types
            :for from = (cepl-utils:symb-package
                         :cepl.types.foreign type '-from-foreign)
            :for to = (cepl-utils:symb-package
                       :cepl.types.foreign type '-to-foreign) :append
            `((declaim (inline ,from))
              (defun ,from (ptr)
                (declare (type cffi:foreign-pointer ptr)
                         (optimize (speed 3) (safety 0) (debug 0)))
                (mem-aref ptr ,type))
              (defun ,to (ptr value)
                (declare (type cffi:foreign-pointer ptr)
                         (optimize (speed 3) (safety 0) (debug 0))
                         ,@(when (> len 0) `((type (simple-array
                                                    ,comp-lisp-type (,len))
                                                   value))))
                (setf (mem-aref ptr ,type) value))
              (export '(,to ,from) :cepl.types.foreign)
              (defmethod get-typed-from-foreign ((type-name (eql ',type)))
                #',from)
              (defmethod get-typed-to-foreign ((type-name (eql ',type)))
                #',to))))))

(make-typed-from-foreign)

;;----------------------------------------------------------------
;; handle types with longhand names

(defmethod get-typed-from-foreign ((type-name (eql (quote :unsigned-int))))
  #'cepl.types.foreign::uint-from-foreign)
(defmethod get-typed-to-foreign ((type-name (eql (quote :unsigned-int))))
  #'cepl.types.foreign::uint-to-foreign)

(defmethod get-typed-from-foreign ((type-name (eql (quote :unsigned-byte))))
  #'cepl.types.foreign::uint8-from-foreign)
(defmethod get-typed-to-foreign ((type-name (eql (quote :unsigned-byte))))
  #'cepl.types.foreign::uint8-to-foreign)

(defmethod get-typed-from-foreign ((type-name (eql (quote :unsigned-short))))
  #'cepl.types.foreign::ushort-from-foreign)
(defmethod get-typed-to-foreign ((type-name (eql (quote :unsigned-short))))
  #'cepl.types.foreign::ushort-to-foreign)

;;----------------------------------------------------------------

(defcfun (%memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))
