(in-package :cepl.types)

;;----------------------------------------------------------------

(defvar *extra-cffi-type-equivalents*
  '((:bool boolean)
    (:int (signed-byte 32))
    (:uint (unsigned-byte 32))
    (:int8 (signed-byte 8))
    (:uint8 (unsigned-byte 8))
    (:half-float single-float)
    (:float single-float)
    (:double double-float)
    (:vec2 (simple-array single-float (2)))
    (:vec3 (simple-array single-float (3)))
    (:vec4 (simple-array single-float (4)))
    (:half-vec2 (simple-array single-float (2)))
    (:half-vec3 (simple-array single-float (3)))
    (:half-vec4 (simple-array single-float (4)))
    (:ivec2 (simple-array (signed-byte 32) (2)))
    (:ivec3 (simple-array (signed-byte 32) (3)))
    (:ivec4 (simple-array (signed-byte 32) (4)))
    (:uvec2 (simple-array (unsigned-byte 32) (2)))
    (:uvec3 (simple-array (unsigned-byte 32) (3)))
    (:uvec4 (simple-array (unsigned-byte 32) (4)))
    (:mat2 (simple-array single-float (4)))
    (:mat3 (simple-array single-float (9)))
    (:mat4 (simple-array single-float (16)))
    (:mat2x2 (simple-array single-float (4)))
    (:mat2x3 (simple-array single-float (6)))
    (:mat2x4 (simple-array single-float (8)))
    (:mat3x2 (simple-array single-float (6)))
    (:mat3x3 (simple-array single-float (9)))
    (:mat3x4 (simple-array single-float (12)))
    (:mat4x2 (simple-array single-float (8)))
    (:mat4x3 (simple-array single-float (12)))
    (:mat4x4 (simple-array single-float (16)))
    (:uint8-vec2 (simple-array (unsigned-byte 8) (2)))
    (:uint8-vec3 (simple-array (unsigned-byte 8) (3)))
    (:uint8-vec4 (simple-array (unsigned-byte 8) (4)))
    (:int8-vec2 (simple-array (signed-byte 8) (2)))
    (:int8-vec3 (simple-array (signed-byte 8) (3)))
    (:int8-vec4 (simple-array (signed-byte 8) (4)))))

(defun lisp-equivalent-of-keyword-cffi-type (name)
  (cadr (assoc name *extra-cffi-type-equivalents*)))

;;----------------------------------------------------------------

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
