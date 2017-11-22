(in-package :cffi)

;;----------------------------------------------------------------

(define-foreign-type gl-half-float () ()
                     (:actual-type :ushort)
                     (:simple-parser :half-float))

(ieee-floats:make-float-converters encode-half-float decode-half-float
                                   5 10 t)

(defmethod expand-to-foreign (value (type gl-half-float))
  `(encode-half-float ,value))

(defmethod expand-from-foreign (value (type gl-half-float))
  `(decode-half-float ,value))

(defmethod translate-to-foreign (value (type gl-half-float))
  (declare (type float value))
  (encode-half-float value))

(defmethod translate-from-foreign (ptr (type gl-half-float))
  (decode-half-float ptr))

;;----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *extra-primitive-types*
    '((:vec2 2 :float single-float)
      (:vec3 3 :float single-float)
      (:vec4 4 :float single-float)
      (:half-vec2 2 :half-float single-float)
      (:half-vec3 3 :half-float single-float)
      (:half-vec4 4 :half-float single-float)
      (:ivec2 2 :int (signed-byte 32))
      (:ivec3 3 :int (signed-byte 32))
      (:ivec4 4 :int (signed-byte 32))
      (:uvec2 2 :uint (unsigned-byte 32))
      (:uvec3 3 :uint (unsigned-byte 32))
      (:uvec4 4 :uint (unsigned-byte 32))
      (:mat2 4 :float single-float)
      (:mat3 9 :float single-float)
      (:mat4 16 :float single-float)
      (:mat2x2 4 :float single-float)
      (:mat2x3 6 :float single-float)
      (:mat2x4 8 :float single-float)
      (:mat3x2 6 :float single-float)
      (:mat3x3 9 :float single-float)
      (:mat3x4 12 :float single-float)
      (:mat4x2 8 :float single-float)
      (:mat4x3 12 :float single-float)
      (:mat4x4 16 :float single-float)
      (:uint8-vec2 2 :uint8 (unsigned-byte 8))
      (:uint8-vec3 3 :uint8 (unsigned-byte 8))
      (:uint8-vec4 4 :uint8 (unsigned-byte 8))
      (:int8-vec2 2 :int8 (signed-byte 8))
      (:int8-vec3 3 :int8 (signed-byte 8))
      (:int8-vec4 4 :int8 (signed-byte 8)))))

(defmacro make-new-types ()
  (labels ((get-lisp-type (f-type)
             (case f-type
               (bool 'boolean) ;;{TODO} why not :bool?
               (:int 'integer)
               (:uint 'integer)
               (:double 'float)
               (:int8 '(signed-byte 8))
               (:uint8 '(unsigned-byte 8))
               (:float 'single-float)
               (:half-float 'single-float)
               (t (error "How is there a cffi type with components of ~a" f-type)))))
    `(progn
       ,@(loop :for (type len comp-type) :in *extra-primitive-types*
            :collect
            (let* ((name (cepl-utils:symb 'cepl- type))
                   (type-name (cepl-utils:symb name '-type)))
              `(progn
                 (cffi:defcstruct ,name (components ,comp-type :count ,len))
                 (define-foreign-type ,type-name ()
                   ()
                   (:actual-type :struct ,name)
                   (:simple-parser ,type))
                 (defmethod translate-from-foreign (ptr (type ,type-name))
                   (make-array ,len :element-type ',(get-lisp-type comp-type)
                               :initial-contents
                               (list ,@(loop :for j :below len :collect
                                          `(mem-aref ptr ,comp-type ,j)))))
                 (defmethod translate-into-foreign-memory
                     (value (type ,type-name) pointer)
                   ,@(loop :for j :below len :collect
                        `(setf (mem-aref pointer ,comp-type ,j) (aref value ,j))))
                 ,(when (< len 5)
                    (let ((components (cepl-utils:kwd (subseq "RGBA" 0 len))))
                      (when (cepl.pixel-formats::valid-pixel-format-p
                             components comp-type t nil)
                        `(defmethod cepl.types:lisp-type->pixel-format ((comp-type (eql ,type)))
                           (cepl.pixel-formats::pixel-format!
                            ,components ',comp-type)))))
                 (defmethod expand-into-foreign-memory
                     (value (type ,type-name) ptr)
                   (cons 'progn
                         (loop :for j :below ,len :collect
                            `(setf (mem-aref ,ptr ,,comp-type ,j)
                                   (aref ,value ,j)))))

                 (defmethod expand-from-foreign (ptr (type ,type-name))
                   (list 'make-array ,len :element-type '',(get-lisp-type comp-type)
                         :initial-contents
                         (list 'list
                               ,@(loop :for j :below len :collect
                                    `(list 'mem-aref ptr ',comp-type ,j)))))))))))
(make-new-types)
