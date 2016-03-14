(in-package :cffi)

(defconstant +allow-extra-keyword-type-names+ t)

;; {TODO} need to add info for autowrap

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *extra-primitive-types*
    '((:vec2 2 :float single-float)
      (:vec3 3 :float single-float)
      (:vec4 4 :float single-float)
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
      (:ubyte-vec2 2 :ubyte (unsigned-byte 8))
      (:ubyte-vec3 3 :ubyte (unsigned-byte 8))
      (:ubyte-vec4 4 :ubyte (unsigned-byte 8))
      (:byte-vec2 2 :byte (signed-byte 8))
      (:byte-vec3 3 :byte (signed-byte 8))
      (:byte-vec4 4 :byte (signed-byte 8)))))

(define-foreign-type jungl-byte ()
  ()
  (:actual-type :int8)
  (:simple-parser :byte))

(define-foreign-type jungl-ubyte ()
  ()
  (:actual-type :uint8)
  (:simple-parser :ubyte))

(defmacro make-new-types ()
  (labels ((get-lisp-type (f-type)
             (case f-type
               (bool 'boolean)
               (:int 'integer)
               (:uint 'integer)
               (:double 'float)
               (:float 'single-float)
               (:byte 'fixnum)
               (:ubyte 'fixnum)
               (t (error "How is there a cffi type with components of ~a" f-type)))))
    (let* ((new-user-types *extra-primitive-types*)
           (kwd-allowed +allow-extra-keyword-type-names+))
      `(progn
         ,@(loop :for (type len comp-type) :in new-user-types
              :collect
              (let* ((name (cepl-utils:symb 'jungl- type))
                     (type-name (cepl-utils:symb name '-type))
                     (comp-bit-size (* 8 (cffi:foreign-type-size comp-type))))
                `(progn
                   (cffi:defcstruct ,name (components ,comp-type :count ,len))
                   (define-foreign-type ,type-name ()
                     ()
                     (:actual-type :struct ,name)
                     ,@(when kwd-allowed
			     `((:simple-parser ,type))))
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
                            (when (jungl:valid-pixel-format-p components comp-type t nil)
                              `(defmethod jungl:lisp-type->pixel-format ((comp-type (eql ,type)))
                                 (jungl:pixel-format ,components ',comp-type)))))

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
				      `(list 'mem-aref ptr ',comp-type ,j)))))

                   (autowrap:define-foreign-record
                       ',name
                       :struct
                     ,(* comp-bit-size len)
                     8
                     ',(loop :for i :below len :with offset = 0 :collect
                          `(,(if (<= len 4)
                                 (nth i '(:x :y :z :w))
                                 (cepl-utils:kwd 'slot- i))
                             ,comp-type :bit-size ,comp-bit-size
                             :bit-offset ,offset :bit-alignment 8)
                          :do (incf offset comp-bit-size)))
		   (autowrap:define-foreign-alias ',name
		       '(:struct (,name))))))))))
(make-new-types)


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
