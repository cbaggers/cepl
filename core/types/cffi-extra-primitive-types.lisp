(in-package :cffi)

(defconstant +allow-extra-keyword-type-names+ t)

;; {TODO} need to add info for autowrap

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *extra-primitive-types*
    '((:vec2 2 :float)
      (:vec3 3 :float)
      (:vec4 4 :float)
      (:ivec2 2 :int)
      (:ivec3 3 :int)
      (:ivec4 4 :int)
      (:uvec2 2 :uint)
      (:uvec3 3 :uint)
      (:uvec4 4 :uint)
      (:mat2 4 :float)
      (:mat3 9 :float)
      (:mat4 16 :float)
      (:mat2x2 4 :float)
      (:mat2x3 6 :float)
      (:mat2x4 8 :float)
      (:mat3x2 6 :float)
      (:mat3x3 9 :float)
      (:mat3x4 12 :float)
      (:mat4x2 8 :float)
      (:mat4x3 12 :float)
      (:mat4x4 16 :float)
      (:uint8-vec2 2 :uint8)
      (:uint8-vec3 3 :uint8)
      (:uint8-vec4 4 :uint8)
      (:int8-vec2 2 :int8)
      (:int8-vec3 3 :int8)
      (:int8-vec4 4 :int8))))

(defmacro make-new-types ()
  (labels ((get-lisp-type (f-type)
             (case f-type
               (bool 'boolean)
               (:int 'integer)
               (:uint 'integer)
               (:double 'float)
               (:float 'single-float)
	       (:int8 'fixnum)
	       (:uint8 'fixnum)
               (t (error "How is there a cffi type with components of ~a" f-type)))))
    (let* ((new-user-types *extra-primitive-types*)
           (kwd-allowed +allow-extra-keyword-type-names+))
      `(progn
         ,@(loop :for (type len comp-type) :in new-user-types
              :collect
              (let* ((name (cepl-utils:symb 'cepl- type))
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
                            (when (cepl.pixel-formats::valid-pixel-format-p
				   components comp-type t nil)
                              `(defmethod cepl.types:lisp-type->pixel-format ((comp-type (eql ,type)))
                                 (cepl.pixel-formats:pixel-format
				  ,components ',comp-type)))))
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

;; Extra functions, these probably need to live somewhere else
(defcfun (%memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))

(export '(%memcpy))
