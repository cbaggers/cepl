
(in-package :cffi)

(defmacro make-non-glsl-types ()
  `(progn 
     ,@(loop :for (type len comp-type) :in '((:ubyte-vec2 2 :uchar)
                                             (:ubyte-vec3 3 :uchar)
                                             (:ubyte-vec4 4 :uchar)
                                             (:byte-vec2 2 :char)
                                             (:byte-vec3 3 :char)
                                             (:byte-vec4 4 :char))
          :collect
          (let* ((name (utils:symb 'cgl- type))
                 (type-name (utils:symb name '-type))) 
            `(progn
               (cffi:defcstruct ,name (components ,comp-type :count ,len))
               (define-foreign-type ,type-name () 
                 ()
                 (:actual-type :struct ,name)
                 (:simple-parser ,type))
               (defmethod translate-from-foreign (ptr (type ,type-name))
                 (make-array ,len :initial-contents
                             (list ,@(loop :for j :below len :collect 
                                        `(mem-aref ptr ,comp-type ,j)))))
               (defmethod translate-into-foreign-memory
                   (value (type ,type-name) pointer)
                 ,@(loop :for j :below len :collect 
                      `(setf (mem-aref pointer ,comp-type ,j) (aref value ,j)))))))))

(defmacro make-glsl-types ()
  `(progn 
     ,@(loop :for (type . len) :in varjo::*glsl-component-counts* 
          :collect
          (let* ((ftype (varjo:flesh-out-type type))
                 (comp-type (varjo:type-component-type ftype))
                 (name (utils:symb 'cgl- type))
                 (type-name (utils:symb name '-type))) 
            `(progn
               (cffi:defcstruct ,name (components ,comp-type :count ,len))
               (define-foreign-type ,type-name () 
                 ()
                 (:actual-type :struct ,name)
                 (:simple-parser ,type))
               (defmethod translate-from-foreign (ptr (type ,type-name))
                 (make-array ,len :initial-contents
                             (list ,@(loop :for j :below len :collect 
                                        `(mem-aref ptr ,comp-type ,j)))))
               (defmethod translate-into-foreign-memory
                   (value (type ,type-name) pointer)
                 ,@(loop :for j :below len :collect 
                      `(setf (mem-aref pointer ,comp-type ,j) (aref value ,j)))))))))
(make-non-glsl-types)
(make-glsl-types)


;; Extra functions, these probably need to live somewhere else
(defcfun (%memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))
