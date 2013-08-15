(in-package :cffi)

(define-foreign-type cgl-byte () 
  ()
  (:actual-type :char)
  (:simple-parser :byte))

(define-foreign-type cgl-ubyte () 
  ()
  (:actual-type :uchar)
  (:simple-parser :ubyte))

(defmacro make-new-types ()
  (let* ((new-user-types '((:ubyte-vec2 2 :ubyte)
                           (:ubyte-vec3 3 :ubyte)
                           (:ubyte-vec4 4 :ubyte)
                           (:byte-vec2 2 :byte)
                           (:byte-vec3 3 :byte)
                           (:byte-vec4 4 :byte)))
         (varjo-types (loop :for (type . len) :in varjo::*glsl-component-counts*
                         :collecting (list type len 
                                           (varjo:type-component-type 
                                            (varjo:flesh-out-type type))))))
    `(progn 
       ,@(loop :for (type len comp-type) :in (append new-user-types varjo-types)
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
                        `(setf (mem-aref pointer ,comp-type ,j) (aref value ,j))))
                 ,(when (< len 5)
                        (let ((components (utils:kwd (subseq "RGBA" 0 len))))
                          (when (cgl::valid-pixel-format-p components comp-type t nil)
                            `(defmethod cgl::pixel-format-of ((comp-type (eql ,type)))
                               (cgl::pixel-format ,components ',comp-type)))))
                 ))))))
(make-new-types)

;; Extra functions, these probably need to live somewhere else
(defcfun (%memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))
