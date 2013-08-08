(in-package :cffi)

(defmacro make-new-prims ()
  `(progn 
     ,@(loop :for (type . len) :in varjo::*glsl-component-counts* 
          :collect
          (let* ((ftype (varjo:flesh-out-type type))
                 (comp-len (varjo:type-component-count type))
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
                 (make-array ,comp-len :initial-contents
                             (list ,@(loop :for j :below comp-len :collect 
                                        `(mem-aref ptr ,comp-type ,j)))))
               (defmethod translate-into-foreign-memory
                   (value (type ,type-name) pointer)
                 ,@(loop :for j :below comp-len :collect 
                      `(setf (mem-aref pointer ,comp-type ,j) (aref value ,j)))))))))
(make-new-prims)


;; Extra functions, these probably need to live somewhere else
(defcfun (%memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))
