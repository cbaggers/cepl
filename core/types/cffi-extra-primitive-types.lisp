(in-package :cffi)

;;----------------------------------------------------------------

(define-foreign-type gl-half-float () ()
                     (:actual-type :ushort)
                     (:simple-parser :half-float))

(defmethod expand-to-foreign (value (type gl-half-float))
  `(%cepl.types::encode-half-float ,value))

(defmethod expand-from-foreign (value (type gl-half-float))
  `(%cepl.types::decode-half-float ,value))

(defmethod translate-to-foreign (value (type gl-half-float))
  (declare (type float value))
  (%cepl.types::encode-half-float value))

(defmethod translate-from-foreign (ptr (type gl-half-float))
  (%cepl.types::decode-half-float ptr))

;;----------------------------------------------------------------

(defmacro make-new-types ()
  (labels ((get-lisp-type (f-type)
             (case f-type
               (bool 'boolean) ;;{TODO} why not :bool?
               (:int '(signed-byte 32))
               (:uint '(unsigned-byte 32))
               (:double 'double-float)
               (:int8 '(signed-byte 8))
               (:uint8 '(unsigned-byte 8))
               (:float 'single-float)
               (:half-float 'single-float)
               (t (error "How is there a cffi type with components of ~a" f-type)))))
    `(progn
       ,@(loop :for (type len comp-type) :in %cepl.types::*extra-primitive-types*
            :collect
            (let* ((name (cepl-utils:symb 'cepl- type))
                   (type-name (cepl-utils:symb name '-type)))
              `(progn
                 (defcstruct ,name (components ,comp-type :count ,len))
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
