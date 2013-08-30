(in-package :cgl)
;;------------------------------------------------------------

;; [TODO] Should add ability to set getter setter or accesor,
;;        it wouldnt be hard and would fit with class behaviour

(defun slot-name (slot) (first slot))
(defun slot-type (slot) (second slot))
(defun slot-normalisedp (slot) (third slot))

(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     ,@(loop for slot in slots :collect 
            (destructuring-bind (slot-name slot-type normalised accessor
                                           indirect hidden) slot
              (declare (ignore normalised accessor hidden))
              (let* ((p-type (varjo:type-principle slot-type))
                     (c-type (if (varjo:type-aggregate-p p-type)
                                 (varjo:type-principle slot-type)
                                 p-type)))
                (list slot-name
                      (if indirect `(:pointer ,c-type) c-type)
                      :count (if (varjo:type-arrayp slot-type)
                                 (varjo:type-array-length slot-type)
                                 1)))))))

(defun make-translators (name type-name value-name slots struct-name)
  (let ((slot-names (mapcar #'first slots)))
    `((defmethod translate-from-foreign (ptr (type ,type-name))
        (make-instance ',value-name :element-type ',name :pointer ptr))
      (defmethod translate-into-foreign-memory
          ((value t) (type ,type-name) pointer)
        (print "NO EFFECT: This style of setting c structs not yet implemented")
        nil)
      (defmethod translate-into-foreign-memory
          ((value list) (type ,type-name) pointer)
        (destructuring-bind ,slot-names value
          ,@(loop :for slot-definition :in slots :collecting
               (destructuring-bind (slot-name vslot-type normalised accessor
                                              indirect hidden) slot-definition
                 (declare (ignore normalised accessor indirect hidden))
                 (if (varjo:type-arrayp vslot-type)
                     ;;if array
                     `(let ((array-ptr (foreign-slot-pointer 
                                        pointer '(:struct ,struct-name)
                                        ',slot-name)))
                        (loop for datum in ,slot-name for i from 0 do
                             (setf (mem-aref array-ptr 
                                            ',(varjo:type-principle vslot-type)
                                            i)
                                   datum)))
                     ;;if value
                     `(setf (mem-ref (foreign-slot-pointer
                                      pointer '(:struct ,struct-name)
                                      ',slot-name) 
                                     ',(varjo:type-principle vslot-type))
                            ,slot-name)))))))))

;; [TODO] Use 'normalize' 
;; [TODO] the setter seems ugly, gotta be a better way
;; [TODO] got to handle aggregate and complex types
;; [TODO] can glsl and thus varjo have multidimensional arrays?
;; [TODO] If slot struct type return a c-value
;;        (make-instance ',value-name :element-type ',name :pointer ptr)
(defun make-getters-and-setters (name value-name struct-name slots)
  (loop for slot-definition in slots appending
       (destructuring-bind (slot-name vslot-type normalised accessor indirect
                                      hidden)
           slot-definition
         (declare (ignore normalised))
         (unless hidden
           `((defmethod ,(or accessor (utils:symb name '- slot-name)) 
                 ((gl-object ,value-name))
               ,(if (varjo:type-arrayp vslot-type)
                    `(make-c-array-from-pointer
                      ,(let ((len (varjo:type-array-length vslot-type)))
                            (if (symbolp len)
                                `(list (foreign-slot-value (pointer gl-object)
                                                           '(:struct ,struct-name)
                                                           ',len))
                                (if (listp len) `(quote ,len) 
                                    `(quote ,(list len)))))
                      ',(varjo:type-principle vslot-type)
                      (foreign-slot-pointer (pointer gl-object)
                                            '(:struct ,struct-name)
                                            ',slot-name)
                      1 ,(if indirect t nil))
                    (if indirect
                        `(mem-ref (foreign-slot-value (pointer gl-object) 
                                                      '(:struct ,struct-name)
                                                      ',slot-name)
                                  ',(varjo:type-principle vslot-type) 0)
                        `(foreign-slot-value (pointer gl-object) 
                                             '(:struct ,struct-name)
                                             ',slot-name))))
             ,(if (varjo:type-arrayp vslot-type)
                  (when (not indirect)
                    `(defmethod (setf ,(or accessor (utils:symb name '- slot-name)))
                         ((value list) (gl-object ,value-name))
                       (let ((array-ptr (foreign-slot-pointer
                                         (pointer gl-object) '(:struct ,struct-name)
                                         ',slot-name)))
                         (loop for datum in value for i from 0 do
                              (setf (mem-aref array-ptr 
                                              ',(varjo:type-principle vslot-type) i)
                                    datum))
                         value)))
                  `(defmethod (setf ,(or accessor (utils:symb name '- slot-name)))
                       ((value t) (gl-object ,value-name))             
                     (setf (mem-ref (foreign-slot-pointer (pointer gl-object) 
                                                          '(:struct ,struct-name)
                                                          ',slot-name) 
                                    ',(varjo:type-principle vslot-type))
                           value))))))))

(defun expand-slot-to-layout (slot)
  (destructuring-bind (type normalise &rest ign)
      slot
    (declare (ignore ign))
    (let ((type (varjo:flesh-out-type type)))
      (cond 
        ((varjo:type-arrayp type) 
         (loop for i below (varjo:type-array-length type)
            :append (expand-slot-to-layout 
                     (list (varjo:flesh-out-type 
                            (varjo:type-principle type))
                           normalise))))
        ((varjo:mat-typep type) 
         (loop for i below (varjo:mat/vec-length type)
            :append (expand-slot-to-layout 
                     (list (varjo:type-mat-col-to-vec type)
                           normalise))))
        ((varjo:vec-typep type) 
         `((,(varjo:mat/vec-length type) 
             ,(varjo:type-vec-core-type type) 
             ,normalise)))
        (t `((1 ,(varjo:type-principle type) ,normalise)))))))
;;(loop for type in slot-types always (if (varjo:type-arrayp type) (varjo:type-array-length type))

;; [TODO] How does hidden affect this?
(defun make-gl-struct-attrib-assigner (type-name slots)
  (let ((slot-types (mapcar #'slot-type slots)))
    ;; If all the types of slots are known, are not struct types and 
    ;; have a fixed length (when they are arrays) then this can be 
    ;; used for vertex buffer data 
    (when (and (every #'varjo:type-built-inp slot-types)
               (loop for type in slot-types always 
                    (if (varjo:type-arrayp type) 
                        (numberp (varjo:type-array-length type)) t)))
      (let* ((stride (if (> (length slots) 1)
                         `(cffi:foreign-type-size ',type-name)
                         0))
             (stride-sym (gensym "stride"))
             (definitions 
              (loop :for attr :in 
                 (mapcan #'(lambda (x) (expand-slot-to-layout (subseq x 1))) 
                         slots)
                 :for i :from 0
                 :with offset = 0
                 :append `((gl:enable-vertex-attrib-array (+ attrib-offset ,i))
                           (%gl:vertex-attrib-pointer 
                            (+ attrib-offset ,i) ,@attr ,stride-sym 
                            (cffi:make-pointer (+ ,offset pointer-offset))))
                 :do (setf offset (+ offset 
                                     (* (first attr) 
                                        (cffi:foreign-type-size 
                                         (second attr))))))))
        (when definitions
          `(defmethod gl-assign-attrib-pointers ((array-type (EQL ',type-name)) 
                                                 &optional (attrib-offset 0)
                                                   (pointer-offset 0)
                                                   stride-override normalised)
             (declare (ignore array-type normalised))
             (let ((,stride-sym (or stride-override ,stride)))
               ,@definitions
               ,(length definitions))))))))

(defun make-struct-pixel-format (name slots)
  (let* ((type (slot-type (first slots)))
         (len (length slots))
         (components (utils:kwd (subseq "RGBA" 0 len))))
    (when (and (< len 5) (loop for i in slots always (eql (slot-type i) type))
               (valid-pixel-format-p components type t nil))
      `(defmethod pixel-format-of ((type (eql ',name)))
         (pixel-format ,components ',type)))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; [TODO] if type is '(struct x) then is seen as array
(defmacro defglstruct (name &body slot-descriptions)
  (destructuring-bind (name &key (glsl t) (vertex t) (pixel t) (accessors t))
      (if (listp name) name (list name))
    (when (keywordp name) (error "Keyword name are now allowed for glstructs"))
    (unless (> (length slot-descriptions) 0) 
      (error "Cannot have a glstruct with no slots"))
    (let ((slots (loop for slot in slot-descriptions collect
                      (destructuring-bind 
                            (slot-name slot-type &key (normalised nil) 
                                       (accessor nil) (indirect nil)
                                       (hidden nil) &allow-other-keys)
                          slot
                        (list slot-name (varjo:flesh-out-type slot-type) 
                              normalised accessor indirect hidden))))
          (struct-name (utils:symb name '-struct))
          (type-name (utils:symb name '-type))
          (value-name (utils:symb name '-value)))
      (unless (loop for slot in slots never (eq (first (second slot)) 'quote))
        (error "Please do not quote the types of any of the slots"))
      (let ((contains-indirect (some (lambda (x) (fifth x)) slots)))
        (when (and contains-indirect (or glsl pixel vertex))
          (error "cannot contain indirect arrays and be compatible with gpu data"))
        `(progn
           ,(when glsl ;; need (and glsl (valid-for-glsl))
                  `(varjo:vdefstruct ,name
                     ,@(loop for slot in slots
                          collect (list (first slot) (second slot) (third slot) 
                                        nil nil (fourth slot)))))
           ,(make-cstruct-def struct-name slots) ;; needs to know to write (:pointer (:pointer thing))
           (define-foreign-type ,type-name () 
             ()
             (:actual-type :struct ,struct-name)
             (:simple-parser ,name))
           (defclass ,value-name (c-value) ())
           ,@(unless contains-indirect ;; (or :pointer)
                     (make-translators name type-name value-name slots struct-name))
           ,@(when accessors (make-getters-and-setters name value-name struct-name slots))
           ,(when vertex (make-gl-struct-attrib-assigner name slots)) ;; and valid-for-glsl
           ,(when pixel (make-struct-pixel-format name slot-descriptions)) ;; and valid-for-glsl
           ',name)))))

;;array of things
;;  (defglstruct moo (a (:vec3 3)))
;;array of pointers to things
;;  (defglstruct moo (a ((:pointer :vec3) 3)))
;;pointer->array of things
;;  (defglstruct moo (a (:vec3 3) :indirect t))
;;pointer->array of pointers to things
;;  (defglstruct moo (a ((:pointer :vec3) 3) :indirect t))
