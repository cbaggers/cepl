(in-package :cgl)

;; this file has been bodged as I am rewriting this soon to be able 
;; to handle more complex types. This is a terrible fucking mess and I'm
;; sorry
;; More than that, this is wrong

;;------------------------------------------------------------

(defun slot-name (slot) (first slot))
(defun slot-type (slot) (second slot))
(defun slot-normalisedp (slot) (third slot))

(defun type->spec (type)
  (let ((spec (type->type-spec type)))
    (if (listp spec)
        `(,(if (core-typep (type-spec->type (first spec)))
               (symbolicate-package "KEYWORD" (subseq (symbol-name (first spec)) 2))
               (first spec))
           ,(if (and (listp (second spec)) (= (length (second spec)) 1))
                (first (second spec)) (second spec)))
        (if (core-typep type)
            (symbolicate-package "KEYWORD" (subseq (symbol-name spec) 2))
            spec))))

(defun type-principle (type)
  (if (v-typep type 'v-array)
      (v-element-type type)
      type))

(defun type-aggregate-p (type)
  (or (v-typep type 'v-vector)
      (v-typep type 'v-matrix)
      (when (v-typep type 'v-array)
        (type-aggregate-p (v-element-type type)))))

(defun get-raw-type (type)
  (if (v-typep type 'v-array)
      (get-raw-type (v-element-type type))
      type))

(defun get-raw-length (type)
  (if (v-typep type 'v-array) 
      (* (apply #'* (v-dimensions type))
         (get-raw-length (v-element-type type)))
      1))

(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     ,@(loop for slot in slots :collect
            (let* ((slot-type (slot-type slot)))
              (list (slot-name slot) 
                    (type->spec (get-raw-type slot-type))
                    :count (get-raw-length slot-type))))))


;; [TODO] is this fucked now?
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
               (destructuring-bind (slot-name vslot-type normalised accessor)
                   slot-definition
                 (declare (ignore normalised accessor))
                 (if (v-typep vslot-type 'v-array)
                     ;;if array
                     `(let ((array-ptr (foreign-slot-pointer 
                                        pointer '(:struct ,struct-name)
                                        ',slot-name)))
                        (loop for datum in ,slot-name for i from 0 do
                             (setf (mem-aref array-ptr 
                                             ',(type-principle vslot-type)
                                             i)
                                   datum)))
                     ;;if value
                     `(setf (mem-ref (foreign-slot-pointer
                                      pointer '(:struct ,struct-name)
                                      ',slot-name) 
                                     ',(type->spec vslot-type))
                            ,slot-name)))))))))


;; [TODO] Use 'normalize' 
;; [TODO] the setter seems ugly, gotta be a better way
;; [TODO] got to handle aggregate and complex types
;; [TODO] can glsl and thus varjo have multidimensional arrays?
;; [TODO] If slot struct type return a c-value
;;        (make-instance ',value-name :element-type ',name :pointer ptr)
(defun make-puller (name value-name slots)
  `(defmethod gpull ((gl-object ,value-name))
     (list ,@(loop :for slot-definition :in slots :collect
                (destructuring-bind (slot-name vslot-type normalised accessor)
                    slot-definition
                  (declare (ignore normalised vslot-type))
                  (list (or accessor (utils:symb name '- slot-name)) 'gl-object))))))


;; [TODO] Fuck this is ugly gotta rework this
;; [TODO] alignment, so we need to care here?
(defun make-getters-and-setters (name value-name struct-name slots)
  (loop :for (slot-name vslot-type normalised accessor) :in slots appending
     `((defmethod ,(or accessor (utils:symb name '- slot-name))
           ((gl-object ,value-name))
         ,(if (v-typep vslot-type 'v-array)
              `(make-c-array-from-pointer 
                ',(let ((len (v-dimensions vslot-type)))
                       (if (listp len) len (list len)))
                ,(type-principle vslot-type)
                (foreign-slot-pointer (pointer gl-object)
                                      '(:struct ,struct-name)
                                      ',slot-name))
              `(foreign-slot-value (pointer gl-object) 
                                   '(:struct ,struct-name)
                                   ',slot-name)))
       ,(if (v-typep vslot-type 'v-array)
            `(defmethod (setf ,(or accessor (utils:symb name '- slot-name)))
                 ((value list) (gl-object ,value-name))
               (let ((array-ptr (foreign-slot-pointer
                                 (pointer gl-object) '(:struct ,struct-name)
                                 ',slot-name)))
                 (loop for datum in value for i from 0 do
                      (setf (mem-aref array-ptr 
                                      ',(type-principle vslot-type) i)
                            datum))
                 value))
            `(defmethod (setf ,(or accessor (utils:symb name '- slot-name)))
                 ((value t) (gl-object ,value-name))             
               (setf (mem-ref (foreign-slot-pointer (pointer gl-object) 
                                                    '(:struct ,struct-name)
                                                    ',slot-name) 
                              ',(type->spec (type-principle vslot-type)))
                     value))))))

(defun expand-slot-to-layout (slot)
  (destructuring-bind (type normalise &rest ign) slot
    (declare (ignore ign))
    (cond 
      ((v-typep type 'v-array) 
       (loop for i below (apply #'* (v-dimensions type))
          :append (expand-slot-to-layout 
                   (list (type->spec (type-principle type)) normalise))))
      ((v-typep type 'v-matrix) 
       (loop for i below (apply #'* (v-dimensions type))
          :append (expand-slot-to-layout
                   (list (first (v-dimensions type)) normalise))))
      ((v-typep type 'v-vector)  
       `((,(apply #'* (v-dimensions type))
           ,(type->spec (v-element-type type)) 
           ,normalise)))
      (t `((1 ,(type->spec (type-principle type)) ,normalise))))))

(defun make-gl-struct-attrib-assigner (type-name slots)
  (when (every #'core-typep (mapcar #'slot-type slots))
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
             ,(length definitions)))))))

(defun make-struct-pixel-format (name slots)
  (let* ((type (slot-type (first slots)))
         (len (length slots))
         (components (utils:kwd (subseq "RGBA" 0 len))))
    (when (and (< len 5) (loop for i in slots always (eql (slot-type i) type))
               (valid-pixel-format-p components type t nil))
      `(defmethod pixel-format-of ((type (eql ',name)))
         (pixel-format ,components ',type)))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; [TODO] vdefstruct should use eval-when to get data into tables early to
;;        avoid any issues when compiling shaders. This should allow moving 
;;        chunks of the compiling into macroexpansion time which will speed
;;        things at runtime.
;;
;; [TODO] vdefstruct needs to be rewritten to make use the new types, should 
;;        be super easy as should match the formats here.

(defmacro defglstruct (name &body slot-descriptions)
  (destructuring-bind (name &key (glsl t) (vertex t) (pixel t) (accessors t))
      (if (listp name) name (list name))
    (when (keywordp name) (error "Keyword name are now allowed for glstructs"))
    (unless (> (length slot-descriptions) 0) 
      (error "Cannot have a glstruct with no slots"))
    (let* ((slots (loop :for slot :in slot-descriptions :collect
                       (destructuring-bind 
                             (slot-name slot-type &key (normalised nil) 
                                        (accessor nil) &allow-other-keys)
                           slot
                         (list slot-name (type-spec->type slot-type) 
                               normalised accessor))))
           (struct-name (utils:symb name '-struct))
           (type-name (utils:symb name '-type))
           (value-name (utils:symb name '-value)))
      `(progn
         ,(when glsl
                `(v-defstruct ,name ()
                   ,@(loop for slot in slots
                        :collect `(,(first slot) 
                                    ,(type->type-spec (second slot))
                                    ,@(when (fourth slot) `(:accessor ,(fourth slot)))))))
         ,(make-cstruct-def struct-name slots)
         (define-foreign-type ,type-name () 
           ()
           (:actual-type :struct ,struct-name)
           (:simple-parser ,name))
         (defclass ,value-name (c-value) ())
         ,@(when accessors (make-getters-and-setters name value-name struct-name slots))
         ,@(make-translators name type-name value-name slots struct-name)
         ,(when accessors (make-puller name value-name slots))
         ,(when vertex (make-gl-struct-attrib-assigner name slots))
         ,(when pixel (make-struct-pixel-format name slot-descriptions))
         ',name))))
