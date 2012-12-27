(in-package :cepl-gl)

(defparameter *glsl-sizes* '((:bool . 1) (:float . 1)
                             (:int . 1) (:uint . 1)))

;; [TODO] We need to look into bool
(defconstant *glsl-internal-types* 
  '(:boolean :bool :int :float :int :uint
    vec2 vec3 vec4 ivec2 ivec3 ivec4 uvec2 uvec3
    uvec4 bvec2 bvec3 bvec4 mat2 mat3 mat4 mat2x2
    mat2x3 mat2x4 mat3x2 mat3x3 mat3x4 mat4x2
    mat4x3 mat4x4))

(defconstant *glsl-aggregate-types* 
  '(vec2 vec3 vec4 ivec2 ivec3 ivec4 uvec2 uvec3
    uvec4 bvec2 bvec3 bvec4 mat2 mat3 mat4 mat2x2
    mat2x3 mat2x4 mat3x2 mat3x3 mat3x4 mat4x2
    mat4x3 mat4x4))

;;------------------------------------------------------------
;; Homeless functions

(defgeneric dpopulate (array-type gl-array data)
  (:documentation 
   "This is the function that actually does the work in the 
    destructuring populate"))

(defgeneric gl-assign-attrib-pointers (type attrib-num &optional stride pointer-offset count))

(defgeneric stream-headers (array-type &optional offset))

(defun glsl-aggregrate-type-p (type-symb)
  (find type-symb *glsl-aggregate-types*))

(defun glsl-type-p (type-symb)
  (when (assoc type-symb *glsl-sizes*) t))

(defun glsl-struct-p (type-symb)
  (when (and (glsl-type-p type-symb)
             (not (glsl-aggregrate-type-p type-symb))) 
    t))

(defun glsl-size (type)
  (cdr (assoc type cgl::*glsl-sizes*)))

;; [TODO] gl-type-format is defunct, find a way to make this work
;; (defun valid-gpu-stream-type (type-name)
;;   (when (gl-type-format type-name)
;;     t))

(defun foreign-type-index (type index)
  (* (cffi:foreign-type-size type)
     index))

;;------------------------------------------------------------
;; Foreign Sequence Types

;; This commented out line was for testing the macroexpansions
;;(def-gl-seq-types ((vec2 :float 2 'single-float 1)))
(defmacro def-gl-seq-types (type-specs)
  `(progn
     ,@(loop for (name type length lisp-type glsl-size)
          in type-specs
          :append 
            (list
             `(defcstruct ,name
                (elements ,type :count ,length))
             
             
             `(defun ,(utils:symb name '-setter) (pointer 
                                                  lisp-value)
                (setf
                 ,@(loop for i below length
                      append `((cffi:mem-aref pointer
                                              ',type
                                              ,i) 
                               (aref lisp-value ,i))))
                lisp-value)

             `(defun ,(utils:symb name '-getter) (pointer)
                (make-array 
                 ,length
                 :element-type ,lisp-type
                 :initial-contents 
                 ,(cons 
                   'list
                   (loop for i below length
                      collect 
                        `(cffi:mem-aref pointer ',type ,i)))))
             ;; [TODO] this is different from other method of same name
             `(defmethod gl-assign-attrib-pointers ((type (EQL ',name))
                                                    attrib-num &optional
                                                      (stride 0)
                                                      (pointer-offset 0)
                                                                 (count 1))
                (declare (ignore type))
                (%gl:vertex-attrib-pointer 
                 attrib-num (* count ,length) ,type nil stride
                 (cffi:make-pointer pointer-offset)))
             `(setf *glsl-sizes* 
                    (acons ',name ,glsl-size *glsl-sizes*))))))


;; And here is the code that does the work
;; [TODO] bools: is signed byte correct?
;; [TODO] Are any of the byte types correct?
(defctype bool :unsigned-char)
(defctype int :int)
(defctype uint :uint)
(defctype float :float)
;; these types can mostly be changed to valid enums for opengl
;; by just making them into a keyword, but not these ones
(defcenum (%gl:enum :unsigned-int)
  (:uint #x1405))

(def-gl-seq-types ((vec2 :float 2 'single-float 1)
                   (vec3 :float 3 'single-float 1)
                   (vec4 :float 4 'single-float 1)
                   (ivec2 :int 2 'signed-byte 1)
                   (ivec3 :int 3 'signed-byte 1)
                   (ivec4 :int 4 'signed-byte 1)
                   (uvec2 :uint 2 'unsigned-byte 1)
                   (uvec3 :uint 3 'unsigned-byte 1)
                   (uvec4 :uint 4 'unsigned-byte 1)
                   (bvec2 :unsigned-char 2 'signed-byte 1) 
                   (bvec3 :unsigned-char 3 'signed-byte 1)
                   (bvec4 :unsigned-char 4 'signed-byte 1)
                   (mat2 :float 4 'single-float 2)
                   (mat3 :float 9 'single-float 3)
                   (mat4 :float 16 'single-float 4)
                   (mat2x2 :float 4 'single-float 2)
                   (mat2x3 :float 6 'single-float 2)
                   (mat2x4 :float 8 'single-float 2)
                   (mat3x2 :float 6 'single-float 3)
                   (mat3x3 :float 9 'single-float 3)
                   (mat3x4 :float 12 'single-float 3)
                   (mat4x2 :float 8 'single-float 4)
                   (mat4x3 :float 12 'single-float 4)
                   (mat4x4 :float 16 'single-float 4)))

(defun make-gl-struct-slot-getters (type-name slots) 
  (loop for (slot-name slot-type count) in slots
     collect `(defun ,(utils:symb type-name '- slot-name) (pointer)
                ,(if (> count 1)
                     (if (keywordp slot-type) 
                         `(foreign-slot-pointer pointer
                                                ',type-name
                                                ',slot-name)
                         `(foreign-slot-pointer pointer 
                                                ',type-name
                                                ',slot-name))
                     (if (keywordp slot-type)
                         `(foreign-slot-value pointer
                                              ',type-name
                                              ',slot-name)
                         `(,(utils::symbolicate-package 'cgl 
                                                        slot-type 
                                                        '-getter)
                            (foreign-slot-pointer pointer
                                                  ',type-name
                                                  ',slot-name)))))))


(defun make-gl-struct-slot-setters (type-name slots) 
  (loop for (slot-name slot-type count) in slots
     collect 
       `(defun (setf ,(utils:symb type-name '- slot-name)) 
            (value pointer)
          ,(if (> count 1)
               `(error "GLSTRUCT SETTER ERROR: Sorry, you cannot directly set a foreign array slot: ~s ~s" value pointer)
               (if (keywordp slot-type)
                   `(setf (foreign-slot-value pointer
                                              ',type-name
                                              ',slot-name)
                          value)
                   `(,(utils::symbolicate-package 
                       (package-name (symbol-package slot-type))
                       slot-type '-setter)
                      (foreign-slot-pointer pointer
                                            ',type-name
                                            ',slot-name)
                      value))))))


(defun make-gl-struct-dpop (type-name slots)
  (let ((loop-token (gensym "LOOP"))
        (slot-names (mapcar #'first slots)))
    `(defmethod dpopulate ((array-type (eql ',type-name))
                           gl-array
                           data)
       (loop for ,slot-names in data
          for ,loop-token from 0
          do ,@(loop for (slot-name) in slots
                  collect
                    `(setf (,(utils:symb type-name '- slot-name)
                             (aref-gl gl-array ,loop-token))
                           ,slot-name))))))

(defun make-gl-struct-glpull (type-name slots)
  `(defmethod glpull-entry ((array-type (eql ',type-name))
                            gl-array
                            index)
     (list ,@(loop for (slot-name) in slots
                collect
                  `(,(utils:symb type-name '- slot-name)
                     (aref-gl gl-array index))))))

(defun make-gl-struct-attrib-assigner (type-name slots)
  (when (notany #'glsl-struct-p (mapcar #'second slots))
    (let* ((stride (if (> (length slots) 1)
                       `(cffi:foreign-type-size ',type-name)
                       0))
           (definitions (loop for (slot-name slot-type count normalised) in slots
                             for i from 0
                           :if (glsl-aggregrate-type-p slot-type) :collect 
                             `(gl-assign-attrib-pointers ',slot-type (+ attrib-num ,i)
                                                         ,stride 
                                                         (+ (foreign-slot-offset ',type-name
                                                                          ',slot-name)
                                                            pointer-offset) 
                                                         ,count)
                           :else :collect
                             `(%gl:vertex-attrib-pointer 
                               (+ attrib-num ,i) ,count ,slot-type ,normalised ,stride
                               (cffi:make-pointer (+ (foreign-slot-offset ',type-name
                                                                          ',slot-name)
                                                     pointer-offset))))))
      (when definitions
        `(defmethod gl-assign-attrib-pointers ((array-type (EQL ',type-name))
                                               attrib-num
                                               &optional stride pointer-offset count)
           (declare (ignore array-type stride count))
           ,@definitions)))))

(defun make-gl-struct-stream-layout (type-name slots)
  ;; Note that the second 'format' runs at macro expansion time
  ;; to make the template for the method's runtime call of 
  ;; format (hence the ~~s)
  `(defmethod stream-headers ((array-type (eql ',type-name)) &optional (offset 0))
     (declare (ignore array-type))
     ,(if (notany #'(lambda (x) (glsl-struct-p (second x))) slots)
          (destructuring-bind (layouts sizes)
              (loop for (slot-name slot-type count normalised) in slots
                 :with total = 0
                 :collecting
                   (format nil "layout(location = ~~s) in ~A ~s~@[[~A]~];~%"
                           slot-type 
                           (utils:symb type-name '- slot-name)
                           (when (> count 1) count)) :into layouts
                 :collecting total :into totals 
                 :do (setf total (+ total (* (cgl::glsl-size slot-type) count)))
                 :finally (return (list layouts totals)))
            `(format nil ,(format nil "~{~A~}" layouts)
                     ,@(loop for i in sizes collect `(+ offset ,i))))
          `(error ,(format nil "Structs of type ~s cannot be used for streams" type-name)))))


(defun calc-glsl-size (type-name slots)
  (let ((glsl-size (loop for (slot-name slot-type count normalised) in slots
                      sum (* count (cdr (assoc slot-type *glsl-sizes*))))))
    `(setf *glsl-sizes*
            (acons ',type-name ,glsl-size cgl::*glsl-sizes*))))


(defmacro defglstruct (name &body slot-descriptions)
  ;; tidy up the slot definintions
  (let ((slots (loop for slot in slot-descriptions
                  collect (destructuring-bind 
                                (slot-name 
                                 slot-type 
                                 &key (count 1) (normalised nil) 
                                 &allow-other-keys)
                              slot
                            (list slot-name slot-type count normalised)))))
    ;; write the code
    `(progn
       (defcstruct ,name ,@slot-descriptions)
       ,@(make-gl-struct-slot-getters name slots)
       ,@(make-gl-struct-slot-setters name slots)
       ,(make-gl-struct-dpop name slots)
       ,(make-gl-struct-glpull name slots)
       ,(make-gl-struct-attrib-assigner name slots)
       ,(make-gl-struct-stream-layout name slots)
       ,(calc-glsl-size name slots)
       ',name)))

;; glvertexattribpointer can take these enums
;; --------------------------------
;; GL_BYTE
;; GL_UNSIGNED_BYTE
;; GL_SHORT
;; GL_UNSIGNED_SHORT
;; GL_INT and
;; GL_UNSIGNED_INT
;; GL_HALF_FLOAT
;; GL_FLOAT
;; GL_DOUBLE
;; GL_FIXED
;; GL_INT_2_10_10_10_REV
;; GL_UNSIGNED_INT_2_10_10_10_REV
;; --------------------------------
;; GL_DOUBLE
