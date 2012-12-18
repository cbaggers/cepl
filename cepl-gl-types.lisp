(in-package :cepl-gl)

(defparameter *glsl-sizes* '((:bool . 1) (:float . 1)
			     (:int . 1) (:uint . 1)))

;;------------------------------------------------------------
;; Homeless functions

(defgeneric dpopulate (array-type gl-array data)
  (:documentation 
   "This is the function that actually does the work in the 
    destructuring populate"))

(defgeneric gl-type-format (array-type &optional address-offset)
  (:documentation "given an array type and an offset in bytes
this command returns a list with the sublists being the 
layout of the attributes of the type.
Thats a pretty ugly description so here is what it could be used
for!:
When creating VAOs you often need to run vertex-attrib-pointer
command to tell opengl where in the buffer the data is and how
it is laid out. This command generates this info for you.
The sublists returned contain the following:
<num-of-components component-type normalized-flag stride pointer>
 The pointer is the reason that this has to be generated, as you
may need to specify different offsets for the first components.
So thats the logic behind it, run this command and cons the 
vertex-attribute index to the start of each sublist and you
have all the arguments for the vertex-attrib-pointer commands you
need." ))

(defmethod gl-type-format ((array-type t) &optional (address-offset 0))
  (list (list nil 1 (utils:make-keyword array-type) nil 0 
	      (cffi:make-pointer address-offset))))

(defun foreign-type-index (type index)
  (* (cffi:foreign-type-size type)
     index))

;;------------------------------------------------------------
;; Foreign Sequence Types

;; Thsi commented out line was for testing the macroexpansions
;;(def-gl-seq-types ((vec2 :float 2 'single-float 1)))
(defmacro def-gl-seq-types (type-specs)
  `(progn
     ,@(loop for spec in type-specs
          append 
            (destructuring-bind (name type length lisp-type 
				 glsl-size)
                spec
              (list
               `(defcstruct ,(utils:symb name '-internal)
                  (elements ,type :count ,length))
               
               `(define-foreign-type ,name ()
                  ()
                  (:actual-type ,(utils:symb name '-internal))
                  (:simple-parser ,name))
               
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
	       `(defmethod gl-type-format 
		    ((array-type (EQL ',name))
		     &optional (address-offset 0))
		  (list
		   (list nil ,length ,(utils:make-keyword type)
			 nil 0 (cffi:make-pointer address-offset))))
	       `(setf *glsl-sizes* 
		      (acons ',name ,glsl-size *glsl-sizes*)))))))


;; And here is the code that does the work
;; [TODO] bools: is signed byte correct?
;; [TODO] Are any of the byte types correct?
(defctype bool :unsigned-char)
(defctype int :int)
(defctype uint :uint)
(defctype float :float)
(defctype short :short)
(defctype unsigned-short :unsigned-short)
(defctype ushort :unsigned-short)
;; these types can mostly be changed to valid enums for opengl
;; by just making them into a keyword, but not these ones
(defcenum (%gl:enum :unsigned-int)
  (:uint #x1405)
  (:ushort #x1403))

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

(defun make-gl-struct-slot-getters (type-name slot-descriptions) 
  (loop for descrip in slot-descriptions
	collect 
	(destructuring-bind (slot-name slot-type 
			     &key (count 1))
	    descrip
	  `(defun ,(utils:symb type-name '- slot-name) (pointer)
	     ,(if (> count 1)
		  `(foreign-slot-pointer pointer 
					 ',type-name
					 ',slot-name)
		  `(,(utils::symbolicate-package 'cgl 
						 slot-type 
						 '-getter)
		    (foreign-slot-pointer pointer
					',type-name
					',slot-name)))))))

(defun make-gl-struct-getter (type-name slot-descriptions)
  (declare (ignore slot-descriptions))
  `(defun ,(utils:symb type-name '-getter) (pointer)
     pointer))

(defun make-gl-struct-slot-setters (type-name slot-descriptions) 
  (loop for descrip in slot-descriptions
     collect 
     (destructuring-bind (slot-name slot-type &key (count 1))
	 descrip
       `(defun (setf ,(utils:symb type-name '- slot-name)) 
	    (value pointer)
	  ,(if (> count 1)
	       `(error "GLSTRUCT SETTER ERROR: Sorry, you cannot directly set a foreign array slot: ~s ~s" value pointer)
	       `(,(utils::symbolicate-package 
		   (package-name (symbol-package slot-type))
		   slot-type '-setter)
		 (foreign-slot-pointer pointer
				       ',type-name
				       ',slot-name)
		 value))))))

(defun make-gl-struct-setter (type-name slot-descriptions)
  `(defun ,(utils:symb type-name '-setter) (pointer lisp-values)
     (if (eq ,(length slot-descriptions) (length lisp-values))
	 (destructuring-bind ,(mapcar #'first slot-descriptions)
	     lisp-values
	   ,@(loop for descrip in slot-descriptions
		   collect 
		   (destructuring-bind (slot-name slot-type 
					&key (count 1))
		       descrip
		     (declare (ignore slot-type count))
		     `(setf (,(utils:symb type-name '- slot-name) 
			     pointer) ,slot-name))))
	 (error ,(format nil "To set a glstruct directly you must pass a list of exactly ~s values. One for each of the slots." (length slot-descriptions))))))

(defun make-gl-struct-dpop (type-name slot-descriptions)
  (let ((slot-names (mapcar #'first slot-descriptions))
	(loop-token (gensym "LOOP")))
    `(defmethod dpopulate ((array-type (eql ',type-name))
			   gl-array
			   data)
       (loop for ,slot-names in data
	     for ,loop-token from 0
	     do ,@(loop for slot-name in slot-names
			collect
			`(setf (,(utils:symb type-name '- slot-name)
				(aref-gl gl-array ,loop-token))
			       ,slot-name))))))

(defun make-gl-struct-glpull (type-name slot-descriptions)
  (let ((slot-names (mapcar #'first slot-descriptions)))
    `(defmethod glpull-entry ((array-type (eql ',type-name))
			      gl-array
			      index)
       (list ,@(loop for slot-name in slot-names
		     collect
		     `(,(utils:symb type-name '- slot-name)
		       (aref-gl gl-array index)))))))
	 
(defun make-gl-struct-format (type-name slot-descriptions)
  (let ((stride (if (> (length slot-descriptions) 1)
		  `(cffi:foreign-type-size ',type-name)
		  0)))
    `(defmethod gl-type-format ((array-type (EQL ',type-name)) 
				&optional (address-offset 0))
       ,@(if 
	  (mapcan #'first 
		  (mapcan #'cgl::gl-type-format 
			  (mapcar #'second slot-descriptions)))
	  `((declare (ignore array-type address-offset))
	    (list))
	  `((list 
	     ,@(loop for descrip in slot-descriptions
		     collect 
		     (destructuring-bind (slot-name  
					 slot-type
					  &key (count 1) 
					    (normalised nil) 
					  &allow-other-keys)
			 descrip
		       (if (keywordp slot-type)
			  `(list ',slot-name ,count ',slot-type 
				 ,normalised ,stride 
				 (cffi:make-pointer 
				  (+ (foreign-slot-offset ',type-name
							  ',slot-name)
				     address-offset)))
			  (let ((sub-type (first (gl-type-format slot-type))))
			    `(list ',slot-name ,(second sub-type)
				   ',(third sub-type) ,normalised
				   ,stride 
				   (cffi:make-pointer 
				    (+ (foreign-slot-offset ',type-name
							    ',slot-name)
				       address-offset)))))))))))))

(defun calc-glsl-size (type-name slot-descriptions)
  (labels ((get-slot-size (slot)
	     (destructuring-bind 
		 (slot-name slot-type 
		  &key (count 1) (normalised nil) 
		  &allow-other-keys)
		 slot
	       (declare (ignore slot-name normalised))
	       (* count (cdr (assoc slot-type *glsl-sizes*))))))
    (let ((glsl-size (apply #'+ (mapcar #'get-slot-size 
					 slot-descriptions))))
      `(setf *glsl-sizes*
	     (acons ',type-name ,glsl-size cgl::*glsl-sizes*)))))

;; (defun make-gl-struct-layout (type-name slot-descriptions)
;;   `(defun)
;;   (loop for slot in slot-descriptions))

(defmacro defglstruct (name &body slot-descriptions)
  "Slots have: (name type &key (count 1) (normalised nil))"
  `(progn
     (defcstruct ,name
       ,@slot-descriptions)     
     ,@(make-gl-struct-slot-getters name slot-descriptions)
     ,@(make-gl-struct-slot-setters name slot-descriptions)
     ,(make-gl-struct-getter name slot-descriptions)
     ,(make-gl-struct-setter name slot-descriptions)
     ,(make-gl-struct-dpop name slot-descriptions)
     ,(make-gl-struct-glpull name slot-descriptions)
     ,(make-gl-struct-format name slot-descriptions)
     ,(calc-glsl-size name slot-descriptions)
     ',name))

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
