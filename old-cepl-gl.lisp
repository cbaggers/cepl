;; This is to shadow and tidy up the cl-opengl feature set.
;;
;; It is designed to be the 'thinnest' possible wrapper around
;; (cl)opengl 2.1+ which abstracts the ugly stuff without taking
;; away any of the functionality.
;;
;; The key design axioms are:
;; * OpenGl is 2 paradigms in one API with totally different 
;;   methodologies and approaches to development. It is better
;;   to make 2 wrappers that seperate and abstract these 
;;   methodologies well, rather than to try and cram both 
;;   together. 
;;   To this end cepl-gl will only support modern opengl (2.1+).
;;
;; * It should be possible to write a 'hello world' opengl 
;;   example from a tutorial online without abstractions getting 
;;   in the way.
;;   Abstractions, in their effort to simplify complex code, can
;;   balloon simple code. This is unacceptable. 
;;   Part of the beauty of lisp is being able to blast out a
;;   quick example in the repl and we must not allow our attempts
;;   to simplify things to take any of that beauty away.
;;

(in-package :cepl-gl)

(define-condition cgl-compile-shader-error (error)
  ((text :initarg :text :reader text)))


;;;--------------------------------------------------------------
;;; SHADOW'D OPENGL FUNCTIONS ;;;
;;;---------------------------;;;

;; These two should be fine as it only applies to vertex arrays
;; 
(defun enable-vertex-attrib-array (buffer index)
  (bind-buffer :array-buffer buffer)
  (gl:enable-vertex-attrib-array index))

(defun vertex-attrib-pointer (buffer index size type 
                              normalised stride pointer)
  (bind-buffer :array-buffer buffer)
  (gl:vertex-attrib-pointer index size type normalised stride
                            pointer))

(defun uniform-matrix (program-id location dim matrices)
  (use-program program-id)
  (gl:uniform-matrix location dim matrices nil))

;;;--------------------------------------------------------------
;;; EXTRA OPENGL FUNCTIONS ;;;
;;;------------------------;;;

(defun draw-elements-base-vertex (mode array indices base-vertex
                                  &key (count (gl::gl-array-size array)))
  (%gl:draw-elements-base-vertex mode count
                                 (gl::cffi-type-to-gl 
                                  (gl::gl-array-type array))
                                 (gl::gl-array-pointer-offset array indices)
                                 base-vertex))

;;;--------------------------------------------------------------
;;; GL-ARRAYS ;;;
;;;-----------;;;

(defun simple-1d-populate (array data)
  "Reads a flat list of data into a gl-array"
  (loop for datum in data
     for i from 0
     do (setf (aref-gl array i) datum)))

(defun simple-array-attrib-error (&rest args)
  "This is just to kick off an error. There will be a better
   way of doing this, but for now it does the job"
  (declare (ignore args))
  (error "Sorry, attribute-formats cannot be generated from primitive array types"))

;;[TODO] What types do we need to support here?
(loop for type-name in `(:float :short)
   do (progn
        (setf (get type-name 'cgl-destructuring-populate)
              #'simple-1d-populate)
        (setf (get type-name 'vertex-attrib-formats) nil)))

(defmacro define-attribute-format (name &rest args)
  "This allows the user to create a non interleaved 
   attribute format. 

   In actuality it is just a bit of syntatic sugar and itself
   uses the define-interleaved-attribute-format macro.

   Attribute formats are the key to getting CEPL to do a lot 
   of the ugly low level work of opengl for you. They are used
   in defining the streams of vertex data you pass to the shader.
   As we have these formats available we can also use them to 
   tell cepl how to push data into opengl buffers.
   Finally the attribute format also works as a 'type' for the
   gl-arrays."
  `(define-interleaved-attribute-format ,name ,args))

;; [TODO] Can we make the name a key symbol? it would be much
;;        nicer in code than remebering to escape it.
(defmacro define-interleaved-attribute-format (name &body clauses)
  "Defines a vertex attribute format. Each clause is as list 
   of parameters which are used to define the type, legnth, etc
   of the attributes themselves.

   Parameters are keyword arguments for the corresponding array
   type. The following parameters are supported:

    :TYPE -- array element type (all array types)
    :COMPONENTS -- list of component (slot) names for this array
                   (all types)
    :STAGE -- active texture for the array (TEX-COORD type)
    :NORMALIZED -- whether values should be normalized

   Attribute formats are the key to getting CEPL to do a lot 
   of the ugly low level work of opengl for you. They are used
   in defining the streams of vertex data you pass to the shader.
   As we have these formats available we can also use them to 
   tell cepl how to push data into opengl buffers.
   Finally the attribute format also works as a 'type' for the
   gl-arrays."
  `(progn
     (defcstruct ,name
         ,@(mapcan #'emit-attrib-struct-clause clauses))
     (setf (get ',name 'cgl-destructuring-populate)
           (dpopulate ,@clauses))
     (setf (get ',name 'vertex-attrib-formats)
           (list ,@(loop with stride = (if (> (length clauses) 1)
                                           `(foreign-type-size ',name)
                                           0)
                      for c in clauses
                      for offset = `(foreign-slot-offset
                                     ',name 
                                     ',(caadr 
                                        (member :components c)))
                      collect `(lambda (x) 
                                 (emit-attrib-format
                                  ',c 
                                  (cffi:make-pointer (+ ,offset x)) 
                                  ,stride )))))
     ',name))

(defun emit-attrib-format (clause offset stride)
  "this is a helper function for define-interleaved-attribute-..
   format that really only exists to remove a little of the 
   complexity from the core loop of ...
   define-interleaved-attribute-format"
  (destructuring-bind (&key (normalized :false) type components
                            &allow-other-keys)
      clause
    `(,(length components) ,type ,normalized ,stride ,offset)))

(defun emit-attrib-struct-clause (clause)
  "This is a helper function is used to flatten the clauses 
   passed to define-interleaved-attribute-format into the format
   expected by defcstruct."
  (destructuring-bind (&key type components &allow-other-keys)
      clause
    (loop for c in components
       collect `(,c ,type))))

(defun attrib-formats (array-type)
  "Returns a list of the attribute formats for the specified 
   gl-array type"
  (get array-type 'vertex-attrib-formats))

(defun attrib-format (array-type &optional  (sub-attrib 0)
                                   (offset 0))
  "Returns the specified attribute format of the provided array
   type. For interleaved types you can specify the attribute
  you ar interested in and also provide an offset to be used."
  (let ((formats (get array-type 'vertex-attrib-formats)))
    (if (null formats)
        (error "Sorry but attribute formats are not available for this array-type")
        (funcall (nth sub-attrib formats) offset))))

(defun destructuring-populate (array data)
  "This function takes a gl-array and a list of data and 
   populates the gl-array using the data. 
   The data must be a list of sublists. Each sublist must
   be in the format expected by a destrucutring-bind with
   the target format being that specified by the array-type.
  
   That sucks as an explanation so here is an example:

   given a format as defined below:
    (cgl:define-interleaved-attribute-format vert-data 
      (:type :float :components (x y z))
      (:type :float :components (r g b a)))

   and an array made using this format
    (setf *vertex-data-gl* (cgl:alloc-array-gl 'vert-data 3))

   then you can populate it as so:
    (cgl:destructuring-populate *vertex-data-gl* 
     	               '((( 0.0     0.5  0.0)
			  ( 1.0     0.0  0.0  1.0))

			 (( 0.5  -0.366  0.0)
			  ( 0.0     1.0  0.0  1.0))

			 ((-0.5  -0.366  0.0)
			  ( 0.0     0.0  1.0  1.0))))

   Hopefully that makes sense."
  (let ((func (get (gl::gl-array-type array) 
                   'cgl-destructuring-populate)))
    (funcall func array data)
    array))

(defun destructuring-allocate (array-type data)
  "This function will create a new gl-array with a length
   equal to the length of the data provided, and then populate 
   the gl-array.

   The data must be a list of sublists. Each sublist must
   be in the format expected by a destrucutring-bind with
   the target format being that specified by the array-type.
  
   That sucks as an explanation so here is an example:

   given a format as defined below:
    (cgl:define-interleaved-attribute-format vert-data 
      (:type :float :components (x y z))
      (:type :float :components (r g b a)))

   then you can create and populate it a new gl-array as so:
    (setf *new-gl-arrray*
        (cgl:destructuring-allocate 'vert-data 
     	               '((( 0.0     0.5  0.0)
			  ( 1.0     0.0  0.0  1.0))

			 (( 0.5  -0.366  0.0)
			  ( 0.0     1.0  0.0  1.0))

			 ((-0.5  -0.366  0.0)
			  ( 0.0     0.0  1.0  1.0)))))

   Hopefully that makes sense."
  (let ((array (alloc-array-gl array-type (length data))))
    (destructuring-populate array data)))

(defmacro dpopulate (&body clauses)
  "This is the macro that generates the code which is used
   by the destructuring-populate function."
  (let ((loop-token (gensym "LOOP")))
    `(lambda (array data)
       (loop for vert in data
          for ,loop-token from 0
          do (destructuring-bind ,(list-components
                                   clauses) 
                 (if (numberp vert)
                     (list (list vert))
                     vert)
               ,@(loop for comp in (utils:flatten 
                                    (list-components
                                     clauses))
                    collect (list 'setf 
                                  `(aref-gl 
                                    array
                                    ,loop-token
                                    ',comp) comp)))))))

(defun list-components (clauses)
  "This is a helper function for dpopulate which returns
   a list of component portions of the clauses"  
  (loop for clause in clauses
     collect (destructuring-bind (&rest rest &key components
                                        &allow-other-keys)
                 clause
               (declare (ignore rest))
               components)))


;; needs to be here right now as I don't know how to set
;; vertex-array-binder to be in cl-opengl
(defun alloc-gl-array (type count)
  "Creates a new gl-array of specified type and length"
  (gl::make-gl-array :pointer (foreign-alloc type :count count)
                     :size count :type type))


(defun alloc-array-gl (type count)
  "Creates a new gl-array of specified type and length"
  (alloc-gl-array type count))


;; Grr, wanted to use this but coudl work out how to do the 
;; setf version
;; (setf (symbol-function 'aref-gl) (symbol-function 'glaref))
(declaim (inline aref-gl))
(defun aref-gl (array index &optional (component nil c-p))
  "Returns the INDEX-th component of gl-array. If COMPONENT is
supplied and ARRAY is of a compound type the component named
COMPONENT is returned."
  (if c-p
      (foreign-slot-value (mem-aref (gl::gl-array-pointer array
                                                          )
                                    (gl::gl-array-type array)
                                    index)
                          (gl::gl-array-type array)
                          component)
      (mem-aref (gl::gl-array-pointer array) (gl::gl-array-type array) index)))


(declaim (inline (setf aref-gl)))
(defun (setf aref-gl) (value array index &optional (component nil c-p))
  "Sets the place (GLAREF ARRAY INDEX [COMPONENT]) to VALUE."
  (if c-p
      (setf (foreign-slot-value (mem-aref (gl::gl-array-pointer array)
                                          (gl::gl-array-type array)
                                          index)
                                (gl::gl-array-type array)
                                component)
            value)
      (setf (mem-aref (gl::gl-array-pointer array) (gl::gl-array-type array) index)
            value)))

;;;--------------------------------------------------------------
;;; BUFFERS ;;;
;;;---------;;;

;; [TODO] Make this external
;; [TODO] Type this
(defstruct glbuffer
  (buffer-id (car (gen-buffers 1))) 
  (format nil))

(base-macros:defmemo bind-buffer (target buffer-id)
  (gl:bind-buffer target (glbuffer-buffer-id buffer-id)))

(defun gen-buffer (&key initial-contents (buffer-type :array-buffer) (draw-type :static-draw))
  "Generates a single opengl buffer"
  (let ((new-buffer (make-glbuffer)))
    (if initial-contents
        (buffer-data new-buffer initial-contents 
		     :buffer-type buffer-type 
		     :draw-type draw-type)
        new-buffer)))

(defun buffer-data (buffer array 
                    &key (buffer-type :array-buffer) 
                      (draw-type :static-draw))
  "This function populates an opengl buffer with the contents 
   of the array. You can also pass in the buffer type and the 
   draw type this buffer is to be used for.
   
   The function returns a list of buffer formats which can
   be passed, as they are or mixed with others from other 
   buffers, and used to create VAOs."
  (bind-buffer buffer-type buffer)
  (gl:buffer-data buffer-type draw-type array)
  (setf (glbuffer-format buffer) 
        (loop for formatter in (attrib-formats (gl::gl-array-type array))
           for i from 0
           collect (funcall formatter 0)))
  buffer)

(defun multi-populate-buffer (buffer arrays 
                              &key (buffer-type :array-buffer) 
                                (draw-type :static-draw))
  "This beast will take a list of arrays and auto-magically
   push them into a buffer taking care of both interleaving 
   and sequencial data and handling all the offsets.
 
   The function returns a list of buffer formats which can
   be passed, as they are or mixed with others from other 
   buffers, and used to create VAOs."
  (let* ((array-byte-sizes (loop for array in arrays
                              collect 
                                (gl:gl-array-byte-size array)))
         (total-size (apply #'+ array-byte-sizes)))
    (bind-buffer buffer-type buffer)
    (gl:buffer-data buffer-type draw-type (car arrays)
                    :size total-size)
    (setf (glbuffer-format buffer) 
          (loop for array in (cdr arrays)
             for size in array-byte-sizes
             with offset = 0
             with attr-count = 0
             do (progn
                  (gl:buffer-sub-data buffer-type array
                                      :buffer-offset offset)
                  (setf offset (+ offset size)))
             collect (loop for formatter in (attrib-formats
                                             (gl::gl-array-type array))
                        for i from 0
                        do (setf attr-count (1+ attr-count))
                        collect (funcall formatter offset)))))
  buffer)

;;;--------------------------------------------------------------
;;; VAOS ;;;
;;;------;;;

;; This creates a function call bind-vao which only evaluates
;; 'gl:bind-vertex-array' when vao-id is different from the 
;; value of vao-id last time bind-vao was called.
(base-macros:defmemo bind-vao (vao-id)
  (gl:bind-vertex-array vao-id))
(setf (documentation 'bind-vao 'function) 
      "Binds the vao specfied")

(defun bind-vertex-array (vao-id)
  "Binds the vao specfied"
  (bind-vao vao-id))

;;[TODO] can bind-buffer inside vao have any other target
;;       than :array-buffer ?
;;[TODO] is enable-vertex-attrib-array relevent here?
;;       it is superseded by later calls?
(defun make-vao (buffer/s/formats &key (element-buffer nil))
  "This function takes a list of buffers formats and 
   optionaly an element buffer and returns a new vertex
   array object which can then be used in a stream."
  (labels ((bind-format (buffer format attr-num)
	     (bind-buffer :array-buffer buffer)
	     (gl:enable-vertex-attrib-array attr-num)
	     (apply #'%gl:vertex-attrib-pointer
		    (cons attr-num format))))
    (let ((vao-id (gl:gen-vertex-array))
	  (attr-num 0))
      (bind-vao vao-id)
      (loop for buffer-format in (if 
				  (glbuffer-p buffer/s/formats)
				  (list buffer/s/formats)
				  buffer/s/formats)
	 do (if (listp buffer-format)
		(progn
		  (bind-format (car buffer-format)
			       (cdr buffer-format)
			       attr-num)
		  (incf attr-num))
		(loop for format in (glbuffer-format buffer-format)
		   do (bind-format buffer-format
				   format
				   attr-num)
		     (incf attr-num))))
      (when element-buffer
	(bind-buffer :element-array-buffer element-buffer))
      (bind-vao 0)
      vao-id)))

;;;--------------------------------------------------------------
;;; STREAMS ;;;
;;;---------;;;

;; [TODO] element-type should definately not be here
;;        I'm not even sure whether draw-style should be either
(defstruct gl-stream 
  vao
  (start 0 :type unsigned-byte)
  (length 1 :type unsigned-byte)
  (element-type nil)
  (draw-style :triangles))


;;;--------------------------------------------------------------
;;; UNIFORMS ;;;
;;;----------;;;

;; FLOATS

(declaim (inline uniform-1f)
         (ftype (function ((integer)
                           (single-float)) 
                          (single-float)) 
                uniform-1f))
(defun uniform-1f (location val-f)
  (declare (type integer location)
           (type single-float val-f))
  (%gl:uniform-1f location val-f)
  val-f)


(declaim (inline uniform-2f)
         (ftype (function ((integer)
                           (simple-array single-float (2))) 
                          (simple-array single-float (2))) 
                uniform-2f))
(defun uniform-2f (location vec2)
  (declare (type integer location)
           (type (simple-array single-float (2)) vec2))
  (%gl:uniform-2f location 
                  (aref vec2 0) (aref vec2 1))
  vec2)


(declaim (inline uniform-3f)
         (ftype (function ((integer)
                           (simple-array single-float (3))) 
                          (simple-array single-float (3))) 
                uniform-3f))
(defun uniform-3f (location vec3)
  (declare (type integer location)
           (type (simple-array single-float (3)) vec3))
  (%gl:uniform-3f location 
                  (aref vec3 0) (aref vec3 1) (aref vec3 2))
  vec3)


(declaim (inline uniform-4f)
         (ftype (function ((integer)
                           (simple-array single-float (4))) 
                          (simple-array single-float (4))) 
                uniform-4f))
(defun uniform-4f (location vec4)
  (declare (type integer location)
           (type (simple-array single-float (4)) vec4))
  (%gl:uniform-4f location 
                  (aref vec4 0) (aref vec4 1) 
                  (aref vec4 2) (aref vec4 3))
  vec4)

;; INTEGERS

(declaim (inline uniform-1i)
         (ftype (function ((integer)
                           (integer)) 
                          (integer)) 
                uniform-1i))
(defun uniform-1i (location val-i)
  (declare (type integer location)
           (type integer val-i))
  (%gl:uniform-1i location val-i)
  val-i)


(declaim (inline uniform-2i)
         (ftype (function ((integer)
                           (simple-array integer (2))) 
                          (simple-array integer (2))) 
                uniform-2i))
(defun uniform-2i (location vec2)
  (declare (type integer location)
           (type (simple-array integer (2)) vec2))
  (%gl:uniform-2i location 
                  (aref vec2 0) (aref vec2 1))
  vec2)


(declaim (inline uniform-3i)
         (ftype (function ((integer)
                           (simple-array integer (3))) 
                          (simple-array integer (3))) 
                uniform-3i))
(defun uniform-3i (location vec3)
  (declare (type integer location)
           (type (simple-array integer (3)) vec3))
  (%gl:uniform-3i location 
                  (aref vec3 0) (aref vec3 1) (aref vec3 2))
  vec3)


(declaim (inline uniform-4i)
         (ftype (function ((integer)
                           (simple-array integer (4))) 
                          (simple-array integer (4))) 
                uniform-4i))
(defun uniform-4i (location vec4)
  (declare (type integer location)
           (type (simple-array integer (4)) vec4))
  (%gl:uniform-4i location 
                  (aref vec4 0) (aref vec4 1) 
                  (aref vec4 2) (aref vec4 3))
  vec4)


;; Matrices

(declaim (inline uniform-matrix2)
         (ftype (function ((integer)
                           (simple-array single-float (4))) 
                          (simple-array single-float (4))) 
                uniform-matrix2))
(defun uniform-matrix2 (location mat2)
  (declare (type integer location)
           (type (simple-array single-float (4)) mat2))
  (with-foreign-object (array '%gl:float 4)
    (dotimes (j 4)
      (setf (mem-aref array '%gl:float j)
            (aref mat2 j)))
    (%gl:uniform-matrix-2fv location 1 nil array))
  mat2)


(declaim (inline uniform-matrix3)
         (ftype (function ((integer)
                           (simple-array single-float (9))) 
                          (simple-array single-float (9))) 
                uniform-matrix3))
(defun uniform-matrix3 (location mat3)
  (declare (type integer location)
           (type (simple-array single-float (9)) mat3))
  (with-foreign-object (array '%gl:float 9)
    (dotimes (j 9)
      (setf (mem-aref array '%gl:float j)
            (aref mat3 j)))
    (%gl:uniform-matrix-3fv location 1 nil array))
  mat3)


(declaim (inline uniform-matrix4)
         (ftype (function ((integer)
                           (simple-array single-float (16))) 
                          (simple-array single-float (16))) 
                uniform-matrix4))
(defun uniform-matrix4 (location mat4)
  (declare (type integer location)
           (type (simple-array single-float (16)) mat4))
  (with-foreign-object (array '%gl:float 16)
    (dotimes (j 16)
      (setf (mem-aref array '%gl:float j)
            (aref mat4 j)))
    (%gl:uniform-matrix-4fv location 1 nil array))
  mat4)

(defun glsl-uniform-type-to-function (type)
  (case type
    ((:float :float-arb) #'uniform-1f)
    ((:float-vec2 :float-vec2-arb) #'uniform-2f)
    ((:float-vec3 :float-vec3-arb) #'uniform-3f)
    ((:float-vec4 :float-vec4-arb) #'uniform-4f)
    ((:int :int-arb :bool :bool-arb) #'uniform-1i)
    ((:int-vec2 :int-vec2-arb
                :bool-vec2 :bool-vec2-arb) #'uniform-2i)
    ((:int-vec3 :int-vec3-arb
                :bool-vec3 :bool-vec3-arb) #'uniform-3i)
    ((:int-vec4 :int-vec4-arb
                :bool-vec4 :bool-vec4-arb) #'uniform-4i)
    ((:float-mat2 :float-mat2-arb) #'uniform-matrix2)
    ((:float-mat3 :float-mat3-arb) #'uniform-matrix3)
    ((:float-mat4 :float-mat4-arb) #'uniform-matrix4)
    (t (error "Sorry cepl doesnt handle that type yet"))))

;;;--------------------------------------------------------------
;;; PROGRAMS ;;;
;;;----------;;;

(defun program-attrib-count (program)
  "Returns the number of attributes used by the shader"
  (get-program program :active-attributes))

(defun program-attributes (program)
  "Returns a list of details of the attributes used by
   the program. Each element in the list is a list in the
   format: (attribute-name attribute-type attribute-size)"
  (loop for i from 0 below (program-attrib-count program)
     collect (multiple-value-bind (size type name)
                 (get-active-attrib program i)
               (list name type size))))

(defun program-uniform-count (program)
  "Returns the number of uniforms used by the shader"
  (get-program program :active-uniforms))

(defun program-uniforms (program-id)
  "Returns a list of details of the uniforms used by
   the program. Each element in the list is a list in the
   format: (uniform-name uniform-type uniform-size)"
  (loop for i from 0 below (program-uniform-count program-id)
     collect (multiple-value-bind (size type name)
                 (get-active-uniform program-id i)
               (list name type size))))

(defun get-uniforms (program)
  "Takes a program and returns a list of uniforms"
  (let ((program-id (funcall program t)))
    (loop for detail in (program-uniforms program-id)
       collect (list (utils:make-keyword
                      (lispify-name (car detail)))
                     (second detail)
                     (third detail)))))

(base-macros:defmemo use-program (program-id)
  (gl:use-program program-id))
(setf (documentation 'use-program 'function) 
      "Installs a program object as part of current rendering state")


(defun shader-type-from-path (path)
  "This uses the extension to return the type of the shader.
   Currently it only recognises .vert or .frag files"
  (let* ((plen (length path))
         (exten (subseq path (- plen 5) plen)))
    (cond ((equal exten ".vert") :vertex-shader)
          ((equal exten ".frag") :fragment-shader)
          (t (error "Could not extract shader type from shader file extension (must be .vert or .frag)")))))

(defun make-shader (file-path 
                    &optional 
                      (shader-type 
                       (shader-type-from-path file-path))
                      (shader-id 
                       (gl:create-shader shader-type)))
  "This makes a new opengl shader object by compiling the text
   in the specified file and, unless specified, establishing the
   shader type from the file extension"
  (restart-case  
      (let ((source-string (utils:file-to-string file-path)))
        (gl:shader-source shader-id source-string)
        (gl:compile-shader shader-id)
        ;;check for compile errors
        (if (not (gl:get-shader shader-id :compile-status))
            (error `cgl-compile-shader-error
                   :text (format nil 
                                 "Error compiling shader ~a~%~a" 
                                 file-path
                                 (gl:get-shader-info-log 
                                  shader-id)))))
    (reload-recompile-shader () (make-shader file-path
                                             shader-type
                                             shader-id)))
  shader-id)

;; [TODO] Add a make-shaders function which takes a list of 
;;        shader paths and returns a list of shaders
(defun make-shaders (&rest shader-paths)
  (mapcar #'cgl:make-shader shader-paths))

(defun link-shaders (shaders)
  "Links all the shaders provided and returns an opengl program
   object"
  (let ((program (gl:create-program)))
    (loop for shader in shaders
       do (gl:attach-shader program shader))
    (gl:link-program program)
    ;;check for linking errors
    (if (not (gl:get-program program :link-status))
        (error (format nil "Error Linking Program~%~a" 
                       (gl:get-program-info-log program))))
    (loop for shader in shaders
       do (gl:detach-shader program shader))
    program))

;; [TODO] Do we really nee to create a null array every time?
;;        This can't be efficient
(defun no-bind-draw-one (stream)
  "This draws the single stream provided using the currently 
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  (let ((e-type (gl-stream-element-type stream)))
    (bind-vao (gl-stream-vao stream))
    (if e-type
        (gl:draw-elements (gl-stream-draw-style stream)
                          (gl::make-null-gl-array e-type)
                          :count (gl-stream-length stream))
        (%gl:draw-arrays (gl-stream-draw-style stream)
                         (gl-stream-start stream)
                         (gl-stream-length stream)))))


;; get uniforms
;; turn names into lispy keyword names
;; hash those against lambdas that will call the cl-opengl 
;;   functions needed to modify that uniform with the details 
;;   pre-populated
;; make a lambda that takes a list of streams, a draw style
;;   e.g triangles, and &keys the rest where the keys are 
;;   the uniform names you will update.
;;   if there are keys lookup the uniforms and call the lambdas
;;   loop through the renderables and call draw on each 

;; [TODO] Doesnt handle arrays yet
(defun hash-uniform-handlers (program-id uniform-details)
  "This function takes a list of uniforms details (as given 
   by program-uniforms) and returns a hash table matching as
   'lispified' version of the uniform name to a lambda which 
   will update the specific uniform."
  (let ((uni-hash (make-hash-table 
                   :test `eq 
                   :size (length uniform-details))))
    (loop for detail in uniform-details
       do (setf (gethash (utils:make-keyword
                          (lispify-name (car detail))) uni-hash)
                (let ((location (gl:get-uniform-location 
                                 program-id
                                 (car detail)))
                      (uniform-updater (glsl-uniform-type-to-function 
                                        (second detail))))
                  (lambda (val)
                    (funcall uniform-updater location val)))))
    uni-hash))

(defun lispify-name (name)
  "take a string and changes it to uppercase and replaces
   all underscores _ with minus symbols -"
  (string-upcase (substitute #\- #\_ name)))

(defun make-program (shaders)
  "Takes a list of shader, links them into an opengl program
   object and wraps it in a lambda.
   The reasoning for this is that a shader program is essentially
   a function in that it takes a values and produces an output
   while escapsulating the inner workings from the rest of the
   program. The fact that the program runs on the gpu rather than
   the cpu is incidental.
   So an opengl 'program' is a function. It is a function that
   takes as it main argument a list of streams which it 's job is
   to render. The other arguments are &key arguments where the
   key is the name of the uniform and the value is the value
   you wish to set the uniform to.
   Opengl Uniforms maintain between calls to the program so you
   only need to specify a uniform argument when you wish to 
   change its value."
  (let* ((program-id (link-shaders shaders))
         (uniform-details (program-uniforms program-id))
         (uniform-hash (hash-uniform-handlers program-id
                                              uniform-details))
         (uniform-lispy-names (loop for detail in uniform-details
                                 collect (utils:make-keyword 
                                          (lispify-name
                                           (car detail))))))
    (lambda (streams &rest uniforms)	     
      (use-program program-id)
      (when uniforms
        (loop for i below (length uniforms) by 2
           do (let ((uniform-updater (gethash (nth i uniforms) 
                                              uniform-hash))) 
                (if uniform-updater
                    (funcall uniform-updater (nth (1+ i) uniforms))
                    (error (format nil "Uniform not found, must be one of ~{~a~^, ~}" uniform-lispy-names))))))
      (if (eq streams t)
          program-id
          (loop for stream in streams
             do (no-bind-draw-one stream))))))


(defun set-program-uniforms (program &rest uniforms)
  "This is a really syntactic sugar around setting uniforms
   of a program. It takes a program and &key arguments where
   the key is the name of the uniform and the value is the value
   you wish to set the uniform to."
  (apply program (cons nil uniforms)))

(defun draw-streams (program streams &rest uniforms)
  "This is a really syntactic sugar around funcall'ing a program.
   It takes a program, a list of streams and &key arguments where
   the key is the name of the uniform and the value is the value
   you wish to set the uniform to."
  (apply program (cons streams uniforms)))
