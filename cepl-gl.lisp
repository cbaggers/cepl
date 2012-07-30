;; This is to shadow and tidy up the cl-opengl feature set.
;; We will define our own abstractions here also so if the user
;; wants to interact with opengl they dont have to think 'Is it
;; in cl-opengl or cepl-gl?' it will just be cepl-gl.
;; Hopefully this will help with cognitive flow.

(in-package :cepl-gl)

(define-condition cgl-compile-shader-error (error)
  ((text :initarg :text :reader text)))


;;;--------------------------------------------------------------
;;; SHADOW'D OPENGL FUNCTIONS ;;;
;;;---------------------------;;;

;; These two should be fine as it only applies to vertex arrays
;; 
(defun enable-vertex-attrib-array (buffer-id index)
  (bind-buffer :array-buffer buffer-id)
  (gl:enable-vertex-attrib-array index))

(defun vertex-attrib-pointer (buffer-id index size type 
			      normalised stride pointer)
  (bind-buffer :array-buffer buffer-id)
  (gl:vertex-attrib-pointer index size type normalised stride
			    pointer))

(defun uniform-matrix (program-id location dim matrices)
  (use-program program-id)
  (gl:uniform-matrix location dim matrices nil))


;;;--------------------------------------------------------------
;;; GL-ARRAYS ;;;
;;;-----------;;;

;; (setf (gl:glaref array index 'x) 1.675)

(defun simple-1d-populate (array data)
  (loop for datum in data
       for i from 0
       do (setf (aref-gl array i) datum)))

(defun simple-array-attrib-error (&rest args)
  (declare (ignore args))
  (error "Sorry, attribute-formats cannot be generated from primitive array types"))

;;[TODO] What types do we need to support here?
(loop for type-name in `(:float :short)
   do (progn
	(setf (get type-name 'cgl-destructuring-populate)
	      #'simple-1d-populate)
	(setf (get type-name 'vertex-attrib-formats) nil)))

(defmacro define-attribute-format (name &rest args)
  `(define-interleaved-attribute-format ,name ,args))

(defmacro define-interleaved-attribute-format (name &body clauses)
  "Defines a vertex array format spcification. Each clause has
the format (array-type parameter*) where array-type is always
VERTEX-ATTRIB.

Parameters are keyword arguments for the corresponding array
type. The following parameters are supported:

    :TYPE -- array element type (all array types)
    :COMPONENTS -- list of component (slot) names for this array (all types)
    :STAGE -- active texture for the array (TEX-COORD type)
    :NORMALIZED -- whether values should be normalized (VERTEX-ATTRIB)
"
  `(progn
     (defcstruct ,name
       ,@(mapcan #'emit-attrib-struct-clause clauses))
     (setf (get ',name 'cgl-destructuring-populate)
	   (destructured-populate ,@clauses))
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
  (destructuring-bind (&key (normalized :false) type components
			    &allow-other-keys)
      clause
    `(,(length components) ,type ,normalized ,stride ,offset)))

(defun emit-attrib-struct-clause (clause)
  (destructuring-bind (&key type components &allow-other-keys)
      clause
    (loop for c in components
          collect `(,c ,type))))

(defun attrib-formats (array-type)
  (get array-type 'vertex-attrib-formats))

(defun attrib-format (array-type &optional  (sub-attrib 0)
				    (offset 0))
  (let ((formats (get array-type 'vertex-attrib-formats)))
    (if (null formats)
	(error "Sorry but attribute formats are not available for this array-type")
	(funcall (nth sub-attrib formats) offset))))

(defun destructuring-populate (array data)
  (let ((func (get (gl::gl-array-type array) 
		   'cgl-destructuring-populate)))
    (funcall func array data)
    array))

(defun destructuring-allocate (array-type data)
  (let ((array (alloc-array-gl array-type (length data))))
    (destructuring-populate array data)))

;;[TODO] These names are too similar
(defmacro destructured-populate (&body clauses)
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
  (loop for clause in clauses
       collect (destructuring-bind (&rest rest &key components
					  &allow-other-keys)
		   clause
		 (declare (ignore rest))
		 components)))


;; needs to be here right now as I don't know how to set
;; vertex-array-binder to be in cl-opengl
(defun alloc-gl-array (type count)
  (gl::make-gl-array :pointer (foreign-alloc type :count count)
                     :size count :type type))


(defun alloc-array-gl (type count)
  (alloc-gl-array type count))


;; Grr, wanted to use this but coudl work out how to do the 
;; setf version
;; (setf (symbol-function 'aref-gl) (symbol-function 'glaref))
(declaim (inline aref-gl))
(defun aref-gl (array index &optional (component nil c-p))
  "Returns the INDEX-th component of ARRAY. If COMPONENT is
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

(base-macros:defmemo bind-buffer (target buffer-id)
  (gl:bind-buffer target buffer-id))

(defun gen-buffer ()
  (first (gen-buffers 1)))

;;(cons (+ (car b) 100) (subseq b 1))
;; SIZE TYPE NORMALIZED STRIDE POINTER
(defun buffer-data (buffer-id array 
		    &key (buffer-type :array-buffer) 
		          (draw-type :static-draw))
  (bind-buffer buffer-type buffer-id)
  (gl:buffer-data buffer-type draw-type array)
  (loop for formatter in (attrib-formats (gl::gl-array-type array))
     for i from 0
     collect (cons buffer-id (funcall formatter 0))))

(defun multi-populate-buffer (buffer-id arrays 
			     &key (buffer-type :array-buffer) 
			           (draw-type :static-draw))
  "This beast will take a list of arrays and auto-magically
   push them into a buffer taking care of both interleaving 
   and sequencial data and handling all the offsets."
  (let* ((array-byte-sizes (loop for array in arrays
				collect 
				(gl:gl-array-byte-size array)))
	 (total-size (apply #'+ array-byte-sizes)))
    (bind-buffer buffer-type buffer-id)
    (gl:buffer-data buffer-type draw-type (car arrays)
		    :size total-size)
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
		  collect (cons buffer-id 
				(funcall formatter offset))))))

;; get a list of the array size in bytes of all the arrays
;; use buffer-data to push the first array in.
;; the use buffer-sub-data to push every other array using the 
;; list of sizes to handle the offset
;; return the buffer-id
;; [TODO] Needs to handle offsets into the array


;;;--------------------------------------------------------------
;;; VAOS ;;;
;;;------;;;

;; This creates a function call bind-vao which only evaluates
;; 'gl:bind-vertex-array' when vao-id is different from the 
;; value of vao-id last time bind-vao was called.
(base-macros:defmemo bind-vao (vao-id)
  (gl:bind-vertex-array vao-id))

(defun bind-vertex-array (vao-id)
  (bind-vao vao-id))

;;[TODO] can bind-buffer inside vao have any other target
;;       than :array-buffer ?
;;[TODO] is enable-vertex-attrib-array relevent here?
;;       it is superseded by later calls?
(defun make-vao (formats &optional (element-buffer nil))
  ;;make vao here
  (let ((vao-id (gl:gen-vertex-array)))
    (bind-vao vao-id)
    (loop for attr-format in formats
       for attr-num from 0
       do (let ((buffer-id (car attr-format)))
	    (bind-buffer :array-buffer buffer-id)
	    (gl:enable-vertex-attrib-array attr-num)
	    (apply #'%gl:vertex-attrib-pointer
		   (cons attr-num (cdr attr-format)))))
    (when element-buffer
      (bind-buffer :element-array-buffer element-buffer))
    (bind-vao 0)
    vao-id))


;;;--------------------------------------------------------------
;;; STREAMS ;;;
;;;---------;;;

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
  (get-program program :active-attributes))

(defun program-attributes (program)
  (loop for i from 0 below (program-attrib-count program)
     collect (multiple-value-bind (size type name)
		 (get-active-attrib program i)
	       (list name type size))))

(defun program-uniform-count (program)
  (get-program program :active-uniforms))

(defun program-uniforms (program-id)
  (loop for i from 0 below (program-uniform-count program-id)
     collect (multiple-value-bind (size type name)
		 (get-active-uniform program-id i)
	       (list name type size))))

(defun get-uniforms (program)
  (let ((program-id (funcall program t)))
    (loop for detail in (program-uniforms program-id)
       collect (list (utils:make-keyword
		      (lispify-name (car detail)))
		     (second detail)))))

(base-macros:defmemo use-program (program-id)
   (gl:use-program program-id))


;; This is one DUMB function...but its serving my needs for now
(defun shader-type-from-path (path)
  "This uses the extension to return the type of the shader"
  (let* ((plen (length path))
	 (exten (subseq path (- plen 5) plen)))
    (cond ((equal exten ".vert") :vertex-shader)
	  ((equal exten ".frag") :fragment-shader)
	  (t (error "Could not extract shader type from shader"))
	  )))

(defun make-shader (file-path &optional 
				(shader-type 
			      (shader-type-from-path file-path))
				(shader-id 
				 (gl:create-shader shader-type)))
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

(defun link-shaders (shaders)
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
;; remove (gl:delete-shader shader) as it messes up retrying 
;; stuff

(defun no-bind-draw-one (stream)
  (let ((e-type (gl-stream-element-type stream)))
    (bind-vao (gl-stream-vao stream))
    (if e-type
	(gl:draw-elements (gl-stream-draw-style stream)
			  (gl::make-null-gl-array e-type)
			  :count (gl-stream-length stream))
	(%gl:draw-arrays (gl-stream-draw-style stream)
			 (gl-stream-start stream)
			 (gl-stream-length stream)))))

	;; (%gl:draw-elements (gl-stream-draw-style stream)
	;; 		   (gl-stream-length stream) 
	;; 		   (gl::make-null-gl-array e-type)
	;; 		   (cffi:make-pointer 0))
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

;; bah move draw style to the stream struct

;; (("cameratoclipmatrix" :FLOAT-MAT4-ARB 1) ("jam" :FLOAT 1)
;;  ("modelToCameraMatrix" :FLOAT-MAT4-ARB 1)) 

;; [TODO] Doesnt handle arrays yet
(defun hash-uniform-handlers (program-id uniform-details)
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
  (string-upcase (substitute #\- #\_ name)))

(defun make-program (shaders)
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
  (apply program (cons nil uniforms)))

(defun draw-streams (program streams &rest uniforms)
  (apply program (cons streams uniforms)))
