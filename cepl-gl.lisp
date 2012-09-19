;; This is to provide abstractions over the cl-opengl-bindings
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

;;
;; [TODO] Look at pretty printing for our new types
;;        http://www.lysator.liu.se/~boris/ll/clm/node259.html
;;
;; [TODO] Write all the doco strings again
;;
;; [TODO] work out if cgl will be :use'd in the cepl package
;;
;; [TODO] add appending arrays to make-gpu-array

(in-package :cepl-gl)

(defparameter *pprint-cgl-types* t)

;;;--------------------------------------------------------------
;;; BUFFERS ;;;
;;;---------;;;

(defstruct glbuffer
  (buffer-id (car (gl:gen-buffers 1)))
  (format nil))

(base-macros:defmemo bind-buffer (buffer buffer-target)
  (gl:bind-buffer buffer-target (glbuffer-buffer-id buffer)))

(defun gen-buffer (&key initial-contents 
		     (buffer-target :array-buffer) 
		     (usage :static-draw))
  (declare (symbol buffer-target usage))
  "Generates a single opengl buffer"
  (let ((new-buffer (make-glbuffer)))
    (if initial-contents
        (buffer-data new-buffer initial-contents buffer-target 
		     usage)
        new-buffer)))

;; buffer format is a list whose sublists are of the format
;; type, index-length, byte-offset-from-start-of-buffer

(defun buffer-data (buffer gl-array buffer-target usage
                    &key (offset 0)
                          (size (glarray-byte-size gl-array)))
  "This function populates an opengl buffer with the contents 
   of the array. You can also pass in the buffer type and the 
   draw type this buffer is to be used for.
   
   The function returns a list of buffer formats which can
   be passed, as they are or mixed with others from other 
   buffers, and used to create VAOs."
  (bind-buffer buffer buffer-target)
  (%gl:buffer-data buffer-target
                   size 
                   (cffi:inc-pointer 
		    (glarray-pointer gl-array)
		    (foreign-type-index (glarray-type gl-array)
					offset))
                   usage)
  (setf (glbuffer-format buffer) 
	(list `(,(glarray-type gl-array)
		 ,(glarray-length gl-array)
		 0)))
  buffer)


;; This can really fuck up a buffer format if not careful so
;; look into handling this.
(defun buffer-sub-data (buffer gl-array byte-offset buffer-target
			&key (safe t))
  (declare (glarray gl-array))
  (let ((byte-size (glarray-byte-size gl-array)))
    (when (and safe (loop for format in (glbuffer-format buffer)
		       when (and (< byte-offset (third format))
				 (> (+ byte-offset byte-size)
				    (third format)))
		       return t))
      (error "The data you are trying to sub into the buffer crosses the boundaries specified format. If you want to do this anyway you should set :safe to nil, though it is not advised as your buffer format would be invalid"))
    (bind-buffer buffer buffer-target)
    (%gl:buffer-sub-data buffer-target
                         byte-offset
                         byte-size
                         (glarray-pointer gl-array)))
  buffer)


(defun multi-buffer-data (buffer arrays buffer-target usage)
  "This beast will take a list of arrays and auto-magically
   push them into a buffer taking care of both interleaving 
   and sequencial data and handling all the offsets."
  (let* ((array-byte-sizes (loop for array in arrays
                              collect 
                                (glarray-byte-size array)))
         (total-size (apply #'+ array-byte-sizes)))
    (bind-buffer buffer buffer-target)
    (buffer-data buffer (first arrays) buffer-target usage
                 :size total-size)
    (setf (glbuffer-format buffer) 
          (loop for gl-array in arrays
             for size in array-byte-sizes
             with offset = 0
             collect `(,(glarray-type gl-array)
			,(glarray-length gl-array)
			,offset)
             do (buffer-sub-data buffer gl-array offset
				 buffer-target)
	       (setf offset (+ offset size)))))
  buffer)

(defun buffer-reserve-raw-block (buffer size-in-bytes buffer-target usage)
  (bind-buffer buffer buffer-target)
  (%gl:buffer-data buffer-target size-in-bytes
                   (cffi:null-pointer) usage)
  buffer)

(defun buffer-reserve-block (buffer type length buffer-target usage)
  (bind-buffer buffer buffer-target)
  (buffer-reserve-raw-block buffer
                            (foreign-type-index type length)
                            buffer-target
                            usage)
  ;; make format
  (setf (glbuffer-format buffer) `((,type ,length ,0)))
  buffer)

(defun buffer-reserve-blocks (buffer types-and-lengths
                              buffer-target usage)
  (let ((size-in-bytes 0))
    (setf (glbuffer-format buffer) 
          (loop for (type length)
             in types-and-lengths
	     do (incf size-in-bytes 
		      (foreign-type-index type length))
             collect `(,type ,length ,size-in-bytes)))
    (buffer-reserve-raw-block buffer size-in-bytes
                              buffer-target usage)
    buffer))

(defgeneric gl-type-format (array-type &optional address-offset)
  (:documentation "given an array type and an offset in bytes
this command returns a list with the sublists being the 
layout of the attributes of the type.
Thats a pretty ugly description so here is what it coudl be used
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

;;;--------------------------------------------------------------
;;; VAOS ;;;
;;;------;;;


(base-macros:defmemo bind-vao (vao)
  (gl:bind-vertex-array vao))
(setf (documentation 'bind-vao 'function) 
      "Binds the vao specfied")

(defun bind-vertex-array (vao)
  "Binds the vao specfied"
  (bind-vao vao))


(defun make-vao (buffers/formats &key element-buffer)
  (make-vao-from-formats
   (loop for item in buffers/formats
      collect 
        (cond 
          ((listp item) item)
          ((glbuffer-p item)
           (cons item 
                 (loop for attrf in (glbuffer-format item)
                    append (gl-type-format 
                            (first attrf) (third attrf)))))))
   :element-buffer element-buffer))

;; ((buffer1 (attr-format1) (attr-format2))
;;  (buffer2 (attr-format3) (attr-format4)))
(defun make-vao-from-formats (formats &key element-buffer)
  (labels ((bind-format (buffer format attr-num)
	     (bind-buffer buffer :array-buffer)
	     (gl:enable-vertex-attrib-array attr-num)
	     (apply #'%gl:vertex-attrib-pointer
		    (cons attr-num format))))
    (let ((vao (gl:gen-vertex-array))
	  (attr-num 0))
      (bind-vao vao)
      (loop for format in formats
	 do (let ((buffer (car format)))
	      (loop for attr-details in (rest format)
		 do (bind-format buffer attr-details attr-num)
		   (incf attr-num))))
      
      (when element-buffer
	(bind-buffer :element-array-buffer element-buffer))
      vao)))

(defun make-vao-from-buffer (buffer &key element-buffer)
  (make-vao (list buffer) :element-buffer element-buffer))

(defun make-vao-from-gpu-arrays (gpu-arrays 
				 &optional indicies-array)
  (let ((element-buffer (when indicies-array
			  (gpuarray-buffer indicies-array))))
    (make-vao-from-formats
     (loop for gpu-array in gpu-arrays
	collect
	  (let* ((buffer (gpuarray-buffer gpu-array))
		 (buffer-format (nth (gpuarray-format-index 
				      gpu-array)
				     (glbuffer-format buffer))))
	    (cons buffer
		  (gl-type-format (first buffer-format)
				  (+ (third buffer-format)
				     (gpuarray-start gpu-array))))))
     :element-buffer element-buffer)))


;;;--------------------------------------------------------------
;;; GLSTRUCTS ;;;
;;;-----------;;;

(defmacro defglstruct (name &body slot-descriptions)
  (let ((clauses (glstruct-slot-descriptions-to-clauses
		  slot-descriptions)))
    `(progn 
       ;; make the c-struct that will be used by the glarrays
       (defcstruct ,name 
         ,@(loop for clause in clauses append (rest clause)))
       ;; make the getters for the foreign array
       ;; [TODO] make this generic for gl-arrays and gpu-arrays
       ,@(loop for clause in clauses
            collect 
              (gen-component-getters name clause))
       ;; make the setters for the foreign array
       ;; [TODO] make this generic for gl-arrays and gpu-arrays
       ,@(loop for clause in clauses
            collect 
              (gen-component-setters name clause))
       ;; make the generic destructuring-populate method for this
       ;; type.
       ,@(let ((loop-token (gensym "LOOP"))
	       (clause-names (mapcar #'car clauses)))
              (list
               `(defmethod dpopulate ((array-type (eql ',name))
                                      gl-array
                                      data)
                  (loop for ,clause-names in data
                     for ,loop-token from 0
                     do ,@(loop for c in clause-names
                             collect
                               `(setf (,(cepl-utils:symb name
                                                         '-
                                                         c)
                                        (aref-gl gl-array ,loop-token))
                                      ,c))))
               `(defmethod glpull-entry ((array-type (eql ',name))
                                         gl-array
                                         index)
                  (list ,@(loop for c in clause-names
                             collect
                               `(,(cepl-utils:symb name
                                                   '-
                                                   c)
                                  (aref-gl gl-array index)))))))
       ,(let ((stride (if (> (length clauses) 1)
                          `(foreign-type-size ',name)
                          0)))
             `(defmethod gl-type-format ((array-type (EQL ',name)) &optional (address-offset 0))
                (list ,@(loop for slotd in slot-descriptions
                           collect 
                             (destructuring-bind (slot-name &key type (length 1) (normalised nil) 
                                                            &allow-other-keys)
                                 slotd
                               `(list ,length ,type ,normalised ,stride 
                                      (cffi:make-pointer 
                                       (+ (foreign-slot-offset ',name ',(cepl-utils:symb slot-name 0))
                                          address-offset))))))))
       ',name)))


(defun gen-component-setters (type-name clause)
  (let ((comp-name (first clause))
	(slots (rest clause)))
    (if (eq (length slots) 1)
	(let ((slot (first slots)))
	  `(defun 
	       (setf ,(cepl-utils:symb type-name '- comp-name)) 
	       (value instance)
	     (setf (foreign-slot-value instance 
				       ',type-name
				       ',(first slot))
		   value)))
	`(defun 
	     (setf ,(cepl-utils:symb type-name '- comp-name))
	     (value instance)
	   ,@(loop for slot in slots
		for i from 0
		collect 
		  `(setf (foreign-slot-value instance
					     ',type-name
					     ',(first slot))
			 (aref value ,i)))
	   value))))

;; [TODO] Type specifiers for returned arrays? YES DEFINITELY
(defun gen-component-getters (type-name clause)
  (let ((comp-name (first clause))
	(slots (rest clause)))
    `(defun ,(cepl-utils:symb type-name '- comp-name) (instance)
       ,(if (eq (length slots) 1)
	    (let ((slot (first slots)))
	      `(foreign-slot-value instance 
				   ',type-name
				   ',(first slot)))
	    `(make-array ,(length slots)
			 ;; :element-type 
			 ;; ',(gl::symbolic-type->real-type 
			 ;;   (second (first slots)))
			 :initial-contents
			 (list ,@(loop for slot in slots
				    collect
				      `(foreign-slot-value 
					instance 
					',type-name
					',(first slot)))))))))

(defun glstruct-slot-descriptions-to-clauses (slot-descriptions)
  (loop for clause in slot-descriptions
     collect 
       (destructuring-bind (name &key type (length 1) 
                                 &allow-other-keys)
	   clause
	 (if (or (< length 1) (> length 4))
	     (error "Invalid length of slot"))
	 (cons name (loop for i below length
		       collect `(,(cepl-utils:symb name i)
				  ,type))))))

(defgeneric dpopulate (array-type gl-array data)
  (:documentation 
   "This is the function that actually does the work in the 
    destructuring populate"))

(defgeneric glpull-entry (array-type gl-array index)
  (:documentation 
   "Pull one entry from a glarray as a list of lisp objects"))

(defgeneric gl-pull (gl-object)
  (:documentation ""))

(defmethod gl-pull ((gl-object gpuarray))
  (gpu-array-pull gl-object))

(defmethod gl-pull ((gl-object glarray))
  (let ((array-type (glarray-type gl-object)))
    (loop for i below (glarray-length gl-object)
       collect (glpull-entry array-type gl-object i))))

(defgeneric gl-push (gl-object data)
  (:documentation ""))

(defmethod gl-push ((gl-object gpuarray) (data glarray))
  (gpu-array-push gl-object data)
  gl-object)

(defmethod gl-push ((gl-object gpuarray) (data list))
  (let ((gl-array (destructuring-allocate 
		   (gpuarray-type gl-object)
		   data)))
    (gpu-array-push gl-object gl-array)
    (free-gl-array gl-array))
  gl-object)

(defmethod gl-push ((gl-object glarray) (data list))
  (destructuring-populate gl-object data)
  gl-object)

(defun foreign-type-index (type index)
  (* (foreign-type-size type)
     index))

;;;--------------------------------------------------------------
;;; GLARRAYS ;;;
;;;----------;;;

;; the struct containing details about our gl-arrays
(defstruct (glarray (:copier nil))
  "Pointer to C array with size and type information attached."
  (pointer (null-pointer))
  (length 0 :type unsigned-byte)
  (type nil :type symbol))

(defun glarray-byte-size (gl-array)
  (declare (glarray gl-array))
  (* (glarray-length gl-array) 
     (foreign-type-size (glarray-type gl-array))))

;; check for glstruct type existance
(defun make-gl-array (&key length element-type initial-contents)
  (if initial-contents
      (destructuring-allocate element-type initial-contents)
      (make-glarray :pointer (foreign-alloc element-type 
					    :count length)
		    :length length 
		    :type element-type)))

(defun free-gl-array (array)
  "Frees an array allocated by ALLOC-GL-ARRAY."
  (foreign-free (glarray-pointer array)))

(declaim (inline aref-gl))
(defun aref-gl (array index)
  "Returns the INDEX-th component of gl-array."
  (mem-aref (glarray-pointer array) 
	    (glarray-type array) 
	    index))

(declaim (inline (setf aref-gl)))
(defun (setf aref-gl) (value array index)
  "Returns the INDEX-th component of gl-array."
  (setf (mem-aref (glarray-pointer array) 
                  (glarray-type array) 
                  index) value))

(defun destructuring-populate (gl-array data)
  "This function takes a gl-array and a list of data and 
   populates the gl-array using the data. 
   The data must be a list of sublists. Each sublist must
   be in the format expected by a destructuring-bind with
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
  (dpopulate (glarray-type gl-array) gl-array data)
  gl-array)

(defun destructuring-allocate (array-type data)
  "This function will create a new gl-array with a length
   equal to the length of the data provided, and then populate 
   the gl-array.

   The data must be a list of sublists. Each sublist must
   be in the format expected by a destructuring-bind with
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
  (let ((array (make-gl-array :element-type array-type 
                              :length (length data))))
    (destructuring-populate array data)
    array))


;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(let ((buffer-pool ()))
  (defun add-buffer-to-pool (buffer)
    (setf buffer-pool (cons buffer buffer-pool))
    buffer)

  (defun free-all-buffers-in-pool ()
    (mapcar #'(lambda (x) (declare (ignore x))
		      (print "freeing a buffer")) 
	    buffer-pool)))

;; [TODO] Can we get rid of target?
(defstruct gpuarray 
  buffer
  format-index
  (start 0)
  length
  index-array 
  (access-style :static-draw))

(defun gpuarray-format (gpu-array)
  (nth (gpuarray-format-index gpu-array)
       (glbuffer-format (gpuarray-buffer gpu-array))))

(defun gpuarray-type (gpu-array)
  (first (gpuarray-format gpu-array)))

(defun gpuarray-offset (gpu-array)
  (let ((format (gpuarray-format gpu-array)))
    (+ (third format)
       (foreign-type-index (first format)
                           (gpuarray-start gpu-array)))))

;; http://www.opengl.org/wiki/GLAPI/glBufferData
(defun make-gpu-array 
    (&key (initial-contents nil)
       (element-type (glarray-type initial-contents))
       (length (glarray-length initial-contents))
       (index-array nil)
       (access-style :static-draw)
       (location nil))
  (if location
      (error "Sorry I havent implemented adding gpu arrays
              to existing buffers yet.")
      (let ((buffer (add-buffer-to-pool (gen-buffer))))
        (make-gpuarray 
         :buffer
         (if initial-contents
             (buffer-data buffer initial-contents
			  (if index-array
			      :element-array-buffer
			      :array-buffer)
			  access-style)
             (buffer-reserve-block buffer element-type length
                                   (if index-array
				       :element-array-buffer
				       :array-buffer)
				   access-style))
         :format-index 0
         :length length
	 :index-array index-array
	 :access-style access-style))))


(defun make-gpu-arrays (gl-arrays &key index-array
				    (access-style :static-draw))
  (let ((buffer (add-buffer-to-pool
                 (multi-buffer-data (gen-buffer) 
				    gl-arrays 
				    (if index-array
					:element-array-buffer
					:array-buffer)
				    access-style))))
    (loop for gl-array in gl-arrays
       for i from 0 collect 
         (make-gpuarray :buffer buffer
                        :format-index i
                        :length (glarray-length gl-array)
			:index-array index-array
			:access-style access-style))))


(defun gpu-sub-array (gpu-array start end)
  (let* ((parent-start (gpuarray-start gpu-array))
         (parent-end (+ parent-start 
                        (gpuarray-length gpu-array)))
         (new-start (+ parent-start (max 0 start))))
    (when (< end start) 
      (error "end cannot be less than start"))
    (make-gpuarray 
     :buffer (gpuarray-buffer gpu-array)
     :format-index (gpuarray-format-index gpu-array)
     :start new-start
     :length (- (min end parent-end) start)
     :index-array (gpuarray-index-array gpu-array)
     :access-style (gpuarray-access-style gpu-array))))

(defun gpu-array-pull (gpu-array)
  (let ((type (gpuarray-type gpu-array)))
    (with-gpu-array-as-gl-array (tmp gpu-array :read-only)
      (loop for i below (gpuarray-length gpu-array)
         collect (glpull-entry type tmp i)))))

(defun gpu-array-push (gpu-array gl-array)  
  (let* ((buffer (gpuarray-buffer gpu-array))
         (format (nth (gpuarray-format-index gpu-array)
                      (glbuffer-format buffer)))
         (type (first format)))
    (if (and (eq (glarray-type gl-array) type)
             (<= (glarray-length gl-array) 
                 (gpuarray-length gpu-array)))
        (setf (gpuarray-buffer gpu-array)
              (buffer-sub-data buffer 
                               gl-array
                               (gpuarray-offset gpu-array)
			       (if (gpuarray-index-array 
				    gpu-array)
				   :element-array-buffer
				   :array-buffer)))
        (error "The gl-array must of the same type as the target gpu-array and not have a length exceeding that of the gpu-array."))
    gpu-array))


(defmacro with-gpu-array-as-gl-array ((temp-array-name
                                       gpu-array
                                       access) 
                                      &body body)
  (let ((glarray-pointer (gensym "POINTER"))
        (buffer-sym (gensym "BUFFER"))
	(target (gensym "target")))
    `(let ((,buffer-sym (gpuarray-buffer ,gpu-array))
	   (,target (if (gpuarray-index-array ,gpu-array)
			:element-array-buffer
			:array-buffer)))
       (bind-buffer ,buffer-sym ,target)
       (gl:with-mapped-buffer (,glarray-pointer 
                               ,target
                               ,access)
	 (let ((,temp-array-name 
		(make-glarray :pointer (cffi:inc-pointer 
					,glarray-pointer
					(gpuarray-offset ,gpu-array))
			      :type (gpuarray-type ,gpu-array)
			      :length (gpuarray-length ,gpu-array))))
	   ,@body)))))


;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

(defstruct gpu-stream 
  vao
  (start 0 :type unsigned-byte)
  (length 1 :type unsigned-byte)
  (draw-type :triangles :type symbol)
  (index-type nil))

(let ((vao-pool (make-hash-table)))
  (defun add-vao-to-pool (vao key)
    (setf (gethash key vao-pool) vao)
    vao)

  (defun free-all-vaos-in-pool ()
    (mapcar #'(lambda (x) (declare (ignore x)) 
		      (print "freeing a vao")) 
	    vao-pool)))

(defun make-gpu-stream-from-gpu-arrays 
    (&key gpu-arrays indicies-array
       (start 0) (length 1) (draw-type :triangles))
  (make-gpu-stream 
   :vao (make-vao-from-gpu-arrays gpu-arrays indicies-array)
   :start start
   :length length
   :draw-type draw-type
   :index-type (unless (null indicies-array)
		 (gpuarray-type indicies-array))))


;;;--------------------------------------------------------------
;;; HELPERS ;;;
;;;---------;;;

(defun free-managed-resources ()
  (free-all-vaos-in-pool)
  (free-all-buffers-in-pool))

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
  (gl:get-program program :active-attributes))

(defun program-attributes (program)
  "Returns a list of details of the attributes used by
   the program. Each element in the list is a list in the
   format: (attribute-name attribute-type attribute-size)"
  (loop for i from 0 below (program-attrib-count program)
     collect (multiple-value-bind (size type name)
                 (gl:get-active-attrib program i)
               (list name type size))))

(defun program-uniform-count (program)
  "Returns the number of uniforms used by the shader"
  (gl:get-program program :active-uniforms))

(defun program-uniforms (program-id)
  "Returns a list of details of the uniforms used by
   the program. Each element in the list is a list in the
   format: (uniform-name uniform-type uniform-size)"
  (loop for i from 0 below (program-uniform-count program-id)
     collect (multiple-value-bind (size type name)
                 (gl:get-active-uniform program-id i)
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
  (mapcar #'make-shader shader-paths))

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

;; [TODO] Need to sort gpustream indicies thing
(defun no-bind-draw-one (stream)
  "This draws the single stream provided using the currently 
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
   (let ((index-type (gpu-stream-index-type stream)))
    (bind-vao (gpu-stream-vao stream))
    (if index-type
	(%gl:draw-elements (gpu-stream-draw-type stream)
			   (gpu-stream-length stream)
			   (gl::cffi-type-to-gl index-type)
			   (cffi:make-pointer 0))
        (%gl:draw-arrays (gpu-stream-draw-type stream)
                         (gpu-stream-start stream)
                         (gpu-stream-length stream)))))



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
