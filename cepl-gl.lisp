;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

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

(in-package :cepl-gl)

;;;--------------------------------------------------------------
;;; SHADER & PROGRAMS ;;;
;;;-------------------;;;

(let ((programs (make-hash-table)))
  (defun program-manager (name)
    (let ((prog-id (gethash name programs)))
      (if prog-id prog-id
	  (setf (gethash name programs) (gl:create-program)))))
  (defun program-manager-delete (name)
    (declare (ignore name))
    (print "delete not yet implemented")))

;; [TODO] We need to make this fast, this 'if not prog' won't do
(defmacro defprogram (name (&rest args) &body shaders)
  (let* ((uniform-names (mapcar #'first (varjo:extract-uniforms args))))
    `(let ((program nil))
       (defun ,name (stream ,@(when uniform-names `(&key ,@uniform-names)))
         (when (not program) 
	   (setf program (make-program ,name ,args ,shaders)))
         (funcall program stream ,@(loop for name in uniform-names 
					 :append `(,(utils:kwd name)
						   ,name)))))))

(defmacro defprogram? (name (&rest args) &body shaders)
  (declare (ignore name))
  `(let* ((shaders (varjo:rolling-translate ',args ',shaders)))
     (format t "~&~{~{~(#~a~)~%~a~}~^-----------~^~%~^~%~}~&" shaders)
     nil))

(defmacro glambda ((&rest args) &body shaders)
  `(make-program nil ,args ,shaders))

;; [TODO] Make glambda handle strings
(defmacro make-program (name args shaders)  
  (let* ((uniforms (varjo:extract-uniforms args))
         (uniform-names (mapcar #'first uniforms)))

    `(let* ((shaders (loop for (type code) in (varjo:rolling-translate 
                                               ',args ',shaders)
                        :collect (make-shader type code)))
            (program-id (link-shaders shaders 
				      ,(if name
					   `(program-manager ',name)
					   `(gl:create-program))))
            (assigners (create-uniform-assigners program-id ',uniforms))
            ,@(loop :for name :in uniform-names :for i :from 0
                 :collect `(,(utils:symb name '-assigner)
                             (nth ,i assigners))))
       (declare (ignorable assigners))
       (mapcar #'%gl:delete-shader shaders)
       (lambda (stream ,@(when uniforms `(&key ,@uniform-names)))
         (use-program program-id)
         ,@(loop :for uniform-name :in uniform-names
              :collect `(when ,uniform-name
                          (dolist (fun ,(utils:symb uniform-name
                                                    '-assigner))
                            (funcall fun ,uniform-name))))
         (no-bind-draw-one stream)))))

;; make this return list of funcs or nil for each uni-var
(defun create-uniform-assigners (program-id uniform-vars)
  (let* ((uniform-details (program-uniforms program-id))
         (active-uniform-details (process-uniform-details 
                                  uniform-details
                                  uniform-vars)))
    (loop for a-uniform in active-uniform-details
       :collect
         (when a-uniform
           (let ((location (gl:get-uniform-location program-id 
						    (second a-uniform))))
	     (if (< location 0)
		 (error "uniform ~a not found, this is a bug"
			(second a-uniform))
		 (loop for part in (subseq a-uniform 2)
		       :collect 
		       (destructuring-bind (offset type length) part
			 (let ((uni-fun (get-foreign-uniform-function type))
			       (lisp-uni-fun (get-uniform-function type)))
			   (if (eq length 1)
			       (lambda (pointer)
				 (funcall uni-fun location length
					  (cffi-sys:inc-pointer pointer offset)))
			       (lambda (pointer-or-val)
				 (funcall (if (cffi:pointerp pointer-or-val)
					      uni-fun
					      lisp-uni-fun)
					  location length
					  (cffi-sys:inc-pointer pointer-or-val
								offset)))))))))))))

;; [TODO] Got to be a quicker and tidier way
(defun process-uniform-details (uniform-details uniform-vars)
  ;; returns '(byte-offset principle-type length)
  (let ((result nil)
        (paths (mapcar #'parse-uniform-path uniform-details)))
    (loop for detail in uniform-details
       for path in paths
       :do (setf result 
                 (acons (caar path) 
                        (cons (first detail)
                              (cons (list (get-path-offset 
                                           path
                                           uniform-vars)
                                          (second detail)
                                          (third detail))
                                    (rest (rest 
                                           (assoc (caar path)
                                                  result)))))
                        result)))
    (loop for var in uniform-vars
       :collect (assoc (first var) result))))

;; [TODO] If we load shaders from files the names will clash
(defun parse-uniform-path (uniform-detail)
  (labels ((s-dot (x) (split-sequence:split-sequence #\. x))
           (s-square (x) (split-sequence:split-sequence #\[ x)))
    (loop for path in (s-dot (first uniform-detail))
       :collect (let ((part (s-square (remove #\] path))))
                  (list (intern (string-upcase 
                                 (first part)))
                        (if (second part)
                            (parse-integer (second part))
                            0))))))

(defun get-slot-type (parent-type slot-name)
  (second (assoc slot-name (varjo:struct-definition parent-type))))

(defun get-path-offset (path uniform-vars)
  (labels ((path-offset (type path &optional (sum 0))
             (if path
                 (let* ((path-part (first path))
                        (slot-name (first path-part))
                        (child-type (varjo:type-principle
                                     (get-slot-type type
                                                    slot-name))))
                   (path-offset 
                    child-type
                    (rest path)
                    (+ sum
                       (+ (cffi:foreign-slot-offset type slot-name) 
                          (* (cffi:foreign-type-size child-type)
                             (second path-part))))))
                 sum)))
    (let* ((first-part (first path))
           (type (second (assoc (first first-part) uniform-vars)))
           (index (second first-part)))
      (+ (* (cffi:foreign-type-size type) index)
         (path-offset type (rest path))))))

;;;--------------------------------------------------------------
;;; BUFFERS ;;;
;;;---------;;;

(defstruct glbuffer
  "This is our opengl buffer object. Along with the opengl
   buffer name (buffer-id) we also store the layout of the data
   within the buffer. 
   This layout is as follows:
   `((data-type data-index-length offset-in-bytes-into-buffer)
   for example:
   `((:float 10 0) ('vert-data 50 40))"
  (buffer-id (car (gl:gen-buffers 1)))
  (format nil))


(base-macros:defmemo bind-buffer (buffer buffer-target)
  (gl:bind-buffer buffer-target (glbuffer-buffer-id buffer)))

(setf (documentation 'bind-buffer 'function) 
      "Binds the specified opengl buffer to the target")

(defun gen-buffer (&key initial-contents 
                     (buffer-target :array-buffer) 
                     (usage :static-draw))
  (declare (symbol buffer-target usage))
  "Creates a new opengl buffer object. 
   Optionally you can provide a gl-array as the :initial-contents
   to have the buffer populated with the contents of the array"
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
   of the array. You also pass in the buffer type and the 
   draw type this buffer is to be used for.
   
   The function returns a buffer object with its format slot
   populated with the details of the data stored within the buffer"
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


(defun buffer-sub-data (buffer gl-array byte-offset buffer-target
                        &key (safe t))
  (declare (glarray gl-array))
  "This function replaces a subsection of the data in the 
   specified buffer with the data in the gl-array.
   The byte offset specified where you wish to start overwriting 
   data from. 
   When the :safe option is t, the function checks to see if the 
   data you are about to write into the buffer will cross the 
   boundaries between data already in the buffer and will emit 
   an error if you are."
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

(defun buffer-reserve-raw-block (buffer size-in-bytes buffer-target 
                                 usage)
  "This function creates an empty block of data in the opengl buffer.
   It will remove ALL data currently in the buffer. It also will not
   update the format of the buffer so you must be sure to handle this
   yourself. It is much safer to use this as an assistant function to
   one which takes care of these issues"
  (bind-buffer buffer buffer-target)
  (%gl:buffer-data buffer-target size-in-bytes
                   (cffi:null-pointer) usage)
  buffer)

(defun buffer-reserve-block (buffer type length buffer-target usage)
  "This function creates an empty block of data in the opengl buffer
   equal in size to (* length size-in-bytes-of-type).
   It will remove ALL data currently in the buffer"
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
  "This function creates an empty block of data in the opengl buffer
   equal in size to the sum of all of the 
   (* length size-in-bytes-of-type) in types-and-lengths.
   types-and-lengths should be of the format:
   `((type length) (type length) ...etc)
   It will remove ALL data currently in the buffer"
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

;;;--------------------------------------------------------------
;;; VAOS ;;;
;;;------;;;


(base-macros:defmemo bind-vao (vao)
  (gl:bind-vertex-array vao))

(setf (documentation 'bind-vao 'function) 
      "Binds the vao specfied")

(setf (symbol-function 'bind-vertex-array) #'bind-vao)

(defgeneric make-vao2 (inputs &key element-buffer))

;; glVertexAttribPointer
;; ---------------------
;; GL_BYTE
;; GL_UNSIGNED_BYTE
;; GL_SHORT
;; GL_UNSIGNED_SHORT
;; GL_INT
;; GL_UNSIGNED_INT 
;; GL_HALF_FLOAT
;; GL_FLOAT
;; GL_DOUBLE
;; GL_FIXED
;; GL_INT_2_10_10_10_REV
;; GL_UNSIGNED_INT_2_10_10_10_REV

;; glVertexAttribLPointer 
;; ----------------------
;; GL_DOUBLE 

;; buffer format is a list whose sublists are of the format
;; type, length, byte-offset-from-start-of-buffer

(defun make-vao (buffer/s/formats &key element-buffer)
  "Make a vao from any of the following:
    - a single buffer object
    - a list of buffer objects
    - a list of formats in the format expected by the function
      make-vao-from-formats
   You can also specify an element buffer to be used in the vao"
  (labels ((format-from-buffer (buffer)
             (cons buffer 
                   (loop for attrf in (glbuffer-format buffer)
                      append (rest 
                              (gl-type-format (first attrf) 
                                              (third attrf)))))))
    (make-vao-from-formats
     (loop for item in (if (glbuffer-p buffer/s/formats)
                           (list
                            (format-from-buffer buffer/s/formats))
                           buffer/s/formats)
        :collect 
          (cond ((listp item) item)
                ((glbuffer-p item) (format-from-buffer item))))
     :element-buffer element-buffer)))

;; For element-array-buffer the indices can be unsigned bytes, 
;; unsigned shorts, or unsigned ints. 

(defun make-vao-from-formats (formats &key element-buffer)
  "Makes a vao from a list of buffer formats.
   The formats list should be laid out as follows:
   `((buffer1 (attr-format1) (attr-format2))
     (buffer2 (attr-format3) (attr-format4)))
   with each attr-format laid out as follows:
   `(component-type normalized-flag stride pointer)
   if you have the type and offset of the data this can be generated
   by using the function gl-type-format.
   You can also specify an element buffer to be used in the vao"
  (let ((vao (gl:gen-vertex-array))
        (attr-num 0))
    (bind-vao vao)
    (loop for format in formats
       :do (let ((buffer (first format)))
             (bind-buffer buffer :array-buffer)
             (loop :for (type normalized stride pointer) 
                :in (rest format)
                :do (setf attr-num
                          (+ attr-num
                             (gl-assign-attrib-pointers
                              type pointer stride))))))
    (when element-buffer
      (bind-buffer element-buffer :element-array-buffer))
    (bind-vao 0)
    vao))


;; buffer format is a list whose sublists are of the format
;; type, length, byte-offset-from-start-of-buffer

(defun make-vao-from-gpu-arrays
    (gpu-arrays &optional indicies-array)
  "makes a vao using a list of gpu-arrays as the source data
   (remember that you can also use gpu-sub-array here if you
   need a subsection of a gpu-array).
   You can also specify an indicies-array which will be used as
   the indicies when rendering"
  (let ((element-buffer (when indicies-array
                          (gpuarray-buffer indicies-array)))
        (vao (gl:gen-vertex-array))
        (attr 0))
    (bind-vao vao)
    (loop for gpu-array in gpu-arrays
       :do (let* ((buffer (gpuarray-buffer gpu-array))
                  (format (nth (gpuarray-format-index gpu-array)
                               (glbuffer-format buffer))))
             (setf attr (+ attr (gl-assign-attrib-pointers
                                 (first format) 
                                 attr
                                 (third format))))))
    (when element-buffer 
      (bind-buffer element-buffer :element-array-buffer))
    (bind-vao 0)
    vao))


;;;--------------------------------------------------------------
;;; GLARRAYS ;;;
;;;----------;;;

;; the struct containing details about our gl-arrays
(defstruct glarray
  (pointer (null-pointer))
  (length 0 :type unsigned-byte)
  (type nil :type symbol))

(defmethod print-object ((object glarray) stream)
  (format stream
          "#.<GL-ARRAY :type ~a :length ~a>"
          (glarray-type object)
          (glarray-length object)))

(defun glarray-byte-size (gl-array)
  "This returns the size in bytes of the gl-array"
  (declare (glarray gl-array))
  (* (glarray-length gl-array) 
     (cffi:foreign-type-size (glarray-type gl-array))))

;; check for glstruct type existance
(defun make-gl-array (element-type &key length initial-contents)
  "Create a new gl-array of the specified element-type. 
   You must also either specify the :length or the 
   :initial-contents of the array."
  (if initial-contents
      (destructuring-allocate element-type initial-contents)
      (make-glarray :pointer (foreign-alloc element-type 
					    :count length)
		    :length length 
		    :type element-type)))

(defun free-gl-array (gl-array)
  "Frees the specified gl-array."
  (foreign-free (glarray-pointer gl-array)))

(declaim (inline aref-gl))
(defun aref-gl (gl-array index)
  "Returns the INDEX-th component of gl-array."
  (mem-aref (glarray-pointer gl-array) 
            (glarray-type gl-array) 
            index))

(declaim (inline (setf aref-gl)))
(defun (setf aref-gl) (value array index)
  "Sets the INDEX-th component of gl-array. to value"
  (setf (mem-aref (glarray-pointer array)
                  (glarray-type array)
                  index) value))

(defun destructuring-populate (gl-array data)
  "This function takes a gl-array and a list of data and 
   populates the gl-array using the data. 
   The data must be a list of sublists. Each sublist must
   contain the data for the attributes of the gl-array's type.  
   That sucks as an explanation so here is an example:

   given a format as defined below:
    (cgl:define-interleaved-attribute-format vert-data 
      (:type :float :components (x y z))
      (:type :float :components (r g b a)))

   and an array made using this format
    (setf *vertex-data-gl* (cgl:make-gl-array 'vert-data :length 3))

   then you can populate it as so:
    (cgl:destructuring-populate *vertex-data-gl*
     	   '((#( 0.0     0.5  0.0)
		      #( 1.0     0.0  0.0  1.0))

			 (#( 0.5  -0.366  0.0)
			  #( 0.0     1.0  0.0  1.0))

			 (#(-0.5  -0.366  0.0)
			  #( 0.0     0.0  1.0  1.0))))

   Hopefully that makes sense."
  (dpopulate (glarray-type gl-array) gl-array data)
  gl-array)

(defun destructuring-allocate (array-type data)
  "This function will create a new gl-array with a length
   equal to the length of the data provided, and then populate 
   the gl-array.

   The data must be a list of sublists. Each sublist must
   contain the data for the attributes of the gl-array's type.  
   That sucks as an explanation so here is an example:

   given a format as defined below:
    (cgl:define-interleaved-attribute-format vert-data 
      (:type :float :components (x y z))
      (:type :float :components (r g b a)))

   and an array made using this format
    (setf *vertex-data-gl* (cgl:make-gl-array 'vert-data :length 3))

   then you can populate it as so:
    (cgl:destructuring-populate *vertex-data-gl*
     	   '((#( 0.0     0.5  0.0)
		      #( 1.0     0.0  0.0  1.0))

			 (#( 0.5  -0.366  0.0)
			  #( 0.0     1.0  0.0  1.0))

			 (#(-0.5  -0.366  0.0)
			  #( 0.0     0.0  1.0  1.0))))

   Hopefully that makes sense."
  (let ((array (make-gl-array array-type :length (length data))))
    (destructuring-populate array data)
    array))


;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

;; [TODO] IMplement buffer freeing properly
(let ((buffer-pool ()))
  (defun add-buffer-to-pool (buffer)
    (setf buffer-pool (cons buffer buffer-pool))
    buffer)

  (defun free-all-buffers-in-pool ()
    (mapcar #'(lambda (x) (declare (ignore x))
                      (print "freeing a buffer")) 
            buffer-pool)))

(defstruct gpuarray 
  buffer
  format-index
  (start 0)
  length
  index-array 
  (access-style :static-draw))

(defmethod print-object ((object gpuarray) stream)
  (format stream 
          "#.<~a :type ~a :length ~a>"
          (if (gpuarray-index-array object)
              "GPU-INDEX-ARRAY"
              "GPU-ARRAY")
          (gpuarray-type object)
          (gpuarray-length object)))

(defun gpuarray-format (gpu-array)
  "Returns a list containing the element-type, the length of the
   array and the offset in bytes from the beginning of the buffer
   this gpu-array lives in."
  (nth (gpuarray-format-index gpu-array)
       (glbuffer-format (gpuarray-buffer gpu-array))))

(defun gpuarray-type (gpu-array)
  "Returns the type of the gpuarray"
  (first (gpuarray-format gpu-array)))

(defun gpuarray-offset (gpu-array)
  "Returns the offset in bytes from the beggining of the buffer
   that this gpuarray is stored at"
  (let ((format (gpuarray-format gpu-array)))
    (+ (third format)
       (foreign-type-index (first format)
                           (gpuarray-start gpu-array)))))


(defun pull-gl-arrays-from-buffer (buffer)
  (loop for attr-format in (glbuffer-format (gpuarray-buffer
                                             buffer))
     collect 
       (progn 
         (bind-buffer buffer :array-buffer)
         (gl:with-mapped-buffer (pointer :array-buffer 
                                         :read-only)
           (let ((gl-array (make-glarray 
                            :pointer (cffi:inc-pointer
                                      pointer (third 
                                               attr-format))
                            :type (first attr-format)
                            :length (second attr-format))))
             (gl-pull gl-array))))))

(defgeneric make-gpu-array (initial-contents &key)
  (:documentation "This function creates a gpu-array which is very similar
   to a gl-array except that it is located in the memory of the
   graphics card and so is accesable to shaders.
   You can either provide and type and length or you can 
   provide a gl-array and the data from that will be used to 
   populate the gpu-array with.

   If this array is to be used as an index array then set the 
   :index-array key to t

   Access style is optional but if you are comfortable with 
   opengl, and know what type of usage pattern thsi array will
   have, you can set this to any of the following:
   (:stream-draw​ :stream-read​ :stream-copy​ :static-draw​ 
    :static-read​ :static-copy​ :dynamic-draw​ :dynamic-read
   ​ :dynamic-copy)

   Finally you can provide an existing buffer if you want to
   append the new array into that buffer. This is VERY slow
   compared to other ways of creating arrays and should only
   really be used in non-production code or when just playing 
   around in the REPL"))

(defmethod make-gpu-array 
    ((initial-contents (eql nil)) 
     &key
       element-type 
       length
       (index-array nil)
       (access-style :static-draw)
       (location nil))
  (declare (ignore initial-contents))
  (if location
      (last (make-gpu-arrays
             (append (pull-gl-arrays-from-buffer location)
                     (make-gl-array element-type
                                    :length length))
             :index-array index-array
             :access-style access-style
             :location location))
      (let ((buffer (add-buffer-to-pool (gen-buffer))))
        (make-gpuarray :buffer (buffer-reserve-block buffer 
                                                     element-type
                                                     length
                                                     (if index-array
                                                         :element-array-buffer
                                                         :array-buffer)
                                                     access-style)
                       :format-index 0
                       :length length
                       :index-array index-array
                       :access-style access-style))))


(defmethod make-gpu-array 
    ((initial-contents list) 
     &key
       element-type
       (index-array nil)
       (access-style :static-draw)
       (location nil))
  (cffi:with-foreign-object (ptr element-type (length initial-contents))
    (make-gpu-array (destructuring-populate
		     (make-glarray :pointer ptr 
				   :length (length initial-contents) 
				   :type element-type) 
		     initial-contents)
                    :index-array index-array
                    :access-style access-style
                    :location location)))

(defmethod make-gpu-array ((initial-contents glarray) 
                           &key
                             (index-array nil)
                             (access-style :static-draw)
                             (location nil))
  (if location
      (last (make-gpu-arrays
             (append (pull-gl-arrays-from-buffer location)
                     initial-contents)
             :index-array index-array
             :access-style access-style
             :location location))
      (let ((buffer (add-buffer-to-pool (gen-buffer))))
        (make-gpuarray :buffer (buffer-data buffer 
                                            initial-contents
                                            (if index-array
                                                :element-array-buffer
                                                :array-buffer)
                                            access-style)
                       :format-index 0
                       :length (glarray-length initial-contents)
                       :index-array index-array
                       :access-style access-style))))



(defun make-gpu-arrays (gl-arrays &key index-array
                                    (access-style :static-draw)
                                    (location nil))
  "This function creates a list of gpu-arrays residing in a
   single buffer in opengl. It create one gpu-array for each 
   gl-array in the list passed in.

   If these arrays are to be used as an index arrays then set the
   :index-array key to t

   Access style is optional but if you are comfortable with 
   opengl, and know what type of usage pattern thsi array will
   have, you can set this to any of the following:
   (:stream-draw​ :stream-read​ :stream-copy​ :static-draw​ 
    :static-read​ :static-copy​ :dynamic-draw​ :dynamic-read
   ​ :dynamic-copy)

   Finally you can provide an existing buffer if you want to
   use it rather than creating a new buffer. Note that all 
   existing data in the buffer will be destroyed in the process"
  (let ((buffer (or location
                    (add-buffer-to-pool
                     (multi-buffer-data (gen-buffer) 
                                        gl-arrays 
                                        (if index-array
                                            :element-array-buffer
                                            :array-buffer)
                                        access-style)))))
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
    "This function returns a gpu-array which contains a subset
     of the gpu-array passed into this function.
     Right this will make more sense with a use case:

     Imagine we have one gpu-array with the vertex data for 10
     different monsters inside it and each monster is made of 100
     vertices. The first mosters vertex data will be in the 
     sub-array (gpu-sub-array bigarray 0 1000) and the vertex 
     data for the second monster would be at 
     (gpu-sub-array bigarray 1000 2000)

     This *view* (for lack of a better term) into our array can
     be really damn handy. Prehaps, for example, we want to 
     replace the vertex data of monster 2 with the data in my
     gl-array newmonster. We can simply do the following:
     (gl-push (gpu-sub-array bigarray 1000 2000) newmonster)

     Obviously be aware that any changes you make to the parent
     array affect the child sub-array. This can really bite you
     in the backside if you change how the data in the array is 
     laid out."
    (when (< end start) 
      (error "end cannot be less than start"))
    (make-gpuarray 
     :buffer (gpuarray-buffer gpu-array)
     :format-index (gpuarray-format-index gpu-array)
     :start new-start
     :length (- (min end parent-end) start)
     :index-array (gpuarray-index-array gpu-array)
     :access-style (gpuarray-access-style gpu-array))))

(defmacro with-gpu-array-as-gl-array ((temp-array-name
                                       gpu-array
                                       access) 
                                      &body body)
  "This macro is really handy if you need to have random access
   to the data on the gpu. It takes a gpu-array and binds it
   to a gl-array which allows you to run any of the gl-array
   commands on it.

   A simple example would be if we wanted to set the 3rd element
   in a gpu array to 5.0 we could do the following:
   (with-gpu-array-as-gl-array (tmp mygpuarray :write-only)
     (setf (aref-gl tmp 2) 5.0))

   The valid values for access are :read-only :write-only & 
   :read-write"
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
                (make-glarray 
                 :pointer (cffi:inc-pointer 
                           ,glarray-pointer
                           (gpuarray-offset ,gpu-array))
                 :type (gpuarray-type ,gpu-array)
                 :length (gpuarray-length ,gpu-array))))
           ,@body)))))

(defun gpu-array-pull (gpu-array)
  "This function returns the contents of the array as lisp list 
   of the data. 
   Note that you often dont need to use this as the generic
   function gl-pull will call this function if given a gpu-array"
  (let ((type (gpuarray-type gpu-array)))
    (with-gpu-array-as-gl-array (tmp gpu-array :read-only)
      (loop for i below (gpuarray-length gpu-array)
         collect (glpull-entry type tmp i)))))


(defun gpu-array-push (gpu-array gl-array)
  "This function pushes the contents of the specified gl-array
   into the gpu-array.
   Note that you often dont need to use this as the generic
   function gl-push will call this function if given a gpu-array"
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


;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

(defstruct gpu-stream 
  "gpu-streams are the structure we use in cepl to pass 
   information to our programs on what to draw and how to draw 
   it.

   It basically adds the only things that arent captured in the
   vao but are needed to draw, namely the range of data to draw
   and the style of drawing.

   If you are using gl-arrays then be sure to use the 
   make-gpu-stream-from-gpu-arrays function as it does all the
   work for you."
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
       (start 0) length (draw-type :triangles))
  "This function simplifies making the gpu-stream if you are 
   storing the data in gpu-arrays.

   Remember that you can also use gpu-sub-arrays in here if you
   want to limit the data you are using, for example the 
   following is perfectly legal code:
   (make-gpu-stream-from-gpu-arrays 
     :gpu-arrays `(,(gpu-sub-array monster-pos-data 1000 2000)
                  ,(gpu-sub-array monster-col-data 1000 2000))
     :indicies-array monster-indicies-array
     :length 1000)"
  (let* ((gpu-arrays (if (gpuarray-p gpu-arrays)
			 (list gpu-arrays)
			 gpu-arrays))
	 (length (or length (apply #'min (mapcar #'gpuarray-length 
						 gpu-arrays)))))
    
    (make-gpu-stream 
     :vao (make-vao-from-gpu-arrays gpu-arrays indicies-array)
     :start start
     :length (if indicies-array
		 (min (gpuarray-length indicies-array) length)
		 length)
     :draw-type draw-type
     :index-type (unless (null indicies-array)
                   (gpuarray-type indicies-array)))))


;;;--------------------------------------------------------------
;;; PUSH AND PULL ;;;
;;;---------------;;;

(defgeneric gl-pull (gl-object)
  (:documentation "Pulls data from the gl-array or gpu-array back into a native lisp list"))

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

;; [TODO] HANDLE DOUBLES
(defun get-foreign-uniform-function (type)
  (case type
    ((:int :int-arb :bool :bool-arb :sampler_1d :sampler_1d_shadow 
           :sampler_2d :sampler_3d :sampler_cube 
           :sampler_2d_shadow) #'%gl:uniform-1iv)
    ((:float :float-arb) #'%gl:uniform-1fv)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) #'%gl:uniform-2iv)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) #'%gl:uniform-3iv)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) #'%gl:uniform-4iv)
    ((:float-vec2 :float-vec2-arb) #'%gl:uniform-2fv)
    ((:float-vec3 :float-vec3-arb) #'%gl:uniform-3fv)
    ((:float-vec4 :float-vec4-arb) #'%gl:uniform-4fv)
    ((:float-mat2 :float-mat2-arb) #'%gl:uniform-matrix-2fv)
    ((:float-mat3 :float-mat3-arb) #'%gl:uniform-matrix-3fv)
    ((:float-mat4 :float-mat4-arb) #'%gl:uniform-matrix-4fv)
    (t (error "Sorry cepl doesnt handle that type yet"))))

(defun get-uniform-function (type)
  (case type
    ((:int :int-arb :bool :bool-arb :sampler_1d :sampler_1d_shadow 
           :sampler_2d :sampler_3d :sampler_cube 
           :sampler_2d_shadow) #'uniform-1i)
    ((:float :float-arb) #'uniform-1f)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) #'uniform-2i)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) #'uniform-3i)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) #'uniform-4i)
    ((:float-vec2 :float-vec2-arb) #'uniform-2f)
    ((:float-vec3 :float-vec3-arb) #'uniform-3f)
    ((:float-vec4 :float-vec4-arb) #'uniform-4f)
    ((:float-mat2 :float-mat2-arb) #'uniform-matrix2)
    ((:float-mat3 :float-mat3-arb) #'uniform-matrix3)
    ((:float-mat4 :float-mat4-arb) #'uniform-matrix4)
    (t (error "Sorry cepl doesnt handle that type yet: ~a" type))))

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

(defun make-shader 
    (shader-type source-string &optional (shader-id (gl:create-shader 
                                                     shader-type)))
  "This makes a new opengl shader object by compiling the text
   in the specified file and, unless specified, establishing the
   shader type from the file extension"
  (gl:shader-source shader-id source-string)
  (gl:compile-shader shader-id)
  ;;check for compile errors
  (when (not (gl:get-shader shader-id :compile-status))
    (error "Error compiling ~(~a~): ~%~a~%~%~a" 
           shader-type
           (gl:get-shader-info-log shader-id)
           source-string))
  shader-id)

(defun load-shader (file-path 
                    &optional (shader-type 
                               (shader-type-from-path file-path)))
  (restart-case
      (make-shader (utils:file-to-string file-path) shader-type)
    (reload-recompile-shader () (load-shader file-path
                                             shader-type))))

(defun load-shaders (&rest shader-paths)
  (mapcar #'load-shader shader-paths))

(defun link-shaders (shaders &optional program_id)
  "Links all the shaders provided and returns an opengl program
   object. Will recompile an existing program if ID is provided"
  (let ((program (or program_id (gl:create-program))))
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

(defun lispify-name (name)
  "take a string and changes it to uppercase and replaces
   all underscores _ with minus symbols -"
  (string-upcase (substitute #\- #\_ name)))


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

(defun draw-stream (program stream &rest uniforms)
  "This is a really syntactic sugar around funcall'ing a program.
   It takes a program, a stream and &key arguments where
   the key is the name of the uniform and the value is the value
   you wish to set the uniform to."
  (apply program (cons (list stream) uniforms)))
