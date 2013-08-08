(in-package :cgl)

;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(defstruct gpuarray 
  buffer
  format-index
  start
  dimensions
  index-array 
  (access-style :static-draw))

(defmethod print-object ((object gpuarray) stream)
  (format stream "#.<~a :element-type ~s :dimensions ~a :backed :BUFFER>"
          (if (gpuarray-index-array object)
              "GPU-INDEX-ARRAY"
              "GPU-ARRAY")
          (gpuarray-type object)
          (gpuarray-length object)))

;;---------------------------------------------------------------

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

;;---------------------------------------------------------------

(defgeneric make-gpu-array (initial-contents &key)
  (:documentation "This function creates a gpu-array which is very similar
   to a c-array except that the data is located in the memory 
   of the graphics card and so is accessible to shaders.
   You can either provide and type and length or you can 
   provide a c-array and the data from that will be used to 
   populate the gpu-array with.

   If this array is to be used as an index array then set the 
   :index-array key to t

   Access style is optional but if you are comfortable with 
   opengl, and know what type of usage pattern thsi array will
   have, you can set this to any of the following:
   (:stream-draw​ :stream-read​ :stream-copy​ :static-draw​ 
    :static-read​ :static-copy​ :dynamic-draw​ :dynamic-read
   ​ :dynamic-copy)"))

(defmethod make-gpu-array ((initial-contents null) 
                           &key element-type length (index-array nil)
                             (access-style :static-draw))
  (declare (ignore initial-contents))
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
                   :access-style access-style)))

;; [TODO] broken? I had left a note saying it was...but not how
(defmethod make-gpu-array ((initial-contents list) 
                           &key element-type (index-array nil)
                             (access-style :static-draw))
  (with-c-array (c-array element-type
                         (length initial-contents) 
                         initial-contents)
    (make-gpu-array c-array :index-array index-array :access-style access-style)))

(defmethod make-gpu-array ((initial-contents c-array) &key (index-array nil)
                             (access-style :static-draw))
  (let ((buffer (add-buffer-to-pool (gen-buffer))))
    (make-gpuarray :buffer (buffer-data buffer 
                                        initial-contents
                                        (if index-array
                                            :element-array-buffer
                                            :array-buffer)
                                        access-style)
                   :format-index 0
                   :length (array-length initial-contents)
                   :index-array index-array
                   :access-style access-style)))

(defun make-gpu-arrays (c-arrays &key index-array (access-style :static-draw))
  "This function creates a list of gpu-arrays residing in a
   single buffer in opengl. It create one gpu-array for each 
   c-array in the list passed in.

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
  (let ((buffer (add-buffer-to-pool
                 (multi-buffer-data (gen-buffer) c-arrays 
                                    (if index-array
                                        :element-array-buffer
                                        :array-buffer)
                                    access-style))))
    (loop for c-array in c-arrays
       for i from 0 collect 
         (make-gpuarray :buffer buffer
                        :format-index i
                        :length (glarray-length c-array)
                        :index-array index-array
                        :access-style access-style))))

(defgeneric gl-subseq (array start &optional end)
  (:documentation
   "This function returns a gpu-array or c-array which contains
   a subset of the array passed into this function.
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
   c-array newmonster. We can simply do the following:
   (gl-push (gpu-sub-array bigarray 1000 2000) newmonster)

   Obviously be aware that any changes you make to the parent
   array affect the child sub-array. This can really bite you
   in the backside if you change how the data in the array is 
   laid out."))

(defmethod gl-subseq ((array c-array) start &optional end)
  (let* ((length (array-length array))
         (type (array-type array))
         (end (or end length)))
    (if (and (< start end) (< start length) (<= end length))
        (make-c-array-from-pointer 
         (cffi:inc-pointer (pointer array) (foreign-type-index type start))
         (if (listp type)
             (if (eq :struct (first type))
                 (second type)
                 (error "we dont handle arrays of pointers yet"))
             type)
         (- end start)))
    (error "Invalid subseq start or end for c-array")))

(defmethod gl-subseq ((array gpuarray) start &optional end)
  (let* ((length (gpuarray-length array))
         (parent-start (gpuarray-start array))
         (new-start (+ parent-start (max 0 start)))
         (end (or end length)))
    (if (and (< start end) (< start length) (<= end length))
        (make-gpuarray 
         :buffer (gpuarray-buffer array)
         :format-index (gpuarray-format-index array)
         :start new-start
         :length (- end start)
         :index-array (gpuarray-index-array array)
         :access-style (gpuarray-access-style array))
        (error "Invalid subseq start or end for c-array"))))

(defun pull-c-arrays-from-buffer (buffer)
  (loop :for attr-format :in (glbuffer-format buffer)
     :collect 
     (progn 
       (bind-buffer buffer :array-buffer)
       (gl:with-mapped-buffer (b-pointer :array-buffer :read-only)
         
         (let* ((array-type (first attr-format))
                (c-array (make-c-array (if (listp array-type)
                                             (if (eq :struct (first array-type))
                                                 (second array-type)
                                                 (error "we dont handle arrays of pointers yet"))
                                             array-type)
                                        (second attr-format))))
           (%memcpy (pointer c-array) 
                    (cffi:inc-pointer b-pointer (third attr-format))
                    (c-array-byte-size c-array))
           c-array)))))

(defmacro with-gpu-array-as-c-array ((temp-array-name
                                       gpu-array
                                       access) 
                                      &body body)
  "This macro is really handy if you need to have random access
   to the data on the gpu. It takes a gpu-array and binds it
   to a c-array which allows you to run any of the c-array
   commands on it.

   A simple example would be if we wanted to set the 3rd element
   in a gpu array to 5.0 we could do the following:
   (with-gpu-array-as-c-array (tmp mygpuarray :write-only)
     (setf (aref-gl tmp 2) 5.0))

   The valid values for access are :read-only :write-only & 
   :read-write"
  (let ((glarray-pointer (gensym "POINTER"))
        (buffer-sym (gensym "BUFFER"))
        (target (gensym "target"))
        (ggpu-array (gensym "gpu-array")))
    `(let ((,buffer-sym (gpuarray-buffer ,gpu-array))
           (,target (if (gpuarray-index-array ,gpu-array)
                        :element-array-buffer
                        :array-buffer))
           (,ggpu-array ,gpu-array))
       (force-bind-buffer ,buffer-sym ,target)
       (gl:with-mapped-buffer (,glarray-pointer 
                               ,target
                               ,access)
         (if (pointer-eq ,glarray-pointer (null-pointer))
             (error "with-gpu-array-as-c-array: buffer mapped to null pointer~%Have you defintely got a opengl context?~%~s"
                    ,glarray-pointer)
             (let ((,temp-array-name 
                    (make-c-array-from-pointer 
                     (cffi:inc-pointer ,glarray-pointer (gpuarray-offset ,ggpu-array))
                     (let ((array-type (gpuarray-type ,ggpu-array)))
                       (if (listp array-type)
                           (if (eq :struct (first array-type))
                               (second array-type)
                               (error "we dont handle arrays of pointers yet"))
                           array-type))
                     (gpuarray-length ,ggpu-array))))
               ,@body))))))

(defun gpu-array-pull (gpu-array)
  "This function returns the contents of the array as lisp list 
   of the data. 
   Note that you often dont need to use this as the generic
   function gl-pull will call this function if given a gpu-array"
  (with-gpu-array-as-c-array (tmp gpu-array :read-only)
    (loop for i below (gpuarray-length gpu-array)
       collect (glpull-entry tmp i))))


(defun gpu-array-push (gpu-array c-array)
  "This function pushes the contents of the specified c-array
   into the gpu-array.
   Note that you often dont need to use this as the generic
   function gl-push will call this function if given a gpu-array"
  (let* ((buffer (gpuarray-buffer gpu-array))
         (format (nth (gpuarray-format-index gpu-array)
                      (glbuffer-format buffer)))
         (type (first format)))
    (if (and (eq (array-type c-array) type)
             (<= (array-length c-array) 
                 (gpuarray-length gpu-array)))
        (setf (gpuarray-buffer gpu-array)
              (buffer-sub-data buffer 
                               c-array
                               (gpuarray-offset gpu-array)
                               (if (gpuarray-index-array 
                                    gpu-array)
                                   :element-array-buffer
                                   :array-buffer)))
        (error "The c-array must of the same type as the target gpu-array and not have a length exceeding that of the gpu-array."))
    gpu-array))
