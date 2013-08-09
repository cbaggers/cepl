(in-package :cgl)

;; [TODO] alignment, what the hell do we do with that? I'm a bit 
;;        drunk to make these choices right now

;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(defstruct gpuarray 
  buffer
  format-index
  (start '(0))
  dimensions
  (access-style :static-draw))

(defmethod print-object ((object gpuarray) stream)
  (format stream "#.<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by :BUFFER>"
          (element-type object)
          (gpuarray-dimensions object)))

;;---------------------------------------------------------------

(defun gpuarray-format (gpu-array)
  "Returns a list containing the element-type, the length of the
   array and the offset in bytes from the beginning of the buffer
   this gpu-array lives in."
  (nth (gpuarray-format-index gpu-array)
       (glbuffer-format (gpuarray-buffer gpu-array))))

(defmethod dimensions ((object gpuarray))
  (gpuarray-dimensions object))

(defmethod element-type ((object gpuarray))
  (first (gpuarray-format object)))

;; [TODO] This looks wrong, the beggining right? NO!
;;        remember that the gpu-array could be a sub-array
;;        in that case the
(defun gpuarray-offset (gpu-array)
  "Returns the offset in bytes from the beggining of the buffer
   that this gpuarray is stored at"
  (let ((format (gpuarray-format gpu-array)))
   (+ (third format) (gl-calc-byte-size (first format) 
                                        (gpuarray-start gpu-array)))))

;;---------------------------------------------------------------

(defgeneric make-gpu-array (initial-contents &key)
  (:documentation "This function creates a gpu-array which is very similar
   to a c-array except that the data is located in the memory 
   of the graphics card and so is accessible to shaders.
   You can either provide and type and length or you can 
   provide a c-array and the data from that will be used to 
   populate the gpu-array with.

   Access style is optional but if you are comfortable with 
   opengl, and know what type of usage pattern thsi array will
   have, you can set this to any of the following:
   (:stream-draw​ :stream-read​ :stream-copy​ :static-draw​ 
    :static-read​ :static-copy​ :dynamic-draw​ :dynamic-read
   ​ :dynamic-copy)"))

;; c-array (dimensions element-type &key initial-contents displaced-by (alignment 1))
;; old-gpu (initial-contents &key element-type length access-style)
;; ??????? (initial-contents &key element-type dimensions access-style)
;; [TODO] Should element-array-buffer to part of the gpu array? is it not better
;;        to complain if someone tries to use one as a element-array-buffer and
;;        it is of invalid type? Buffers are buffers, vaos should be handling 
;;        their usage in terms of state
;; [TODO] Check to see we have all the data we need
;; [TODO] all make-gpu-array need the start argument specified
;; [TODO] all dimensions need checking for sanity..some clearly dont have any :D
(defun valid-as-element-arrayp (gl-object)
  (declare (ignore gl-object))
  (error "IMPLEMENT ME! GIVE ME PURPOSE! PLEASE.... ARGHHHHHHHHHHHHHHH"))

(defmethod make-gpu-array ((initial-contents null) 
                           &key element-type dimensions
                             (access-style :static-draw))
  (declare (ignore initial-contents))
  (let ((buffer (add-buffer-to-pool (gen-buffer))))
    (make-gpuarray :buffer (buffer-reserve-block
                            buffer element-type dimensions
                            :array-buffer access-style)
                   :format-index 0
                   :dimensions dimensions
                   :access-style access-style)))

;; [TODO] broken? I had left a note saying it was...but not how
(defmethod make-gpu-array ((initial-contents list) 
                           &key element-type (access-style :static-draw) 
                             (alignment 1))
  (with-c-array (c-array (length initial-contents) element-type 
                         :initial-contents initial-contents
                         :alignment alignment)
    (make-gpu-array c-array :access-style access-style)))

(defmethod make-gpu-array ((initial-contents c-array) 
                           &key (access-style :static-draw))
  (let ((buffer (add-buffer-to-pool (gen-buffer))))
    (make-gpuarray :buffer (buffer-data buffer initial-contents
                                        :array-buffer access-style)
                   :format-index 0
                   :dimensions (dimensions initial-contents)
                   :access-style access-style)))

(defun make-gpu-arrays (c-arrays &key (access-style :static-draw))
  "This function creates a list of gpu-arrays residing in a
   single buffer in opengl. It create one gpu-array for each 
   c-array in the list passed in.

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
                                    :array-buffer access-style))))
    (loop :for c-array :in c-arrays :for i :from 0 :collecting 
       (make-gpuarray :buffer buffer
                      :format-index i
                      :dimensions (dimensions c-array)
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

;; [TODO] This needs to accept 1D arrays only
;;        The generic case of this should be available to c-arrays 
;;        as well I think...otherwise the name is a bit odd..maybe
;;        not now it is c-array rather than gl-array...hmmm.
;; [TODO] Move this definition to c-arrays
(defmethod gl-subseq ((array c-array) start &optional end)
  (let* ((length (dimensions array))
         (type (element-type array))
         (end (or end length)))
    (if (and (< start end) (< start length) (<= end length))
        (make-c-array-from-pointer 
         (cffi:inc-pointer (pointer array) (gl-calc-byte-size type start))
         (if (listp type) 
             (if (eq :struct (first type))
                 (second type)
                 (error "we dont handle arrays of pointers yet"))
             type)
         (- end start)))
    (error "Invalid subseq start or end for c-array")))

(defmethod gl-subseq ((array gpuarray) start &optional end)
  (let* ((length (gpuarray-dimensions array))
         (parent-start (gpuarray-start array))
         (new-start (+ parent-start (max 0 start)))
         (end (or end length)))
    (if (and (< start end) (< start length) (<= end length))
        (make-gpuarray 
         :buffer (gpuarray-buffer array)
         :format-index (gpuarray-format-index array)
         :start new-start
         :dimensions (- end start)
         :access-style (gpuarray-access-style array))
        (error "Invalid subseq start or end for c-array"))))

(defun pull-c-arrays-from-buffer (buffer)
  (loop :for attr-format :in (glbuffer-format buffer)
     :collect 
     (progn 
       (bind-buffer buffer :array-buffer)
       (gl:with-mapped-buffer (b-pointer :array-buffer :read-only)
         
         (let* ((element-type (first attr-format))
                (c-array (make-c-array (if (listp element-type)
                                             (if (eq :struct (first element-type))
                                                 (second element-type)
                                                 (error "we dont handle arrays of pointers yet"))
                                             element-type)
                                        (second attr-format))))
           (cffi::%memcpy (pointer c-array) 
                    (cffi:inc-pointer b-pointer (third attr-format))
                    (c-array-byte-size c-array))
           c-array)))))

;; [TODO] Dont require a temporary name, just use the one it has
;;        this makes it feel more magical to me and also it is 
;;        in-line with things like with-slots
;; [TODO] Need to unmap if something goes wrong
(defmacro with-gpu-array-as-c-array ((gpu-array 
                                      &key (access-type :read-write) 
                                      temp-name) 
                                     &body body)
  "This macro is really handy if you need to have random access
   to the data on the gpu. It takes a gpu-array and maps it
   as a c-array which allows you to run any of the c-array
   commands on it.

   A simple example would be if we wanted to set the 3rd element
   in a gpu array to 5.0 we could do the following:
   (with-gpu-array-as-c-array (mygpuarray)
     (setf (aref-gl mygpuarray 2) 5.0))

   The valid values for access are :read-only :write-only & 
   :read-write"
  (unless (find access-type '(:read-write :read-only :write-only))
    (error "The access argument must be set to :read-write :read-only or :write-only"))
  (when (and (not temp-name) (not (symbolp gpu-array)))
    (error "The gpu array argument must be a symbol naming a gpu-array unless you specify a temp-name"))
  (let ((glarray-pointer (gensym "POINTER"))
        (buffer-sym (gensym "BUFFER"))
        (target (gensym "target"))
        (ggpu-array (gensym "gpu-array")))
    `(let ((,buffer-sym (gpuarray-buffer ,gpu-array))
           (,target :array-buffer)
           (,ggpu-array ,gpu-array))
       (force-bind-buffer ,buffer-sym ,target)
       (gl:with-mapped-buffer (,glarray-pointer 
                               ,target
                               ,access-type)
         (if (pointer-eq ,glarray-pointer (null-pointer))
             (error "with-gpu-array-as-c-array: buffer mapped to null pointer~%Have you defintely got an opengl context?~%~s"
                    ,glarray-pointer)
             (let ((,(or temp-name gpu-array)
                    (make-c-array-from-pointer
                     (gpuarray-dimensions ,ggpu-array)
                     (element-type ,ggpu-array)
                     (cffi:inc-pointer ,glarray-pointer (gpuarray-offset ,ggpu-array)))))
               ,@body))))))

;; (defun gpu-array-pull (gpu-array)
;;   "This function returns the contents of the array as lisp list 
;;    of the data. 
;;    Note that you often dont need to use this as the generic
;;    function gl-pull will call this function if given a gpu-array"
;;   (with-gpu-array-as-c-array (tmp gpu-array :read-only)
;;     (loop for i below (gpuarray-dimensions gpu-array)
;;        collect (glpull-entry tmp i))))


;; (defun gpu-array-push (gpu-array c-array)
;;   "This function pushes the contents of the specified c-array
;;    into the gpu-array.
;;    Note that you often dont need to use this as the generic
;;    function gl-push will call this function if given a gpu-array"
;;   (let* ((buffer (gpuarray-buffer gpu-array))
;;          (format (nth (gpuarray-format-index gpu-array)
;;                       (glbuffer-format buffer)))
;;          (type (first format)))
;;     (if (and (eq (element-type c-array) type)
;;              (<= (dimensions c-array) 
;;                  (gpuarray-dimensions gpu-array)))
;;         (setf (gpuarray-buffer gpu-array)
;;               (buffer-sub-data buffer c-array (gpuarray-offset gpu-array)
;;                                :array-buffer))
;;         (error "The c-array must of the same type as the target gpu-array and not have a length exceeding that of the gpu-array."))
;;     gpu-array))
