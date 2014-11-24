(in-package :cgl)

;; [TODO] Justify your use of the gl- prefix everywhere.
;; [TODO] alignment, what the hell do we do with that? I'm a bit 
;;        drunk to make these choices right now
;; [TODO] How do we free these? Tag buffer format type as :free and handle?

;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(defstruct gpuarray 
  buffer
  format-index
  (start 0)
  dimensions
  (access-style :static-draw))

(defmethod print-object ((object gpuarray) stream)
  (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by :BUFFER>"
          (element-type object)
          (gpuarray-dimensions object)))

(defmethod backed-by ((object gpuarray))
  :buffer)

(defmethod gl-free ((object gpuarray))
  (free-gpu-array-b object))

(defmethod free-gpu-array ((gpu-array gpuarray))
  (free-gpu-array-b gpu-array))

(defun blank-gpu-array-b-object (gpu-array)
  (setf (gpuarray-buffer gpu-array) nil
        (gpuarray-format-index gpu-array) nil
        (gpuarray-start gpu-array) nil
        (gpuarray-dimensions gpu-array) nil
        (gpuarray-access-style gpu-array) nil))

;; we only set the buffer slot type as undefined as the size and
;; offset dont change
;; If the buffer is managed and all formats are undefined then free it.
(defun free-gpu-array-b (gpu-array)
  (let* ((buffer (gpuarray-buffer gpu-array))
         (buffer-formats (glbuffer-format buffer)))
    (setf (first (nth (gpuarray-format-index gpu-array) buffer-formats))
          :UNDEFINED)
    (when (and (glbuffer-managed buffer)
               (loop :for format :in buffer-formats :always 
                  (eq (car format) :UNDEFINED)))
      (free-buffer buffer)))
  (blank-gpu-array-b-object gpu-array))

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

;; [TODO] This looks wrong, the beginning right? NO!
;;        remember that the gpu-array could be a sub-array
;;        in that case the
(defun gpuarray-offset (gpu-array)
  "Returns the offset in bytes from the beginning of the buffer
   that this gpuarray is stored at"
  (let ((format (gpuarray-format gpu-array)))
   (+ (third format) (gl-calc-byte-size (first format) 
                                        (list (gpuarray-start gpu-array))))))

;;---------------------------------------------------------------

;; c-array (dimensions element-type &key initial-contents displaced-by (alignment 1))
;; old-gpu (initial-contents &key element-type length access-style)
;; ??????? (initial-contents &key element-type dimensions access-style)
;; [TODO] Check to see we have all the data we need
;; [TODO] all make-gpu-array need the start argument specified
;; [TODO] all dimensions need checking for sanity..some clearly dont have any :D
(defmethod make-gpu-array ((initial-contents null) 
                           &key element-type dimensions
                             (access-style :static-draw))
  (declare (ignore initial-contents))
  (let ((buffer (gen-buffer :managed t)))
    (make-gpuarray :buffer (buffer-reserve-block
                            buffer element-type dimensions
                            :array-buffer access-style)
                   :format-index 0
                   :dimensions dimensions
                   :access-style access-style)))

(defmethod make-gpu-array ((initial-contents t) 
                           &key dimensions element-type (access-style :static-draw) 
                             (alignment 1))
  (with-c-array (c-array (make-c-array initial-contents :dimensions dimensions
                                       :element-type element-type
                                       :alignment alignment))
    (make-gpu-array c-array :access-style access-style)))

(defmethod make-gpu-array ((initial-contents c-array) 
                           &key (access-style :static-draw))
  (let ((buffer (gen-buffer :managed t)))
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
  (let ((buffer (multi-buffer-data (gen-buffer :managed t) c-arrays 
                                   :array-buffer access-style)))
    (loop :for c-array :in c-arrays :for i :from 0 :collecting 
       (make-gpuarray :buffer buffer
                      :format-index i
                      :dimensions (dimensions c-array)
                      :access-style access-style))))

(defmethod gl-subseq ((array gpuarray) start &optional end)
  (let ((dimensions (dimensions array)))
    (if (> (length dimensions) 1)
        (error "Cannot take subseq of multidimensional array")
        (let* ((length (first dimensions))
               (parent-start (gpuarray-start array))
               (new-start (+ parent-start (max 0 start)))
               (end (or end length)))
          (if (and (< start end) (< start length) (<= end length))
              (make-gpuarray :buffer (gpuarray-buffer array)
                             :format-index (gpuarray-format-index array)
                             :start new-start
                             :dimensions (list (- end start))
                             :access-style (gpuarray-access-style array))
              (error "Invalid subseq start or end for c-array"))))))

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
     (setf (aref-c mygpuarray 2) 5.0))

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

(defun gpu-array-pull-1 (gpu-array)
  "This function returns the contents of the gpu array as a c-array
   Note that you often dont need to use this as the generic
   function gl-pull will call this function if given a gpu-array"
  (with-gpu-array-as-c-array (gpu-array :access-type :read-only)
    (clone-c-array gpu-array)))

;; allignmetn
(defmethod gl-push ((object list) (destination gpuarray))
  (with-c-array (tmp (make-c-array object
                                   :dimensions (dimensions destination)
                                   :element-type (element-type destination) 
                                   :alignment 1))
    (gl-push tmp destination)))

(defmethod gl-push ((object c-array) (destination gpuarray))
  (let* ((buffer (gpuarray-buffer destination))
         (format (gpuarray-format destination))
         (type (first format))
         (ob-dimen (dimensions object))
         (des-dimen (dimensions object)))
    (if (and (eq (element-type object) type)
             (if (= 1 (length des-dimen) (length ob-dimen))
                 (<= (first ob-dimen) (first des-dimen))
                 (equal ob-dimen des-dimen)))
        (setf (gpuarray-buffer destination)
              (buffer-sub-data buffer object (gpuarray-offset destination)
                               :array-buffer))
        (error "If the arrays are 1D then the length of the source array must
be <= length of the destination array. If the arrays have more than 1 
dimension then their sizes must match exactly"))
    destination))

(defmethod gl-pull-1 ((object gpuarray)) 
  (gpu-array-pull-1 object))

(defmethod gl-pull ((object gpuarray))
  (with-gpu-array-as-c-array (object :access-type :read-only)
    (gl-pull-1 object)))

;; copy buffer to buffer: glCopyBufferSubData
;; http://www.opengl.org/wiki/GLAPI/glCopyBufferSubData
