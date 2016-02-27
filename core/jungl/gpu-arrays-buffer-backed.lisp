(in-package :jungl)

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

;; defmethod print-mem can be found further down the page

(defmethod backed-by ((object gpuarray))
  :buffer)

(defmethod free ((object gpuarray))
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
  (let ((buffer (make-buffer :managed t)))
    (make-gpuarray :buffer (buffer-reserve-block
                            buffer element-type dimensions
                            :array-buffer access-style)
                   :format-index 0
                   :dimensions (listify dimensions)
                   :access-style access-style)))

(defmethod make-gpu-array ((initial-contents t)
                           &key dimensions element-type (access-style :static-draw)
                             (alignment 1))
  (with-c-array (c-array (make-c-array initial-contents :dimensions dimensions
                                       :element-type element-type
                                       :alignment alignment))
    (make-gpu-array c-array :access-style access-style)))

(defmethod make-gpu-array ((initial-contents c-array)
                           &key (access-style :static-draw)
			     dimensions)
  (let ((buffer (make-buffer :managed t))
	(dimensions (listify dimensions))
	(c-dimensions (dimensions initial-contents)))
    (when dimensions
      (asserting (and (every #'= c-dimensions dimensions)
		      (= (length c-dimensions) (length dimensions)))
		 make-gpu-array-from-c-array-mismatched-dimensions
		 :c-array-dimensions c-dimensions
		 :provided-dimensions dimensions))
    (make-gpuarray :buffer (buffer-data buffer initial-contents
                                        :array-buffer access-style)
                   :format-index 0
                   :dimensions (dimensions initial-contents)
                   :access-style access-style)))

(deferror make-gpu-array-from-c-array-mismatched-dimensions ()
    (c-array-dimensions provided-dimensions)
    "Jungl: make-gpu-array mismatched dimensions

A call to #'make-gpu-array was made with a c-array as the initial-contents.
The dimensions of the c-array are ~s, however the dimensions given in the
call to #'make-gpu-array were ~s"
  c-array-dimensions provided-dimensions)

(defun make-gpu-arrays (c-arrays &key (access-style :static-draw))
  "This function creates a list of gpu-arrays residing in a
   single buffer in opengl. It create one gpu-array for each
   c-array in the list passed in.

   Access style is optional but if you are comfortable with
   opengl, and know what type of usage pattern thsi array will
   have, you can set this to any of the following:
   (:stream-draw :stream-read :stream-copy :static-draw
    :static-read :static-copy :dynamic-draw :dynamic-read
    :dynamic-copy)"
   ;;   Finally you can provide an existing buffer if you want to
   ;; use it rather than creating a new buffer. Note that all
  ;; existing data in the buffer will be destroyed in the process
  ;; {TODO} Really? where?
  (let ((buffer (multi-buffer-data (make-buffer :managed t) c-arrays
                                   :array-buffer access-style)))
    (loop :for c-array :in c-arrays :for i :from 0 :collecting
       (make-gpuarray :buffer buffer
                      :format-index i
                      :dimensions (dimensions c-array)
                      :access-style access-style))))

(defun subseq-g (array start &optional end)
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

(defmacro with-gpu-array-as-pointer
    ((temp-name gpu-array &key (access-type :read-write)) &body body)
  "This macro is really handy if you need to have random access
   to the data on the gpu. It takes a gpu-array and maps it
   giving you the pointer"
  (unless (find access-type '(:read-write :read-only :write-only))
    (error "The access argument must be set to :read-write :read-only or :write-only"))
  (let ((glarray-pointer (gensym "POINTER"))
	(array-sym (gensym "BUFFER"))
        (buffer-sym (gensym "BUFFER"))
        (target (gensym "target")))
    `(progn
       (let ((,array-sym ,gpu-array))
	 (unless (typep ,array-sym 'gpuarray)
	   (if (typep ,array-sym 'gpu-array-t)
	       (error "Unfortunately jungl doesnt not support texture backed gpu-array right now, it should, and it will...But not today. Prod me with a github issue if you need this urgently")
	       (error "with-gpu-array-* does not support the type ~s"
		      (type-of ,array-sym))))
	 (let ((,buffer-sym (gpuarray-buffer ,array-sym))
	       (,target :array-buffer))
	   (force-bind-buffer ,buffer-sym ,target)
	   (gl:with-mapped-buffer (,glarray-pointer
				   ,target
				   ,access-type)
	     (if (pointer-eq ,glarray-pointer (null-pointer))
		 (error "with-gpu-array-as-*: buffer mapped to null pointer~%Have you defintely got an opengl context?~%~s"
			,glarray-pointer)
		 (let ((,temp-name ,glarray-pointer))
		   ,@body))))))))

;; [TODO] Dont require a temporary name, just use the one it has
;;        this makes it feel more magical to me and also it is
;;        in-line with things like with-slots
;; [TODO] Need to unmap if something goes wrong
(defmacro with-gpu-array-as-c-array
    ((temp-name gpu-array &key (access-type :read-write)) &body body)
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
  (let ((ggpu-array (gensym "gpu-array")))
    `(let ((,ggpu-array ,gpu-array))
       (with-gpu-array-as-pointer (,temp-name ,ggpu-array :access-type ,access-type)
         (let ((,temp-name
                (make-c-array-from-pointer
                 (gpuarray-dimensions ,ggpu-array)
                 (element-type ,ggpu-array)
                 (cffi:inc-pointer ,temp-name (gpuarray-offset ,ggpu-array)))))
           ,@body)))))


(defmethod print-mem ((thing gpuarray) &optional (size-in-bytes 64) (offset 0))
  (with-gpu-array-as-pointer (a thing :access-type :read-only)
    (print-mem (cffi:inc-pointer a offset) size-in-bytes)))

(defun gpu-array-pull-1 (gpu-array)
  "This function returns the contents of the gpu array as a c-array
   Note that you often dont need to use this as the generic
   function pull-g will call this function if given a gpu-array"
  (with-gpu-array-as-c-array (x gpu-array :access-type :read-only)
    (clone-c-array x)))

;; allignmetn
(defmethod push-g ((object list) (destination gpuarray))
  (with-c-array (tmp (make-c-array object
                                   :dimensions (dimensions destination)
                                   :element-type (element-type destination)
                                   :alignment 1))
    (push-g tmp destination)))

(defmethod push-g ((object c-array) (destination gpuarray))
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

(defmethod pull1-g ((object gpuarray))
  (gpu-array-pull-1 object))

(defmethod pull-g ((object gpuarray))
  (with-gpu-array-as-c-array (x object :access-type :read-only)
    (pull1-g x)))

;; {TODO} copy buffer to buffer: glCopyBufferSubData
;; http://www.opengl.org/wiki/GLAPI/glCopyBufferSubData
