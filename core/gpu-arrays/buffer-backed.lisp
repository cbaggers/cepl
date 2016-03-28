(in-package :cepl.gpu-arrays.buffer-backed)

;; [TODO] Justify your use of the gl- prefix everywhere.
;; [TODO] How do we free these? Tag buffer format type as :free and handle?

;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(defmethod print-object ((object gpu-array) stream)
  (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by :BUFFER>"
          (element-type object)
          (gpu-array-dimensions object)))

;; defmethod print-mem can be found further down the page

(defmethod free ((object gpu-array))
  (free-gpu-array-bb object))

(defgeneric cepl.gpu-arrays:free-gpu-array (gpu-array))
(defmethod cepl.gpu-arrays:free-gpu-array ((gpu-array gpu-array))
  (free-gpu-array-bb gpu-array))

(defun gpu-array-buffer (gpu-array)
  (%cepl.types::gpu-array-bb-buffer gpu-array))

(defun gpu-array-access-style (gpu-array)
  (%cepl.types::gpu-array-bb-access-style gpu-array))

(defun blank-gpu-array-b-object (gpu-array)
  (setf (gpu-array-bb-buffer gpu-array) +null-gpu-buffer+
        (gpu-array-bb-format-index gpu-array) 0
        (gpu-array-bb-start gpu-array) 0
        (gpu-array-dimensions gpu-array) nil
        (gpu-array-bb-access-style gpu-array) nil))

;; we only set the buffer slot type as undefined as the size and
;; offset dont change
;; If the buffer is managed and all formats are undefined then free it.
(defun free-gpu-array-bb (gpu-array)
  (let* ((buffer (gpu-array-bb-buffer gpu-array))
         (buffer-formats (gpu-buffer-format buffer)))
    (setf (first (nth (gpu-array-bb-format-index gpu-array) buffer-formats))
          :UNDEFINED)
    (when (and (cepl.gpu-buffers::gpu-buffer-managed buffer)
               (loop :for format :in buffer-formats :always
                  (eq (car format) :UNDEFINED)))
      (free-buffer buffer)))
  (blank-gpu-array-b-object gpu-array))

;;---------------------------------------------------------------

(defun gpu-array-format (gpu-array)
  "Returns a list containing the element-type, the length of the
   array and the offset in bytes from the beginning of the buffer
   this gpu-array lives in."
  (nth (gpu-array-bb-format-index gpu-array)
       (gpu-buffer-format (gpu-array-buffer gpu-array))))

(defmethod dimensions ((object gpu-array))
  (gpu-array-dimensions object))

(defun gpu-array-bb-element-type (gpu-array)
  (first (gpu-array-format gpu-array)))

(defmethod element-type ((object gpu-array))
  (first (gpu-array-format object)))

;; [TODO] This looks wrong, the beginning right? NO!
;;        remember that the gpu-array could be a sub-array
;;        in that case the
(defun gpu-array-offset (gpu-array)
  "Returns the offset in bytes from the beginning of the buffer
   that this gpu-array is stored at"
  (let ((format (gpu-array-format gpu-array)))
    (+ (third format) (cepl.c-arrays::gl-calc-byte-size
		       (first format)
		       (list (gpu-array-bb-start gpu-array))))))

;;---------------------------------------------------------------

;; old-gpu (initial-contents &key element-type length access-style)
;; ??????? (initial-contents &key element-type dimensions access-style)
;; [TODO] Check to see we have all the data we need
;; [TODO] all make-gpu-array need the start argument specified
;; [TODO] all dimensions need checking for sanity..some clearly dont have any :D

(defgeneric make-gpu-array (initial-contents &key))

(defmethod make-gpu-array ((initial-contents null)
                           &key element-type dimensions
                             (access-style :static-draw))
  (declare (ignore initial-contents))
  (let ((buffer (cepl.gpu-buffers::make-managed-gpu-buffer)))
    (%make-gpu-array-bb :buffer (buffer-reserve-block
				 buffer element-type dimensions
				 :array-buffer access-style)
			:format-index 0
			:dimensions (listify dimensions)
			:access-style access-style)))

(defmethod make-gpu-array ((initial-contents t)
                           &key dimensions element-type (access-style :static-draw))
  (with-c-array (c-array (make-c-array initial-contents :dimensions dimensions
                                       :element-type element-type))
    (make-gpu-array c-array :access-style access-style)))

(defmethod make-gpu-array ((initial-contents c-array)
                           &key (access-style :static-draw)
			     dimensions)
  (let ((buffer (cepl.gpu-buffers::make-managed-gpu-buffer))
	(dimensions (listify dimensions))
	(c-dimensions (dimensions initial-contents)))
    (when dimensions
      (asserting (and (every #'= c-dimensions dimensions)
		      (= (length c-dimensions) (length dimensions)))
		 make-gpu-array-from-c-array-mismatched-dimensions
		 :c-arr-dimensions c-dimensions
		 :provided-dimensions dimensions))
    (%make-gpu-array-bb :buffer (buffer-data buffer initial-contents
					     :array-buffer access-style)
			:format-index 0
			:dimensions (dimensions initial-contents)
			:access-style access-style)))

(deferror make-gpu-array-from-c-array-mismatched-dimensions ()
    (c-arr-dimensions provided-dimensions)
    "Jungl: make-gpu-array mismatched dimensions

A call to #'make-gpu-array was made with a c-array as the initial-contents.
The dimensions of the c-array are ~s, however the dimensions given in the
call to #'make-gpu-array were ~s"
  c-arr-dimensions provided-dimensions)

(defun make-gpu-arrays (c-arrays &key (access-style :static-draw))
  (let ((buffer (multi-buffer-data
		 (cepl.gpu-buffers::make-managed-gpu-buffer) c-arrays
		 :array-buffer access-style)))
    (loop :for c-array :in c-arrays :for i :from 0 :collecting
       (%make-gpu-array-bb :buffer buffer
			   :format-index i
			   :dimensions (dimensions c-array)
			   :access-style access-style))))

(defun subseq-g (array start &optional end)
  (let ((dimensions (dimensions array)))
    (if (> (length dimensions) 1)
        (error "Cannot take subseq of multidimensional array")
        (let* ((length (first dimensions))
               (parent-start (gpu-array-bb-start array))
               (new-start (+ parent-start (max 0 start)))
               (end (or end length)))
          (if (and (< start end) (< start length) (<= end length))
              (%make-gpu-array-bb
	       :buffer (gpu-array-buffer array)
	       :format-index (gpu-array-bb-format-index array)
	       :start new-start
	       :dimensions (list (- end start))
	       :access-style (gpu-array-access-style array))
              (error "Invalid subseq start or end for c-array"))))))

;; {TODO} copy buffer to buffer: glCopyBufferSubData
;; http://www.opengl.org/wiki/GLAPI/glCopyBufferSubData
