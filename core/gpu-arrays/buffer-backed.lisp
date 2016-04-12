(in-package :cepl.gpu-arrays.buffer-backed)

;; [TODO] Justify your use of the gl- prefix everywhere.
;; [TODO] How do we free these? Tag buffer format type as :free and handle?

;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(defmethod print-object ((object gpu-array) stream)
  (if (initialized-p object)
      (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by :BUFFER>"
	      (element-type object)
	      (gpu-array-dimensions object))
      (format stream "#<GPU-ARRAY :UNINITIALIZED :backed-by :BUFFER>")))

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
  (setf (gpu-array-dimensions gpu-array) nil
	(gpu-array-bb-buffer gpu-array) +null-gpu-buffer+
        (gpu-array-bb-start gpu-array) 0
	(gpu-array-bb-access-style gpu-array) :uninitialized
	(gpu-array-bb-element-type gpu-array) nil
	(gpu-array-bb-byte-size gpu-array) 0
	(gpu-array-bb-offset-in-bytes-into-buffer gpu-array) 0))

;; we only set the buffer slot type as undefined as the size and
;; offset dont change
;; If the buffer is managed and all formats are undefined then free it.
(defun free-gpu-array-bb (gpu-array)
  (let* ((buffer (gpu-array-bb-buffer gpu-array)))
    (blank-gpu-array-b-object gpu-array)
    (when (and (cepl.gpu-buffers::gpu-buffer-managed buffer)
	       (not (some #'initialized-p (gpu-buffer-arrays buffer))))
      (free-buffer buffer)))
  nil)

;;---------------------------------------------------------------

(defmethod dimensions ((object gpu-array))
  (gpu-array-dimensions object))

(defmethod element-type ((object gpu-array-bb))
  (gpu-array-bb-element-type object))

;; [TODO] This looks wrong, the beginning right? NO!
;;        remember that the gpu-array could be a sub-array
;;        in that case the correct index into the buffer is
;;        the byte-offset + the start
(defun gpu-array-offset (gpu-array)
  "Returns the offset in bytes from the beginning of the buffer
   that this gpu-array is stored at"
  (+ (gpu-array-bb-offset-in-bytes-into-buffer gpu-array)
     (cepl.c-arrays::gl-calc-byte-size
      (gpu-array-bb-element-type gpu-array)
      (list (gpu-array-bb-start gpu-array)))))

;;---------------------------------------------------------------

;; old-gpu (initial-contents &key element-type length access-style)
;; ??????? (initial-contents &key element-type dimensions access-style)
;; [TODO] Check to see we have all the data we need
;; [TODO] all make-gpu-array need the start argument specified
;; [TODO] all dimensions need checking for sanity..some clearly dont have any :D

(defgeneric make-gpu-array (initial-contents &key))

;;---------------------------------------------------------------

(defun make-gpu-array-share-data (gpu-array-to-modify gpu-array-with-data
				  byte-offset-into-source-data element-type
				  dimensions &optional byte-size)
  (let* ((parent gpu-array-with-data)
	 (child gpu-array-to-modify)
	 (offset byte-offset-into-source-data)
	 (dimensions (listify dimensions))
	 (byte-size (if byte-size byte-size
			(cepl.c-arrays::gl-calc-byte-size
			 element-type dimensions))))
    (setf (gpu-array-dimensions child) dimensions
	  (gpu-array-bb-buffer child) (gpu-array-bb-buffer parent)
	  (gpu-array-bb-access-style child) (gpu-array-bb-access-style parent)
	  (gpu-array-bb-element-type child) element-type
	  (gpu-array-bb-start child) 0 ;; remove
	  (gpu-array-bb-byte-size child) byte-size)
    (setf (gpu-array-bb-offset-in-bytes-into-buffer child)
	  (+ (gpu-array-bb-offset-in-bytes-into-buffer parent) offset))
    child))

;;---------------------------------------------------------------
;; no initial-contents
(defmethod make-gpu-array ((initial-contents null)
                           &key element-type dimensions
                             (access-style :static-draw))
  (declare (ignore initial-contents))
  (labels ((init (arr)
	     (let* ((buffer (buffer-reserve-block
			    (cepl.gpu-buffers::make-managed-gpu-buffer)
			    element-type dimensions :array-buffer
			    access-style))
		    (base-arr (aref (gpu-buffer-arrays buffer) 0)))
	       (make-gpu-array-share-data
		arr base-arr 0 element-type dimensions))))
    (cepl.memory::if-context
     (init %pre%)
     (make-uninitialized-gpu-array-bb))))

;;---------------------------------------------------------------
;; from c-array

(defun init-gpu-array-from-c-array (arr c-array access-style
				    dimensions)
  (let ((dimensions (listify dimensions))
	(c-dimensions (dimensions c-array)))
    (when dimensions
      (asserting (and (every #'= c-dimensions dimensions)
		      (= (length c-dimensions) (length dimensions)))
		 make-gpu-array-from-c-array-mismatched-dimensions
		 :c-arr-dimensions c-dimensions
		 :provided-dimensions dimensions))
    (let* ((source (buffer-data (gpu-array-bb-buffer arr)
			       c-array :usage access-style))
	   (base-arr (aref )))
      (make-gpu-array-share-data
       arr )
      (setf (gpu-array-bb-buffer arr) source
	    (gpu-array-bb-format-index arr) 0
	    (gpu-array-dimensions arr) (dimensions c-array)
	    (gpu-array-bb-access-style arr) access-style)))
  arr)

(defmethod make-gpu-array ((initial-contents c-array)
                           &key (access-style :static-draw) dimensions)
  (let ((buffer (cepl.gpu-buffers::make-managed-gpu-buffer)))
    (cepl.memory::if-context
     (init-gpu-array-from-c-array %pre% initial-contents access-style dimensions)
     (make-uninitialized-gpu-array-bb buffer)
     (list buffer))))

;;---------------------------------------------------------------
;; from lisp-data

(defmethod make-gpu-array ((initial-contents t)
                           &key dimensions element-type (access-style :static-draw))
  (let ((buffer (cepl.gpu-buffers::make-managed-gpu-buffer)))
    (cepl.memory::if-context
     (with-c-array (c-array (make-c-array initial-contents :dimensions dimensions
					  :element-type element-type))
       (init-gpu-array-from-c-array %pre% c-array access-style dimensions))
     (make-uninitialized-gpu-array-bb buffer)
     (list buffer))))

;;---------------------------------------------------------------
;; from multiple c-arrays

(defun make-gpu-arrays (c-arrays &key (access-style :static-draw))
  (let ((buffer (cepl.gpu-buffers::make-managed-gpu-buffer)))
    (cepl.memory::delay-initialization
     (lambda () (multi-buffer-data c-arrays buffer :array-buffer access-style))
     (list buffer))
    (loop :for c-array :in c-arrays :for i :from 0 :collecting
       (%make-gpu-array-bb :buffer buffer
			   :format-index i
			   :dimensions (dimensions c-array)
			   :access-style access-style))))

;;---------------------------------------------------------------

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
