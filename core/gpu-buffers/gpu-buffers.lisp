(in-package :cepl.gpu-buffers)

;;;--------------------------------------------------------------
;;; BUFFERS ;;;
;;;---------;;;

;; [TODO] Should buffers have pull-g and push-g? of course! do it :)

(defmethod print-object ((object gpu-buffer) stream)
  (if (initialized-p object)
      (call-next-method object stream)
      (format stream "#<GPU-BUFFER :UNITIALIZED>")))

(defmethod free ((object gpu-buffer))
  (free-buffer object))

(defun blank-buffer-object (buffer)
  (setf (gpu-buffer-id buffer) 0)
  (setf (gpu-buffer-arrays buffer) +null-gpu-buffer+)
  (setf (gpu-buffer-managed buffer) nil)
  buffer)

(defun free-buffer (buffer)
  (with-foreign-object (id :uint)
    (setf (mem-ref id :uint) (gpu-buffer-id buffer))
    (blank-buffer-object buffer)
    (%gl:delete-buffers 1 id)))

(defun free-buffers (buffers)
  (with-foreign-object (id :uint (length buffers))
    (loop :for buffer :in buffers :for i :from 0 :do
       (setf (mem-aref id :uint i) (gpu-buffer-id buffer))
       (blank-buffer-object buffer))
    (%gl:delete-buffers 1 id)))

;; [TODO] This needs a rework given how gl targets operate
(let ((buffer-id-cache nil)
      (buffer-target-cache nil))
  (defun bind-buffer (buffer buffer-target)
    (let ((id (gpu-buffer-id buffer)))
      (unless (and (eq id buffer-id-cache)
                   (eq buffer-target buffer-target-cache))
        (cl-opengl-bindings:bind-buffer buffer-target id)
        (setf buffer-target-cache id)
        (setf buffer-target-cache buffer-target)))
    buffer)
  (defun force-bind-buffer (buffer buffer-target)
    "Binds the specified opengl buffer to the target"
    (let ((id (gpu-buffer-id buffer)))
      (cl-opengl-bindings:bind-buffer buffer-target id)
      (setf buffer-id-cache id)
      (setf buffer-target-cache buffer-target))
    buffer)
  (defun unbind-buffer ()
    (cl-opengl-bindings:bind-buffer :array-buffer 0)
    (setf buffer-id-cache 0)
    (setf buffer-target-cache :array-buffer)))

(defmacro with-buffer ((var-name buffer &optional (buffer-target :array-buffer))
		       &body body)
  `(let* ((,var-name ,buffer))
     (unwind-protect (progn (bind-buffer ,var-name ,buffer-target)
			    ,@body)
       (unbind-buffer ,var-name))))

(defun gen-buffer ()
  (first (gl:gen-buffers 1)))

(defun init-gpu-buffer-now (new-buffer gl-object initial-contents
				    buffer-target usage managed)
  (declare (symbol buffer-target usage))
  (setf (gpu-buffer-id new-buffer) gl-object
	(gpu-buffer-managed new-buffer) managed)
  (setf (gpu-buffer-arrays new-buffer)
	(make-array 0 :element-type 'gpu-array-bb
		    :initial-element +null-buffer-backed-gpu-array+
		    :adjustable t :fill-pointer 0))
  (if initial-contents
      (buffer-data new-buffer initial-contents
		   :target buffer-target :usage usage)
      new-buffer))

(defun make-gpu-buffer-from-id (gl-object &key initial-contents
					    (buffer-target :array-buffer)
					    (usage :static-draw)
					    (managed nil))
  (declare (symbol buffer-target usage))
  (init-gpu-buffer-now
   (make-uninitialized-gpu-buffer) gl-object initial-contents
   buffer-target usage managed))

(defun make-gpu-buffer (&key initial-contents
			  (buffer-target :array-buffer)
			  (usage :static-draw)
			  (managed nil))
  (declare (symbol buffer-target usage))
  (cepl.memory::if-context
   (init-gpu-buffer-now
    %pre% (gen-buffer) initial-contents buffer-target usage managed)
   (make-uninitialized-gpu-buffer)))


(defun make-managed-gpu-buffer (&key initial-contents
				  (buffer-target :array-buffer)
				  (usage :static-draw))
  (cepl.memory::if-context
   (init-gpu-buffer-now %pre% (gen-buffer) initial-contents
			buffer-target usage t)
   (make-uninitialized-gpu-buffer)))

(defun buffer-data-raw (data-pointer byte-size buffer
			&optional (target :array-buffer) (usage :static-draw)
			  (byte-offset 0))
  (bind-buffer buffer target)
  (%gl:buffer-data target byte-size
		   (cffi:inc-pointer data-pointer byte-offset)
		   usage)
  (setf (gpu-buffer-arrays buffer)
	(make-array 1 :element-type 'gpu-array-bb :initial-element
		    (%make-gpu-array-bb
		     :dimensions (list byte-size)
		     :buffer buffer
		     :start 0
		     :access-style usage
		     :element-type :uint8
		     :byte-size byte-size
		     :offset-in-bytes-into-buffer 0)))
  buffer)

(defun buffer-data (buffer c-array &key (target :array-buffer)
				     (usage :static-draw) (offset 0) byte-size)
  (buffer-data-raw (pointer c-array)
		   (or byte-size (cepl.c-arrays::c-array-byte-size c-array))
		   buffer target usage (* offset (element-byte-size c-array))))

(defun multi-buffer-data (buffer c-arrays target usage)
  (let* ((c-array-byte-sizes (loop :for c-array :in c-arrays :collect
				(cepl.c-arrays::c-array-byte-size c-array)))
	 (total-size (reduce #'+ c-array-byte-sizes)))
    (map nil #'free (gpu-buffer-arrays buffer))
    (bind-buffer buffer target)
    (buffer-reserve-block-raw buffer total-size target usage)
    (buffer-data buffer (first c-arrays)
		 :target target
		 :usage usage
		 :byte-size total-size)
    (let ((offset 0))
      (setf (gpu-buffer-arrays buffer)
	    (make-array
	     (length c-arrays) :element-type 'gpu-array-bb :initial-contents
	     (loop :for c-array :in c-arrays
		:for byte-size :in c-array-byte-sizes
		:collect (%make-gpu-array-bb
			  :dimensions (list byte-size)
			  :buffer buffer
			  :start 0
			  :access-style usage
			  :element-type :uint8
			  :byte-size byte-size
			  :offset-in-bytes-into-buffer offset)
		:do (incf offset byte-size)))))
    (loop :for c :in c-arrays :for g :across (gpu-buffer-arrays buffer) :do
       (gpu-array-sub-data g c)))
  buffer)

(defun gpu-array-sub-data (gpu-array c-array &key (type-must-match t))
  (when type-must-match
    (assert (equal (element-type gpu-array) (element-type c-array))))
  (let ((byte-size (cepl.c-arrays::c-array-byte-size c-array))
	(byte-offset (gpu-array-bb-offset-in-bytes-into-buffer
		      gpu-array)))
    (unless (> byte-size (gpu-array-bb-byte-size gpu-array))
      (error "The data you are trying to sub into the gpu-array does not fit"))
    (bind-buffer (gpu-array-bb-buffer gpu-array) :array-buffer)
    (%gl:buffer-sub-data :array-buffer byte-offset byte-size (pointer c-array)))
  gpu-array)

(defun buffer-reserve-block-raw (buffer byte-size target usage)
  (bind-buffer buffer target)
  (%gl:buffer-data target byte-size (cffi:null-pointer) usage)
  buffer)

(defun buffer-reserve-block (buffer type dimensions target usage)
  (let ((type (safer-gl-type type)))
    (bind-buffer buffer target)
    (unless dimensions (error "dimensions are not optional when reserving a buffer block"))
    (let* ((dimensions (listify dimensions))
           (byte-size (cepl.c-arrays::gl-calc-byte-size type dimensions)))
      (buffer-reserve-block-raw buffer byte-size target usage)
      (setf (gpu-buffer-arrays buffer)
	    (make-array 1 :element-type 'gpu-array-bb :initial-element
		    (%make-gpu-array-bb
		     :dimensions (list byte-size)
		     :buffer buffer
		     :start 0
		     :access-style usage
		     :element-type :uint8
		     :byte-size byte-size
		     :offset-in-bytes-into-buffer 0))))
    buffer))

;;---------------------------------------------------------------

(defun safer-gl-type (type)
  "In some cases cl-opengl doesnt like certain types. :ushort is the main case
as it prefers :unsigned-short. This function fixes this"
  (if (eq type :ushort)
      :unsigned-short
      type))
