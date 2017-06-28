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

(defun2 blank-buffer-object (buffer)
  (setf (gpu-buffer-id buffer) 0)
  (setf (gpu-buffer-arrays buffer)
        (make-array 0 :element-type 'gpu-array-bb
                    :initial-element +null-buffer-backed-gpu-array+
                    :adjustable t :fill-pointer 0))
  buffer)

(defun2 free-buffer (buffer)
  (with-foreign-object (id :uint)
    (setf (mem-ref id :uint) (gpu-buffer-id buffer))
    (blank-buffer-object buffer)
    (%gl:delete-buffers 1 id)))

(defun2 free-buffers (buffers)
  (with-foreign-object (id :uint (length buffers))
    (loop :for buffer :in buffers :for i :from 0 :do
       (setf (mem-aref id :uint i) (gpu-buffer-id buffer))
       (blank-buffer-object buffer))
    (%gl:delete-buffers 1 id)))

(defmacro with-buffer ((var-name buffer &optional (buffer-target :array-buffer))
                       &body body)
  (alexandria:with-gensyms (old-id cache-id target)
    `(let* ((,var-name ,buffer)
            ,@(if (keywordp buffer-target)
                  `((,cache-id ,(cepl.context::buffer-kind->cache-index
                                 buffer-target)))
                  `((,target ,buffer-target)
                    (,cache-id (cepl.context::buffer-kind->cache-index
                                ,target))))
            (,old-id
             (cepl.context::gpu-buffer-bound-id *cepl-context* ,cache-id)))
       (unwind-protect
            (progn
              (cepl.context::set-gpu-buffer-bound-id
               *cepl-context* ,cache-id (if ,var-name (gpu-buffer-id ,var-name) 0))
              ,@body)
         (cepl.context::set-gpu-buffer-bound-id
          *cepl-context* ,cache-id ,old-id)))))

(defun2 gen-buffer ()
  (first (gl:gen-buffers 1)))

(defun2 init-gpu-buffer-now (new-buffer gl-object initial-contents
                            buffer-target usage)
  (declare (symbol buffer-target usage))
  (setf (gpu-buffer-id new-buffer) gl-object)
  (setf (gpu-buffer-arrays new-buffer)
        (make-array 0 :element-type 'gpu-array-bb
                    :initial-element +null-buffer-backed-gpu-array+
                    :adjustable t :fill-pointer 0))
  (cepl.context::register-gpu-buffer *cepl-context* new-buffer)
  (if initial-contents
      (if (list-of-c-arrays-p initial-contents)
          (multi-buffer-data new-buffer initial-contents buffer-target usage)
          (buffer-data new-buffer initial-contents
                       :target buffer-target :usage usage))
      new-buffer))

(defun2 list-of-c-arrays-p (x)
  (and (listp x) (every #'c-array-p x)))

(defun2 make-gpu-buffer-from-id (gl-object &key initial-contents
                                            (buffer-target :array-buffer)
                                            (usage :static-draw))
  (declare (symbol buffer-target usage))
  (init-gpu-buffer-now
   (make-uninitialized-gpu-buffer) gl-object initial-contents
   buffer-target usage))

(defun2 make-gpu-buffer (&key initial-contents
                          (buffer-target :array-buffer)
                          (usage :static-draw))
  (declare (symbol buffer-target usage))
  (assert (or (null initial-contents)
              (typep initial-contents 'c-array)
              (list-of-c-arrays-p initial-contents)))
  (cepl.context::if-gl-context
   (init-gpu-buffer-now
    %pre% (gen-buffer) initial-contents buffer-target usage)
   (make-uninitialized-gpu-buffer)))

(defun2 buffer-data-raw (data-pointer byte-size buffer
                        &optional (target :array-buffer) (usage :static-draw)
                          (byte-offset 0))
  (with-buffer (foo buffer target)
    (%gl:buffer-data target byte-size
                     (cffi:inc-pointer data-pointer byte-offset)
                     usage)
    (setf (gpu-buffer-arrays buffer)
          (make-array 1 :element-type 'gpu-array-bb :initial-element
                      (%make-gpu-array-bb
                       :dimensions (list byte-size)
                       :buffer buffer
                       :access-style usage
                       :element-type :uint8
                       :byte-size byte-size
                       :offset-in-bytes-into-buffer 0)))
    buffer))

(defun2 buffer-data (buffer c-array &key (target :array-buffer)
                                     (usage :static-draw) (offset 0) byte-size)
  (buffer-data-raw (pointer c-array)
                   (or byte-size (cepl.c-arrays::c-array-byte-size c-array))
                   buffer target usage (* offset (element-byte-size c-array))))

(defun2 multi-buffer-data (buffer c-arrays target usage)
  (let* ((c-array-byte-sizes (loop :for c-array :in c-arrays :collect
                                (cepl.c-arrays::c-array-byte-size c-array)))
         (total-size (reduce #'+ c-array-byte-sizes)))
    (map nil #'free (gpu-buffer-arrays buffer))
    (with-buffer (foo buffer target)
      (buffer-reserve-block-raw buffer total-size target usage)
      (buffer-data buffer (first c-arrays)
                   :target target
                   :usage usage
                   :byte-size total-size)
      (let ((offset 0))
        (setf (gpu-buffer-arrays buffer)
              (make-array
               (length c-arrays) :element-type 'gpu-array-bb :initial-contents
               (loop :for byte-size :in c-array-byte-sizes
                  :collect (%make-gpu-array-bb
                            :dimensions (list byte-size)
                            :buffer buffer
                            :access-style usage
                            :element-type :uint8
                            :byte-size byte-size
                            :offset-in-bytes-into-buffer offset)
                  :do (incf offset byte-size)))))
      (loop :for c :in c-arrays :for g :across (gpu-buffer-arrays buffer) :do
         (gpu-array-sub-data g c :types-must-match nil)))
    buffer))

(defun2 gpu-array-sub-data (gpu-array c-array &key (types-must-match t))
  (when types-must-match
    (assert (equal (gpu-array-bb-element-type gpu-array)
                   (c-array-element-type c-array))))
  (let ((byte-size (cepl.c-arrays::c-array-byte-size c-array))
        (byte-offset (gpu-array-bb-offset-in-bytes-into-buffer
                      gpu-array)))
    (unless (>= (gpu-array-bb-byte-size gpu-array) byte-size)
      (error "The data you are trying to sub into the gpu-array does not fit
c-array: ~s (byte-size: ~s)
gpu-array: ~s (byte-size: ~s)"
             c-array byte-size
             gpu-array (gpu-array-bb-byte-size gpu-array)))
    (with-buffer (foo (gpu-array-bb-buffer gpu-array) :array-buffer)
      (%gl:buffer-sub-data :array-buffer byte-offset byte-size (pointer c-array))
      gpu-array)))

(defun2 buffer-reserve-block-raw (buffer byte-size target usage)
  (with-buffer (foo buffer target)
    (%gl:buffer-data target byte-size (cffi:null-pointer) usage)
    buffer))

(defun2 buffer-reserve-block (buffer type dimensions target usage)
  (with-buffer (foo buffer target)
    (unless dimensions (error "dimensions are not optional when reserving a buffer block"))
    (let* ((dimensions (listify dimensions))
           (byte-size (cepl.c-arrays::gl-calc-byte-size type dimensions)))
      (buffer-reserve-block-raw buffer byte-size target usage)
      (setf (gpu-buffer-arrays buffer)
            (make-array 1 :element-type 'gpu-array-bb :initial-element
                        (%make-gpu-array-bb
                         :dimensions (list byte-size)
                         :buffer buffer
                         :access-style usage
                         :element-type :uint8
                         :byte-size byte-size
                         :offset-in-bytes-into-buffer 0))))
    buffer))

(defun2 reallocate-buffer (buffer)
  (assert (= (length (gpu-buffer-arrays buffer)) 1))
  (let ((curr (aref (gpu-buffer-arrays buffer) 0)))
    (buffer-reserve-block-raw buffer
                              (gpu-array-bb-byte-size curr)
                              :array-buffer
                              (gpu-array-bb-access-style curr))))

;;---------------------------------------------------------------
