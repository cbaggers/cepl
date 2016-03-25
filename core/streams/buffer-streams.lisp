(in-package :cepl.streams)

;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

(defmethod print-object ((object buffer-stream) stream)
  (format stream "#<JUNGL:BUFFER-STREAM (~s) :LENGTH ~s~@[ :ARRAYS ~s~]~@[ :INDEXED ~s~]>"
	  (buffer-stream-vao object)
	  (buffer-stream-length object)
	  (when (buffer-stream-gpu-arrays object)
	    (length (first (buffer-stream-gpu-arrays object))))
	  (not (null (buffer-stream-index-type object)))))

(defmethod free ((object buffer-stream))
  (free-buffer-stream object))

(defun blank-buffer-stream (buffer-stream)
  (setf (buffer-stream-vao buffer-stream) nil)
  (setf (buffer-stream-start buffer-stream) 0)
  (setf (buffer-stream-length buffer-stream) 0)
  (setf (buffer-stream-index-type buffer-stream) nil)
  (setf (buffer-stream-managed buffer-stream) nil))

(defun free-buffer-stream (buffer-stream)
  (when (buffer-stream-managed buffer-stream)
    (free-vao (buffer-stream-vao buffer-stream)))
  ;; (when (buffer-stream-gpu-arrays buffer-stream)
  ;;   (mapcar #'free-gpu-array-b (buffer-stream-gpu-arrays buffer-stream)))
  (blank-buffer-stream buffer-stream))


(defun make-buffer-stream (gpu-arrays &key index-array (start 0) length
                                        retain-arrays)

  (let* ((gpu-arrays (listify gpu-arrays)))
    (make-buffer-stream-from-id (make-vao gpu-arrays index-array)
                                gpu-arrays
                                :index-array index-array
                                :start start
                                :length length
                                :retain-arrays retain-arrays)))

(defun make-buffer-stream-from-id (vao-gl-object gpu-arrays
                                   &key index-array (start 0) length
                                     retain-arrays)
  (unless gpu-arrays
    (error 'make-buffer-stream-with-no-gpu-arrays))
  (let* ((gpu-arrays (listify gpu-arrays))
         ;; THIS SEEMS WEIRD BUT IF HAVE INDICES ARRAY THEN
         ;; LENGTH MUST BE LENGTH OF INDICES ARRAY NOT NUMBER
         ;; OF TRIANGLES
         (length (or length
                     (when index-array (first (dimensions index-array)))
                     (apply #'min (mapcar #'(lambda (x) (first (dimensions x)))
                                          gpu-arrays)))))
    (if (and (every #'1d-p gpu-arrays) (if index-array (1d-p index-array) t))
        (make-raw-buffer-stream
         :vao (make-vao-from-id vao-gl-object gpu-arrays index-array)
         :start start
         :length length
         :index-type (when index-array (element-type index-array))
         :managed t
         :gpu-arrays (when retain-arrays
                       (list gpu-arrays index-array)))
        (error "You can only make buffer-streams from 1D arrays"))))
