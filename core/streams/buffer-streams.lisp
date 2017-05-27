(in-package :cepl.streams)

;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

(defmethod print-object ((object buffer-stream) stream)
  (format stream "#<CEPL:BUFFER-STREAM (~s) :LENGTH ~s~@[ :INDEXED ~s~]>"
          (buffer-stream-vao object)
          (buffer-stream-length object)
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
  ;;   (mapcar #'free-gpu-array-bb (buffer-stream-gpu-arrays buffer-stream)))
  (blank-buffer-stream buffer-stream))


(defun make-buffer-stream (gpu-arrays
                           &key index-array (start 0) length
                             (retain-arrays t) (primitive :triangles))
  (unless gpu-arrays
    (error 'make-buffer-stream-with-no-gpu-arrays))
  (let ((gpu-arrays (listify gpu-arrays)))
    (cepl.context::if-gl-context
     (init-buffer-stream-from-id %pre% (make-vao gpu-arrays index-array)
                                 gpu-arrays index-array start length
                                 retain-arrays)
     (make-uninitialized-buffer-stream primitive)
     gpu-arrays)))

(defun make-buffer-stream-from-id (vao-gl-object gpu-arrays
                                   &key index-array (start 0) length
                                     retain-arrays (primitive :triangles))
  (unless gpu-arrays
    (error 'make-buffer-stream-with-no-gpu-arrays))
  (let ((gpu-arrays (listify gpu-arrays)))
    (init-buffer-stream-from-id
     (make-raw-buffer-stream :primitive primitive) vao-gl-object gpu-arrays
     index-array start length retain-arrays)))

(defun init-buffer-stream-from-id (stream-obj vao-gl-object gpu-arrays
                                   index-array start length retain-arrays)
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
        (progn
          (setf (buffer-stream-vao stream-obj) (make-vao-from-id vao-gl-object gpu-arrays index-array)
                (buffer-stream-start stream-obj) start
                (buffer-stream-length stream-obj) length
                (buffer-stream-index-type stream-obj) (when index-array
                                                        (gpu-array-bb-element-type
                                                         index-array))
                (buffer-stream-managed stream-obj) t
                (buffer-stream-gpu-arrays stream-obj) (when retain-arrays
                                                        (list gpu-arrays index-array)))
          stream-obj)
        (error "You can only make buffer-streams from 1D arrays"))))
