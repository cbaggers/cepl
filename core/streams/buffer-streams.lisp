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

(defun+ blank-buffer-stream (buffer-stream)
  (setf (buffer-stream-vao buffer-stream) 0)
  (setf (buffer-stream-start buffer-stream) 0)
  (setf (buffer-stream-length buffer-stream) 0)
  (setf (buffer-stream-index-type buffer-stream) nil)
  (setf (buffer-stream-managed buffer-stream) nil))

(defun+ free-buffer-stream (buffer-stream)
  (when (buffer-stream-managed buffer-stream)
    (free-vao (buffer-stream-vao buffer-stream)))
  ;; (when (buffer-stream-gpu-arrays buffer-stream)
  ;;   (mapcar #'free-gpu-array-bb (buffer-stream-gpu-arrays buffer-stream)))
  (blank-buffer-stream buffer-stream))


(defun+ make-buffer-stream (gpu-arrays
                           &key index-array (start 0) length
                             (retain-arrays t) (primitive :triangles))
  (when (not gpu-arrays)
    (assert (not index-array) () 'index-on-buffer-stream-with-no-gpu-arrays))
  (let ((gpu-arrays (preprocess-gpu-arrays-for-vao gpu-arrays)))
    (cepl.context::if-gl-context
     (init-buffer-stream-from-id %pre% (make-vao gpu-arrays index-array)
                                 gpu-arrays index-array start length
                                 retain-arrays)
     (make-uninitialized-buffer-stream primitive)
     gpu-arrays)))

(defun+ make-buffer-stream-from-id (vao-gl-object gpu-arrays
                                   &key index-array (start 0) length
                                     retain-arrays (primitive :triangles))
  (when (not gpu-arrays)
    (assert (not index-array) () 'index-on-buffer-stream-with-no-gpu-arrays))
  (let ((gpu-arrays (preprocess-gpu-arrays-for-vao gpu-arrays)))
    (init-buffer-stream-from-id
     (make-raw-buffer-stream :primitive primitive) vao-gl-object gpu-arrays
     index-array start length retain-arrays)))

(defun+ init-buffer-stream-from-id (stream-obj
                                    vao-gl-object gpu-arrays
                                    index-array start length retain-arrays)
  (when (not gpu-arrays)
    (assert (not index-array) () 'index-on-buffer-stream-with-no-gpu-arrays))
  (let* ((gpu-arrays (preprocess-gpu-arrays-for-vao gpu-arrays))
         ;; THIS SEEMS WEIRD BUT IF HAVE INDICES ARRAY THEN
         ;; LENGTH MUST BE LENGTH OF INDICES ARRAY NOT NUMBER
         ;; OF TRIANGLES
         (length (if gpu-arrays
                     length
                     1))
         (length (or length
                     (when index-array (first (dimensions index-array)))
                     (apply #'min (mapcar #'(lambda (x)
                                              (let ((x (if (consp x)
                                                           (car x)
                                                           x)))
                                                (first (dimensions x))))
                                          gpu-arrays)))))
    (assert (and (every #'cons-aware-1d-p gpu-arrays)
                 (if index-array (1d-p index-array) t))
            () "You can only make buffer-streams from 1D arrays")
    (setf (buffer-stream-start stream-obj) start
          (buffer-stream-length stream-obj) length
          (buffer-stream-managed stream-obj) t

          (buffer-stream-vao stream-obj) (make-vao-from-id vao-gl-object
                                                           gpu-arrays
                                                           index-array)

          (buffer-stream-index-type stream-obj) (when index-array
                                                  (gpu-array-bb-element-type
                                                   index-array))

          (buffer-stream-gpu-arrays stream-obj) (when retain-arrays
                                                  (list gpu-arrays index-array)))
    stream-obj))

(defun process-stream-layout (layout)
  (assert (and (listp layout)
               (find :dimensions layout)
               (find :element-type layout))
          ()  'invalid-stream-layout :layout layout)
  (destructuring-bind (&key dimensions element-type) layout
    (let* ((dimensions (listify dimensions)))
      (assert (= (length dimensions) 1)
              () "CEPL: You can only make buffer-streams from 1D layouts")
      (list element-type (first dimensions)))))

(defun+ make-buffer-stream-from-id-and-layouts (vao-gl-object
                                                data-layouts
                                                index-layout
                                                &key
                                                (start 0)
                                                length
                                                (primitive :triangles))
  (when (not data-layouts)
    (assert (not index-layout) () 'index-on-buffer-stream-with-no-gpu-layouts))
  (let ((data-info (mapcar #'process-stream-layout data-layouts))
        (index-info (when index-layout (process-stream-layout index-layout)))
        (stream-obj (make-raw-buffer-stream :primitive primitive)))
    (let* (;; THIS SEEMS WEIRD BUT IF HAVE INDICES ARRAY THEN
           ;; LENGTH MUST BE LENGTH OF INDICES ARRAY NOT NUMBER
           ;; OF TRIANGLES
           (length (if data-info
                       length
                       1))
           (length (or length
                       (when index-info (second index-info))
                       (apply #'min (mapcar #'second data-info)))))
      (setf (buffer-stream-start stream-obj) start
            (buffer-stream-length stream-obj) length
            (buffer-stream-managed stream-obj) nil
            (buffer-stream-vao stream-obj) vao-gl-object
            (buffer-stream-index-type stream-obj) (when index-info
                                                    (first index-info)))
      stream-obj)))
