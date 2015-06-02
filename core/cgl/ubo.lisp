(in-package :cgl)

;;---------------------------------------------------

(defvar *lowest-unused-ubo-id* 0)
(defvar *freed-ubo-id* nil)

(defun get-free-ubo-id ()
  (if *freed-ubo-id*
      (pop *freed-ubo-id*)
      (incf *lowest-unused-ubo-id*)))

;;---------------------------------------------------

(defstruct (ubo (:constructor %make-ubo))
  (id 0 :type fixnum)
  (data nil)
  (index 0 :type fixnum))

(defun ubo-data-type (ubo)
  (let ((data (ubo-data ubo)))
    (typecase data
      (glbuffer (caar (glbuffer-format data)))
      (gpuarray (first (gpuarray-format data))))))

(defun make-ubo (&optional data (index 0))
  (assert (>= index 0))
  (assert (or (= index 0) (typep data 'gpuarray)))
  (let ((ubo (%make-ubo :id (get-free-ubo-id)
                        :data data
                        :index index)))
    (%bind-ubo ubo)))

;;---------------------------------------------------

(defun %bind-ubo (ubo)
  (let* ((data (ubo-data ubo))
         (buffer-id (typecase data
                      (glbuffer (glbuffer-buffer-id data))
                      (gpuarray (glbuffer-buffer-id
                                 (gpuarray-buffer data)))))
         (offset (typecase data
                   (glbuffer 0)
                   (gpuarray
                    (destructuring-bind (type len byte-offset)
                        (gpuarray-format data)
                      (declare (ignore len))
                      (+ byte-offset
                         (gl-calc-byte-size type (list (ubo-index ubo))))))))
         (size (typecase data
                 (glbuffer (gl-type-size (caar (glbuffer-format data))))
                 (gpuarray
                  (destructuring-bind (type len byte-offset)
                      (gpuarray-format data)
                    (declare (ignore len byte-offset))
                    (gl-type-size type))))))
    (%gl:bind-buffer-range :uniform-buffer (ubo-id ubo)
                           buffer-id offset size))
  ubo)
