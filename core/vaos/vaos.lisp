(in-package :cepl.vaos)

;; [TODO] The terminology in here seems inconsistant, need to
;; nail this down

;;--------------------------------------------------------------
;; VAOS ;;
;;------;;

(defn free-vao ((vao gl-id)) (values)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (with-foreign-object (id :uint)
    (setf (mem-ref id :uint) vao)
    (%gl:delete-vertex-arrays 1 id)
    (values)))

;; [TODO] would a unboxed lisp array be faster?
(defun+ free-vaos (vaos)
  (with-foreign-object (ids :uint (length vaos))
    (loop :for vao :in vaos :for i :from 0 :do
       (setf (mem-aref ids :uint i) vao))
    (%gl:delete-vertex-arrays (length vaos) ids)
    (values)))

(defmacro with-vao-bound (vao &body body)
  (alexandria:with-gensyms (ctx vao-id old-vao)
    `(with-cepl-context (,ctx)
      (let* ((,old-vao (vao-bound ,ctx))
             (,vao-id ,vao))
         (release-unwind-protect
              (progn (setf (vao-bound ,ctx) ,vao-id)
                     (progn ,@body))
           (setf (vao-bound ,ctx) ,old-vao))))))

(defun+ suitable-array-for-index-p (array)
  (and (eql (length (gpu-buffer-arrays (gpu-array-buffer array))) 1)
       (1d-p array)
       (find (element-type array) '(:uint8 :ushort :uint :unsigned-short
                                    :unsigned-int))))

(defun+ preprocess-gpu-arrays-for-vao (gpu-arrays)
  (if (list-not-consp gpu-arrays)
      gpu-arrays
      (list gpu-arrays)))

(defun+ cons-aware-1d-p (x)
  (1d-p (if (consp x) (car x) x)))

(defn make-vao ((gpu-arrays list) &optional (index-array gpu-array-bb))
    gl-id
  (let ((gpu-arrays (preprocess-gpu-arrays-for-vao gpu-arrays)))
    (the gl-id
         (make-vao-from-id
          (progn (assert (and (every #'cons-aware-1d-p gpu-arrays)
                              (or (null index-array)
                                  (suitable-array-for-index-p
                                   index-array))))
                 (gl:gen-vertex-array))
          gpu-arrays index-array))))

(defn make-vao-from-id ((gl-object gl-id)
                        (gpu-arrays (or list gpu-array-bb))
                        &optional (index-array gpu-array-bb))
    gl-id
  (declare (profile t))
  (let ((gpu-arrays (preprocess-gpu-arrays-for-vao gpu-arrays)))
    (assert (and (every #'cons-aware-1d-p gpu-arrays)
                 (or (null index-array)
                     (suitable-array-for-index-p index-array)))
            () "You can only make VAOs from 1D arrays")
    (with-cepl-context (ctx)
      (setf (cepl.context:gpu-buffer-bound ctx :array-buffer) nil)
      (setf (cepl.context:gpu-buffer-bound ctx :element-array-buffer) nil)
      (let ((element-buffer (when index-array
                              (gpu-array-buffer index-array)))
            (vao gl-object)
            (attr 0))
        (with-vao-bound vao
          (loop :for gpu-array :in gpu-arrays :do
             (let* ((instance-divisor (when (consp gpu-array)
                                        (cdr gpu-array)))
                    (instance-divisor (if (eql instance-divisor 0)
                                          nil
                                          instance-divisor))
                    (gpu-array (if (consp gpu-array)
                                   (car gpu-array)
                                   gpu-array))
                    (buffer (gpu-array-buffer gpu-array))
                    (elem-type (gpu-array-bb-element-type gpu-array))
                    (offset (gpu-array-bb-offset-in-bytes-into-buffer gpu-array)))
               (assert (or (null instance-divisor) (> instance-divisor 0))
                       () "Instance divisor for ~a was ~a, however a positive integer was expected"
                       gpu-array instance-divisor)
               (setf (gpu-buffer-bound ctx :array-buffer) buffer)
               (release-unwind-protect
                    (incf attr
                          (gl-assign-attrib-pointers
                           (if (listp elem-type)
                               (second elem-type)
                               elem-type)
                           attr offset nil nil instance-divisor))
                 (setf (gpu-buffer-bound ctx :array-buffer) nil))))
          (when element-buffer
            (setf (gpu-buffer-bound ctx :element-array-buffer)
                  element-buffer)))
        vao))))
