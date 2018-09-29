(in-package :cepl.transform-feedback)

(defun make-transform-feedback-stream (&rest buffer-backed-gpu-arrays)
  (assert (every (lambda (arr) (typep arr 'gpu-array-bb))
                 buffer-backed-gpu-arrays)
          (buffer-backed-gpu-arrays) 'invalid-args-in-make-tfs
          :args buffer-backed-gpu-arrays)
  (assert (every (lambda (arr)
                   (gpu-array-bb-element-type arr)
                   (= (mod (gpu-array-bb-byte-size arr) 4)
                      0))
                 buffer-backed-gpu-arrays)
          (buffer-backed-gpu-arrays) 'invalid-sizes-in-make-tfs
          :args buffer-backed-gpu-arrays)
  (let ((arrs (make-array (length buffer-backed-gpu-arrays)
                          :element-type 'gpu-array-bb
                          :initial-contents buffer-backed-gpu-arrays)))
    (%make-tfs :arrays arrs)))

(defmacro with-transform-feedback ((transform-feedback-stream) &body body)
  (alexandria:with-gensyms (ctx old-tfs new-tfs)
    `(with-cepl-context (,ctx)
       (%with-cepl-context-slots (default-framebuffer current-tfs)
           ,ctx
         (assert (not current-tfs) () 'nested-with-transform-feedback)
         (let ((,new-tfs ,transform-feedback-stream)
               (,old-tfs current-tfs))
           (%bind-tfs-arrays ,ctx ,new-tfs)
           (setf current-tfs ,new-tfs)
           (setf (%tfs-bound ,new-tfs) t)
           (release-unwind-protect (progn ,@body)
             (when (/= (%tfs-current-prog-id ,new-tfs) +unknown-gl-id+)
               (setf (%tfs-current-prog-id ,new-tfs) +unknown-gl-id+)
               (%gl:end-transform-feedback))
             (%unbind-tfs-arrays ,ctx ,new-tfs)
             (setf (%tfs-bound ,new-tfs) nil)
             (setf current-tfs ,old-tfs)))))))

(defn %bind-tfs-arrays ((ctx cepl-context)
                        (tfs transform-feedback-stream))
    (values)
  (loop :for array :across (%tfs-arrays tfs)
     :for index :from 0 :do
     (let* ((offset (gpu-array-bb-offset-in-bytes-into-buffer array))
            (size (gpu-array-bb-byte-size array))
            (gpu-buffer (gpu-array-buffer array)))
       (cepl.context::transform-feedback-bind-buffer-id-range
        ctx
        (gpu-buffer-id gpu-buffer)
        index
        offset
        size)))
  (values))

(defn %unbind-tfs-arrays ((ctx cepl-context)
                          (tfs transform-feedback-stream))
    (values)
  (loop :for index :below (length (%tfs-arrays tfs)) :do
     (cepl.context::transform-feedback-bind-buffer-id-range
      ctx
      +unknown-gl-id+
      index
      0
      0))
  (when (%tfs-pending-arrays tfs)
    (setf (%tfs-arrays tfs) (%tfs-pending-arrays tfs))
    (setf (%tfs-pending-arrays tfs) nil))
  (values))

(defn-inline transform-feedback-stream-arrays ((tfs transform-feedback-stream))
    list
  (coerce (%tfs-arrays tfs) 'list))

(defn (setf transform-feedback-stream-arrays)
    ((buffer-backed-gpu-arrays list) (tfs transform-feedback-stream))
    list
  (assert (every (lambda (arr) (typep arr 'gpu-array-bb))
                 buffer-backed-gpu-arrays)
          (buffer-backed-gpu-arrays) 'invalid-args-in-make-tfs
          :args buffer-backed-gpu-arrays)
  (let ((arrs (make-array (length buffer-backed-gpu-arrays)
                              :element-type 'gpu-array-bb
                              :initial-contents buffer-backed-gpu-arrays)))
    (if (%tfs-bound tfs)
        (progn
          (warn 'tfs-setf-arrays-whilst-bound)
          (setf (%tfs-pending-arrays tfs) arrs))
        (setf (%tfs-arrays tfs) arrs))
    buffer-backed-gpu-arrays))

(defmethod print-object ((tfs transform-feedback-stream) stream)
  (format stream "#<TRANSFORM-FEEDBACK-STREAM~{ ~s~}>"
          (loop :for arr :across (%tfs-arrays tfs) :collect
             (let* ((dim (gpu-array-dimensions arr))
                    (dim (if (= (length dim) 1)
                             (first dim)
                             dim)))
               (list (gpu-array-bb-element-type arr)
                     dim)))))
