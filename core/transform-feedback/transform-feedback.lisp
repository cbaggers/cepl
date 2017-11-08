(in-package :cepl.transform-feedback)

(defun make-transform-feedback-stream (buffer-backed-gpu-array)
  (assert (typep buffer-backed-gpu-array 'gpu-array-bb)
          (buffer-backed-gpu-array)
          "{TODO} foo ~a" buffer-backed-gpu-array)
  (%make-tfs :array buffer-backed-gpu-array))

(defmacro with-transform-feedback ((transform-feedback-stream) &body body)
  (alexandria:with-gensyms (ctx old-tfs new-tfs)
    `(with-cepl-context (,ctx)
       (%with-cepl-context-slots (default-framebuffer current-tfs)
           ,ctx
         (assert (not current-tfs) () 'nested-with-transform-feedback)
         (let ((,new-tfs ,transform-feedback-stream)
               (,old-tfs current-tfs))
           (setf current-tfs ,new-tfs)
           (setf (%tfs-bound ,new-tfs) t)
           (%bind-tfs-array ,ctx ,new-tfs)
           (release-unwind-protect (progn ,@body)
             (when (/= (%tfs-current-prog-id ,new-tfs) +unknown-gl-id+)
               (setf (%tfs-current-prog-id ,new-tfs) +unknown-gl-id+)
               (%gl:end-transform-feedback))
             (%unbind-tfs-array ,ctx)
             (setf (%tfs-bound ,new-tfs) nil)
             (setf current-tfs ,old-tfs)))))))

(defn %bind-tfs-array ((ctx cepl-context)
                       (tfs transform-feedback-stream))
    (values)
  (let* ((data (%tfs-array tfs))
         (type (gpu-array-bb-element-type data))
         (offset (+ (gpu-array-bb-offset-in-bytes-into-buffer data)
                    (cepl.c-arrays::gl-calc-byte-size
                     type 0)))
         (size (gpu-array-bb-byte-size data))
         (gpu-buffer (gpu-array-buffer data)))
    (cepl.context::transform-feedback-bind-buffer-id-range
     ctx
     (gpu-buffer-id gpu-buffer)
     0 ;; {TODO} big ol' todo, gotta research this
     offset
     size))
  (values))

(defn %unbind-tfs-array ((ctx cepl-context))
    (values)
  (cepl.context::transform-feedback-bind-buffer-id-range
   ctx
   +unknown-gl-id+
   0 ;; {TODO} big ol' todo, gotta research this
   0
   0)
  (values))

(defn-inline transform-feedback-stream-array ((tfs transform-feedback-stream))
    (or null gpu-array-bb)
  (%tfs-array tfs))

(defmethod print-object ((tfs transform-feedback-stream) stream)
  (format stream "#<TRANSFORM-FEEDBACK-STREAM ~a>" (%tfs-array tfs)))
