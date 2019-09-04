(in-package :cepl.pipelines)

(defstruct-g (arrays-indirect-command :layout std-430)
  (count :uint)
  (instance-count :uint)
  (first :uint)
  (base-instance :uint))

(defstruct-g (elements-indirect-command :layout std-430)
  (count :uint)
  (instance-count :uint)
  (first-index :uint)
  (base-vertex :uint)
  (base-instance :uint))

(defmacro multi-map-g (pipeline-func
                       draw-command-array stream
                       &rest uniforms)
  (alexandria:with-gensyms (mapg-ctx)
    `(locally (declare (optimize (speed 3) (safety 1) (debug 1)
                                 (compilation-speed 0)))
       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (with-cepl-context (,mapg-ctx)
         (funcall ,pipeline-func ,mapg-ctx ,stream ,draw-command-array
                  ,@uniforms)))))

(defn set-draw-command ((c-array c-array)
                        (index c-array-index)
                        (stream buffer-stream)
                        &optional
                        (instance-count c-array-index)
                        (base-instance c-array-index))
    c-array
  (with-cepl-context (ctx)
    (let ((instance-count
           (or instance-count
               (cepl.context::%cepl-context-instance-count ctx)))
          (base-instance
           (or base-instance 0)))
      (if (= (buffer-stream-%index-type-enum stream) 0)
          (let ((elem (aref-c c-array index)))
            (setf (arrays-indirect-command-count elem)
                  (buffer-stream-length stream))
            (setf (arrays-indirect-command-instance-count elem)
                  instance-count)
            (setf (arrays-indirect-command-first elem)
                  (buffer-stream-start stream))
            (setf (arrays-indirect-command-base-instance elem)
                  base-instance))
          (let ((elem (aref-c c-array index)))
            (setf (elements-indirect-command-count elem)
                  (buffer-stream-length stream))
            (setf (elements-indirect-command-instance-count elem)
                  instance-count)
            (setf (elements-indirect-command-first-index elem)
                  (buffer-stream-start stream))
            (setf (elements-indirect-command-base-vertex elem)
                  (buffer-stream-base-vertex stream))
            (setf (elements-indirect-command-base-instance elem)
                  base-instance)))))
  c-array)

(defn make-draw-command-c-array ((stream buffer-stream)
                                 (length c-array-index))
    c-array
  (let ((element-type
         (if (= (buffer-stream-%index-type-enum stream) 0)
             'arrays-indirect-command
             'elements-indirect-command)))
    (make-c-array nil :dimensions length
                  :element-type element-type)))

(defun make-draw-command-gpu-array (stream length)
  (check-type stream buffer-stream)
  (check-type length c-array-index)
  (let ((element-type
         (if (= (buffer-stream-%index-type-enum stream) 0)
             'arrays-indirect-command
             'elements-indirect-command)))
    (make-gpu-array nil :dimensions length
                    :element-type element-type)))
