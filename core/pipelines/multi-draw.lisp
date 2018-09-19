(in-package :cepl.pipelines)

(defstruct-g indirect-command
  (count :uint)
  (instance-count :uint)
  (first :uint)
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
