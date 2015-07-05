(in-package :cgl)

(defmacro map-g (pipeline-func stream &rest uniforms)
  `(with-viewport (current-viewport)
     (%map-g ,pipeline-func ,stream ,@uniforms)))

(defmacro %map-g (pipeline-func stream &rest uniforms)
  (labels ((function-formp (x) (eq (first x) 'function)))
    `(progn
       (funcall ,pipeline-func ,+mapg-constant+ ,stream ,@uniforms)
       cgl::%current-fbo)))
