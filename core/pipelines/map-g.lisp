(in-package :cepl.pipelines)

(defmacro map-g (pipeline-func stream &rest uniforms)
  `(with-viewport (current-viewport)
     (%map-g ,pipeline-func ,stream ,@uniforms)))

(defmacro %map-g (pipeline-func stream &rest uniforms)
  (labels ((function-formp (x) (eq (first x) 'function)))
    `(progn
       (funcall ,pipeline-func ,+mapg-constant+ ,stream ,@uniforms)
       %current-fbo)))

(defmacro map-g-into (fbo pipeline-func stream &rest uniforms)
  `(with-fbo-bound (,fbo)
     (map-g ,pipeline-func ,stream ,@uniforms)))

(defmacro map-g-into* ((fbo &key (target :framebuffer) (unbind t)
			    (with-viewport t) (attachment-for-size 0)
			    (with-blending t) (draw-buffers t))
			      pipeline-func stream &rest uniforms)
  `(with-fbo-bound (,fbo :target target
			 :unbind unbind
			 :with-viewport with-viewport
			 :with-blending with-blending
			 :draw-buffers draw-buffers)
     (map-g ,pipeline-func ,stream ,@uniforms)))
