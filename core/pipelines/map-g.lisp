(in-package :cepl.pipelines)

(defmacro map-g (pipeline-func stream &rest uniforms)
  `(with-viewport (current-viewport)
     (%map-g ,pipeline-func ,stream ,@uniforms)))

(defmacro %map-g (pipeline-func stream &rest uniforms)
  (alexandria:with-gensyms (ctx)
    `(let ((,ctx *cepl-context*))
       (funcall ,pipeline-func ,+mapg-constant+ ,stream ,@uniforms)
       (draw-fbo-bound ,ctx))))

(defmacro map-g-into (fbo pipeline-func stream &rest uniforms)
  `(with-fbo-bound (,fbo)
     (map-g ,pipeline-func ,stream ,@uniforms)))

(defmacro map-g-into* ((fbo &key (target :framebuffer) (with-viewport t)
			    (attachment-for-size 0) (with-blending t))
			      pipeline-func stream &rest uniforms)
  `(with-fbo-bound (,fbo :target ,target
			 :unbind t
			 :attachment-for-size ,attachment-for-size
			 :with-viewport ,with-viewport
			 :with-blending ,with-blending
			 :draw-buffers t)
     (clear)
     (map-g ,pipeline-func ,stream ,@uniforms)))
