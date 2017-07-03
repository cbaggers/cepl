(in-package :cepl.pipelines)

(defmacro map-g (pipeline-func stream &rest uniforms)
  ;; the map-g context has to be a gensym containing the name
  ;; MAPG-CTX this is a hacky but reliable way to check if the
  ;; pipeline is being called from map-g. Of course someone can
  ;; go out of their way to trick us but at that point they are
  ;; already playing with fire.
  ;;
  ;; If you are reading this trying to work out how to call your
  ;; pipeline without map-g, DONT! this is one of the things that
  ;; allows cepl to perform a bunch of optimizations and change
  ;; api internals without breaking people's code.
  ;; If you have a usecase where map-g is a problem please raise an
  ;; issue on github
  ;;
  ;; see also #'mapg-context-p
  (alexandria:with-gensyms (mapg-ctx)
    `(locally (declare (optimize (speed 3) (safety 1) (debug 0)
                                 (compilation-speed 0)))
       (with-cepl-context (,mapg-ctx)
         (with-viewport (current-viewport)
           (funcall ,pipeline-func ,mapg-ctx ,stream ,@uniforms)
           (draw-fbo-bound ,mapg-ctx))))))

(defmacro map-g-into (fbo pipeline-func stream &rest uniforms)
  `(with-fbo-bound (,fbo :target :draw-framebuffer)
     (map-g ,pipeline-func ,stream ,@uniforms)))

(defmacro map-g-into* ((fbo &key (with-viewport t)
                            (attachment-for-size 0) (with-blending t))
                              pipeline-func stream &rest uniforms)
  `(with-fbo-bound (,fbo :target :draw-framebuffer
                         :unbind t
                         :attachment-for-size ,attachment-for-size
                         :with-viewport ,with-viewport
                         :with-blending ,with-blending
                         :draw-buffers t)
     (map-g ,pipeline-func ,stream ,@uniforms)))
