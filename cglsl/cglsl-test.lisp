(in-package :cglsl)

;; [TODO] - What happens if you have two bindings with same name in
;;          let* ?
;;        - Make a let
;;        - Make all math functions
;;        - How do we handle out values?

(defshader test ((position vec4) (color vec4) 
		 &uniforms (camera-to-clip-matrix mat4)
		 (world-to-camera-matrix mat4)
		 (model-to-world-matrix mat4))
  (let* (((temp vec4) (m4:mcol*vec4 model-to-world-matrix 
				    position)))
    (setf gl_Position (m4:mcol*vec4 
		       camera-to-clip-matrix 
		       (m4:mcol*vec4 world-to-camera-matrix
				     temp)))
    (setf interpColor color)))
