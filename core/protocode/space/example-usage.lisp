;;----------------------------------------------------------------------

(defun-g nm-vert ((data g-pnt) &uniform (model-to-cam :mat4) (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g nm-vert ((data g-pnt) &uniform (clip-space space-g))
  (values (in clip-space
	    (v! (pos data) 1.0))
	  (pos data)
	  (norm data)
	  (v! 0.4 0 0.4 0)
	  (tex data)))

;;----------------------------------------------------------------------

(defun-g light ((x :vec4) &uniform (pos :sampler-2d) (normals :sampler-2d)
		(clip-space space-g) (lpos :vec3))
  (in clip-space
    (light lpos (texture pos gl-frag-coord) (texture buf gl-frag-coord))))
