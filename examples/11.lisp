;; textures...maybe

(cgl:defglstruct vert-data
  (position :vec4 :accessor pos)
  (colour :vec4 :accessor col)
  (tex-pos :vec2 :accessor tex-pos))

(cgl:defpipeline prog-1 ((vert vert-data) &uniform (tex :sampler-2d))
  (:vertex (setf gl-position (pos vert))
           (out (the-color :smooth) (col vert))
           (out (tex-coord :smooth) (tex-pos vert)))
  (:fragment (out outputColor (texture tex tex-coord))))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((data (cgl:make-gpu-array 
                (list (list (v!  0.0    0.5 0.0 1.0) (v! 1.0 0.0 0.0 1.0) 
                            (v! 0.0 -1.0))
                      (list (v!  0.5 -0.366 0.0 1.0) (v! 0.0 1.0 0.0 1.0)
                            (v! 1.0 1.0))
                      (list (v! -0.5 -0.366 0.0 1.0) (v! 0.0 0.0 1.0 1.0)
                            (v! -1.0 1.0)))
                :dimensions 3
                :element-type 'vert-data))
         (gstream (cgl:make-gpu-stream-from-gpu-arrays data))
         (texture (cgl::make-texture '(64 64)))
         (tex-data (make-c-array '(64 64) :byte-vec4 :initial-contents 
                                 (loop :for i :below 64 :collect
                                    (loop :for j :below 64 
                                       :collect (v! (random 254) 0 0 0))))))
    (cgl::upload-c-array-to-gpuarray-t (cgl::texref texture) tex-data :rgba :byte)
    (loop :until (find :quit-event (sdl:collect-event-types)) :do
       (cepl-utils:update-swank)
       (base-macros:continuable (progn (gl:clear :color-buffer-bit)
                                       (prog-1 gstream :tex texture)
                                       (gl:flush)
                                       (sdl:update-display))))))
