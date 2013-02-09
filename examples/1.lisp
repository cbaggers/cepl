;; This is simply to get a colored triangle up on the screen

(cgl:defglstruct vert-data
  (position :vec4)
  (colour :vec4))

(cgl:defprogram prog-1 ((vert vert-data))
  (:vertex (setf gl-position (vert-data-position vert))
           (out (the-color :smooth) (vert-data-colour vert)))
  (:fragment (let ((lerp-value (/ (y gl-frag-coord) 500.0)))
               (out outputColor (mix the-color 
                                     (vec4 0.2 0.2 0.2 1.0)
                                     lerp-value)))))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((data `((,(v!  0.0    0.5 0.0 1.0) ,(v! 1.0 0.0 0.0 1.0))
                 (,(v!  0.5 -0.366 0.0 1.0) ,(v! 0.0 1.0 0.0 1.0))
                 (,(v! -0.5 -0.366 0.0 1.0) ,(v! 0.0 0.0 1.0 1.0))))
         (gstream (cgl:make-gpu-stream-from-gpu-arrays
                   :gpu-arrays (cgl:make-gpu-array 
                                data :element-type 'vert-data))))
    (do-until (find :quit-event (collect-sdl-event-types))
      (cepl-utils:update-swank)
      (base-macros:continuable (progn (gl:clear :color-buffer-bit)
				      (prog-1 gstream)
				      (gl:flush)
				      (sdl:update-display))))))
