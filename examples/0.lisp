;; This is simply to get a colored triangle up on the screen

(cgl:defglstruct vert-data
  (position :type :float :length 4)
  (colour :type :float :length 4))

(defun draw (program streams)
  (gl:clear :color-buffer-bit)
  (cgl:draw-streams program streams)
  (gl:flush)
  (sdl:update-display))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((program (apply #'cgl:make-program (cgl:load-shaders "1.vert" "1.frag")))
         (data '((#( 0.0    0.5 0.0 1.0) #( 1.0 0.0 0.0 1.0))
                 (#( 0.5 -0.366 0.0 1.0) #( 0.0 1.0 0.0 1.0))
                 (#(-0.5 -0.366 0.0 1.0) #( 0.0 0.0 1.0 1.0))))
         (streams `(,(cgl:make-gpu-stream-from-gpu-arrays
                      :length 3
                      :gpu-arrays (cgl:make-gpu-array
				   data :element-type 'vert-data)))))
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:w width :h height) (gl:viewport 0 0 width height))
      (:idle () (cepl-utils:update-swank)
                (base-macros:continuable (draw program streams))))))
