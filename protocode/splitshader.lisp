;; This is simply to get a colored triangle up on the screen

(defstruct-g vert-data
  (position :vec4 :accessor pos)
  (colour :vec4 :accessor col))

(defvshader vs ((vert vert-data))
  (setf gl-position (pos vert))
  (out (the-color :smooth) (col vert)))

(deffshader fs ((the-color :vec4 :smooth) &uniform (i :int))
  (let ((lerp-value (/ (y gl-frag-coord) 500.0)))
    (out outputColor (mix the-color
                          (vec4 0.2 0.2 0.2 1.0)
                          lerp-value))))

(defpipeline prog-1 ((vert vert-data) &uniform (i :int))
  vs fs)

(defun run-demo ()
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 1024 768)
  (let* ((data (make-gpu-array
                (list (list (v!  0.0    0.5 0.0 1.0) (v! 1.0 0.0 0.0 1.0))
                      (list (v!  0.5 -0.366 0.0 1.0) (v! 0.0 1.0 0.0 1.0))
                      (list (v! -0.5 -0.366 0.0 1.0) (v! 0.0 0.0 1.0 1.0)))
                :dimensions 3
                :element-type 'vert-data))
         (gstream (make-buffer-stream data)))
    (loop :until (find :quit-event (sdl:collect-event-types)) :do
       (cepl-utils:update-swank)
       (base-macros:continuable (progn (gl:clear :color-buffer-bit)
                                       (prog-1 gstream)
                                       (gl:flush)
                                       (sdl:update-display))))))
