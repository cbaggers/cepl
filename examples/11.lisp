(defparameter *count* 0.0)

(cgl:defglstruct vert-data
  (position :vec4 :accessor pos)
  (colour :vec4 :accessor col)
  (tex-pos :vec2 :accessor tex-pos))

(cgl:defpipeline ripple-with-wobble
    ((vert vert-data) &uniform (tex :sampler-2d) (count :float)
     (pos-offset :vec4))
  (:vertex (setf gl-position (+ (pos vert) pos-offset))
           (out (the-color :smooth) (col vert))
           (out (tex-coord :smooth) (tex-pos vert)))
  (:fragment 
   (out outputColor
        (let* ((rip-size 0.02)
               (centre (vec2 0.0 0.25))
               (dif (- tex-coord centre))
               (dist (dot dif dif))
               (damp 0.6)
               (peaks 21.0)
               (height (sin (+ count (* peaks dist))))
               (rip-offset (* (* rip-size (normalize dif))
                              height damp)))
          (+ (texture tex (+ tex-coord rip-offset))
             (vec4 (* -0.2 height) (* -0.2 height) 0.0 0.0)
             (vec4 0.0 0.0 0.1 0.0))))))

(defun step-demo (gstream texture)  
  (ripple-with-wobble gstream :tex texture :count *count*)
  (incf *count* 0.05))

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
         (tex-data (make-c-array 
                    '(64 64) :ubyte-vec4 :initial-contents 
                    (loop :for i :below 64 :collect
                       (loop :for j :below 64 
                          :collect (v!ubyte 0 0 (random 254) 0))))))
    (cgl::upload-c-array-to-gpuarray-t (cgl::texref texture) 
                                       tex-data :rgba :byte)
    (loop :until (find :quit-event (sdl:collect-event-types)) :do
       (cepl-utils:update-swank)
       (base-macros:continuable
         (gl:clear :color-buffer-bit)
         (step-demo gstream texture)
         (gl:flush)
         (sdl:update-display)))))
