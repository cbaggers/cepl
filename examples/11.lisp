;; Texturing and fragment effects

(defparameter *count* 0.0) 

(defglstruct vert-data
  (position :vec4 :accessor pos)
  (tex-pos :vec2 :accessor tex-pos))

(defpipeline ripple-with-wobble
    ((vert vert-data) &uniform (tex :sampler-2d) (count :float)
     (pos-offset :vec4))
  (:vertex (setf gl-position (+ (pos vert) pos-offset))
           (out (tex-coord :smooth) (tex-pos vert)))
  (:fragment 
   (let* ((rip-size 0.02) (centre (vec2 0.0 0.25)) (damp 0.6)
          (peaks 21.0)
          (dif (- tex-coord centre))
          (dist (dot dif dif))
          (height (sin (+ count (* peaks dist))))
          (rip-offset (* (* rip-size (normalize dif)) height damp)))
     (out outputColor (+ (texture tex (+ tex-coord rip-offset))
                         (vec4 (* -0.2 height) (* -0.2 height) 0.0 0.0))))))

(defun step-demo (gstream texture)  
  (ripple-with-wobble gstream :tex texture :count *count*)
  (incf *count* 0.05))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (cgl:viewport 0 0 640 480)
  (let* ((v-data (make-gpu-array `((,(v!  0.0    0.5 0.0 1.0) ,(v!  0.0 -1.0))
                                   (,(v!  0.5 -0.366 0.0 1.0) ,(v!  1.0 1.0))
                                   (,(v! -0.5 -0.366 0.0 1.0) ,(v! -1.0 1.0)))
                                 :dimensions 3 :element-type 'vert-data))
         (gstream (make-gpu-stream-from-gpu-arrays v-data))
         (img-data (loop :for i :below 64 :do 
                      (loop :for j :below 64 :collect (random 254))))
         (texture (with-c-array (temp '(64 64) :ubyte :initial-contents img-data)
                    (make-texture :initial-contents temp))))
    (loop :until (find :quit-event (sdl:collect-event-types)) :do
       (cepl-utils:update-swank)
       (base-macros:continuable
         (cgl:clear :color-buffer-bit)
         (step-demo gstream texture)
         (cgl:flush)
         (sdl:update-display)))))
