;; Texturing and fragment effects

(defparameter *count* 0.0) 
(defparameter *texture* nil)
(defparameter *vert-gpu* nil)
(defparameter *v-stream* nil)

(defglstruct vert-data
  (position :vec4 :accessor pos)
  (tex-pos :vec2 :accessor tex-pos))

(defpipeline ripple-with-wobble ((vert vert-data) &uniform (tex :sampler-2d)
                                 (count :float) (pos-offset :vec4))
  (:vertex (setf gl-position (+ (pos vert) pos-offset))
           (out (tex-coord :smooth) (tex-pos vert)))
  (:fragment 
   (let* ((rip-size 0.02) (centre (vec2 0.5 0.5)) (damp 0.6)
          (peaks 31.0)
          (dif (- tex-coord centre))
          (dist (dot dif dif))
          (height (/ (+ (sin (+ count (* peaks dist)))
                        (sin (- count (* peaks (y tex-coord)))))
                     2.0))
          (rip-offset (* (* rip-size (normalize dif)) height damp)))
     (out outputColor (+ (texture tex (+ rip-offset tex-coord) )
                         (vec4 (* -0.2 height) (* -0.2 height) 0.0 0.0))))))

(defun step-demo ()
  (ripple-with-wobble *v-stream* :tex *texture* :count *count*
                      :pos-offset (v! 0 0 0 0))
  (incf *count* 0.02))
(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (cgl:viewport 0 0 1024 768)
  (let* ((img-data (loop :for i :below 64 :collect
                      (loop :for j :below 64 :collect (random 254)))))
    (setf *vert-gpu* 
          (make-gpu-array `((,(v!  0.0    0.5 0.0 1.0) ,(v!  0.0 -1.0))
                            (,(v!  0.5 -0.366 0.0 1.0) ,(v!  1.0 1.0))
                            (,(v! -0.5 -0.366 0.0 1.0) ,(v! -1.0 1.0)))
                          :dimensions 3 :element-type 'vert-data))
    (setf *v-stream* (make-vertex-stream *vert-gpu*))
    (setf *texture* (with-c-array
                        (temp (make-c-array '(64 64) :ubyte 
                                            :initial-contents img-data))
                      (make-texture :initial-contents temp)))
    (loop :until (find :quit-event (sdl:collect-event-types)) :do
       (cepl-utils:update-swank)
       (base-macros:continuable
         (cgl:clear :color-buffer-bit)
         (step-demo)
         (cgl:flush)
         (sdl:update-display)))))

