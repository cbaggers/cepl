;; Texturing and fragment effects

(defparameter *count* 0.0)
(defparameter *texture* nil)
(defparameter *vert-gpu* nil)
(defparameter *v-stream* nil)

(defstruct-g vert-data ()
  (position :vec4 :accessor pos)
  (tex-pos :vec2 :accessor tex-pos))

(defpipeline ripple-with-wobble ((vert vert-data) &uniform (tex :sampler-2d)
                                 (count :float) (pos-offset :vec4))
  (:vertex (setf gl-position (pos vert))
           (out (tex-coord :smooth) (tex-pos vert)))
  (:fragment
   (let* ((rip-size 0.02) (centre (v! 0.0 0.0)) (damp 0.6)
          (peaks 9.0)
          (dif (- tex-coord centre))
          (dist (dot dif dif))
          (height (/ (+ (sin (+ count (* dist peaks)))
                        (sin (- count (* (y tex-coord) peaks))))
                     2.0))
          (rip-offset (* (* (normalize dif) rip-size) height damp)))
     (out outputColor (+ (texture tex (+ rip-offset tex-coord))
                         (v! (* -0.2 height) (* -0.2 height) 0.0 0.0))))))

(defun step-demo ()
  (ripple-with-wobble *v-stream* :tex *texture* :count *count*
                      :pos-offset (v! 0 0 0 0))
  (incf *count* 0.01))

(let ((running nil))
  (defun run-demo ()
    (setf running t)
    (cgl:clear-color 0.0 0.0 0.0 0.0)
    (apply #'gl:viewport 0 0 cgl:+default-resolution+)
    (let* ((img-data (loop :for i :below 64 :collect
                        (loop :for j :below 64 :collect (random 254)))))
      (setf *vert-gpu*
            (make-gpu-array `((,(v! -0.5 -0.366 0.0 1.0) ,(v! -1.0 1.0))
                              (,(v!  0.5 -0.366 0.0 1.0) ,(v!  1.0 1.0))
                              (,(v!  0.0    0.5 0.0 1.0) ,(v!  0.0 -1.0)))
                            :dimensions 3 :element-type 'vert-data))
      (setf *v-stream* (make-buffer-stream *vert-gpu*))
      (setf *texture* (with-c-array
                          (temp (make-c-array img-data :dimensions '(64 64)
                                              :element-type :ubyte))
                        (make-texture temp)))
      (loop :while running :do
         (case-events (event) (:quit () (setf running nil)))
         (update-swank)
         (base-macros:continuable
           (cgl:clear :color-buffer-bit)
           (step-demo)
           (cgl:flush)
           (cgl:update-display)))))
  (defun stop-demo () (setf running nil)))
