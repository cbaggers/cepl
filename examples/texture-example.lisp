;; Texturing and fragment effects
(in-package :cepl)

(defparameter *count* 0.0)
(defparameter *texture* nil)
(defparameter *v-stream* nil)

(defun-g tex-vert ((vert g-pt))
  (values (v! (pos vert) 1)
          (:smooth (tex vert))))

(defun-g tex-frag ((tex-coord :vec2) &uniform (texture :sampler-2d)
                   (count :float) (pos-offset :vec4))
  (let* ((rip-size 0.02) (centre (v! 0.0 0.0)) (damp 0.6)
         (peaks 9.0)
         (dif (- tex-coord centre))
         (dist (dot dif dif))
         (height (/ (+ (cos (+ count (* dist peaks)))
                       (sin (- count (* (y tex-coord) peaks))))
                    2.0))
         (rip-offset (* (* (normalize dif) rip-size) height damp)))
    (+ (texture texture (+ rip-offset tex-coord))
       (v! (* -0.2 height) (* -0.2 height) 0.0 0.0))))

(defpipeline ripple-with-wobble () (g-> #'tex-vert #'tex-frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'ripple-with-wobble *v-stream*
        :texture *texture* :count *count* :pos-offset (v! 0 0 0 0))
  (incf *count* 0.08)
  (update-display))

(let ((running nil))
  (defun run-loop ()
    (setf running t)
    (let* ((img-data (loop :for i :below 64 :collect
                        (loop :for j :below 64 :collect (random 254)))))
      (setf *v-stream*
            (make-buffer-stream
             (make-gpu-array `((,(v! -0.5 -0.366 0) ,(v! -1  1))
                               (,(v!  0.5 -0.366 0) ,(v!  1  1))
                               (,(v!    0    0.5 0) ,(v!  0 -1)))
                             :dimensions 3 :element-type 'g-pt)
             :retain-arrays t))
      (setf *texture* (with-c-array
                          (temp (make-c-array img-data :dimensions '(64 64)
                                              :element-type :ubyte))
                        (make-texture temp)))
      (loop :while running :do (continuable (step-demo)))))
  (defun stop-loop () (setf running nil)))

(evt:def-event-listener sys-listener (e :sys)
  (when (typep e 'evt:will-quit) (stop-loop)))
