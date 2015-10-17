(in-package :cepl)
;; This gives us a simple moving triangle

(defparameter *vertex-stream* nil)
(defparameter *array* nil)
(defparameter *loop* 0.0)

;; note the use of implicit uniform capture with *loop*
;; special vars in scope can be used inline. During compilation
;; cepl will try work out the type. It does this by seeing if the
;; symbol is bound to a value, and if it is it checks the type of
;; the value for a suitable matching varjo type
(defun-g calc-pos ((v-pos :vec4) (id :int))
  (let ((pos (v! (* (s~ v-pos :xyz) 0.3) 1.0)))
    (+ pos (let ((i (/ (+ (float id)) 2)))
             (v! (sin (+ i *loop*))
                 (cos (* 3 (+ (tan i) *loop*)))
                 0.0 0.0)))))

(defun-g vert ((position :vec4) &uniform (i :int))
  (calc-pos position i))

(defun-g frag ()
  (v! (cos *loop*) (sin *loop*) 0.4 1.0))

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (setf *loop* (+ 0.004 *loop*))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (ttm:update)
  (loop :for i :below 100 :do
     (let ((i (/ i 2.0)))
       (map-g #'prog-1 *vertex-stream* :i i)))
  (update-display))

(let ((running nil))
  (defun run-loop ()
    (setf running t)
    (setf *array* (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                        (v! -0.2  -0.2  0.0  1.0)
                                        (v!  0.2  -0.2  0.0  1.0))
                                  :element-type :vec4
                                  :dimensions 3))
    (setf *vertex-stream* (make-buffer-stream *array*))
    (loop :while running :do (continuable (step-demo))))
  (defun stop-loop () (setf running nil)))

(evt:def-event-listener sys-listener (e :sys)
  (when (typep e 'evt:will-quit) (stop-loop)))
