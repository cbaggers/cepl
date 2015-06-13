;; Raymarcher!

(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *loop* 0.0)

(defmacro-g density-normal (object-call position-arg-num)
  (when (not (listp object-call)) (error "object-call form must a list"))
  (let ((p (1+ position-arg-num))
        (oc object-call))
    `(normalize
      (v! (- ,(utils:replace-nth oc p `(+ ,(nth p oc) (v! 0.01  0.0  0.0)))
             ,(utils:replace-nth oc p `(- ,(nth p oc) (v! 0.01  0.0  0.0))))
          (- ,(utils:replace-nth oc p `(+ ,(nth p oc) (v!  0.0 0.01  0.0)))
             ,(utils:replace-nth oc p `(- ,(nth p oc) (v!  0.0 0.01  0.0))))
          (- ,(utils:replace-nth oc p `(+ ,(nth p oc) (v!  0.0  0.0 0.01)))
             ,(utils:replace-nth oc p `(- ,(nth p oc) (v!  0.0  0.0 0.01))))))))

(defun-g sphere ((p :vec3) (r :float))
  (- (length p) r))

(defun-g box ((p :vec3) (b :vec3))
  (let ((d (- (abs p) b)))
    (+ (min (max (x d) (max (y d) (z d))) 0.0)
       (length (max d 0.0)))))

(defun-g thing2 ((p :vec3) (r :float) (l :float))
  (+ (* 0.2 (+ (y p)
               (cos (+ (* (+ (cos l) 8.0)
                          (* 2 (x p))) l))
               (* 3 (sin (* 3 l)))))
     (- (length p) r)))

(defun-g ray-vert ((position :vec4))
  (values position
          (swizzle position :xy)))

(defun-g ray-frag ((posxy :vec2) &uniform (loop :float) (eye-pos :vec3))
  (let* ((eye-pos (v! 0.0 0.0 -5.0))
         (eye-dir (normalize (v! (x posxy) (y posxy) 1.0)))
         (e eye-pos)
         (output (v! 0.0 0.0 0.0)))
    (for (i 0) (< i 20) (++ i)
         (let ((d (thing2 e 1.4 loop)))
           (if (<= d 0.0)
               (let ((norm (density-normal (thing2 e 1.4 loop) 0)))
                 (setf output (v! (+ 0.3 (* 0.5 (y norm)))
                                  0.0
                                  (+ 0.5 (* 0.2 (mix (y norm) (x norm)
                                                     (sin (* 1.0 loop))))) ))
                 (break)))
           (setf d (max d 0.01))
           (setf e (+ e (* eye-dir d)))))
    (v! output 1.0)))

(defpipeline raymarcher () (g-> #'ray-vert #'ray-frag))

(let ((running nil))
  (defun run-loop ()
    (setf *gpu-array* (make-gpu-array (list (v! -1.0  -1.0  0.0  1.0)
                                            (v!  1.0  -1.0  0.0  1.0)
                                            (v!  1.0   1.0  0.0  1.0)
                                            (v!  1.0   1.0  0.0  1.0)
                                            (v! -1.0   1.0  0.0  1.0)
                                            (v! -1.0  -1.0  0.0  1.0))
                                      :element-type :vec4
                                      :dimensions 6))
    (setf *vertex-stream* (make-buffer-stream *gpu-array*))
    (setf running t)
    (loop :while running :do (continuable (draw))))
  (defun stop-loop () (setf running nil)))

(evt:observe (e |sys|) (when (typep e 'evt:will-quit) (stop-loop)))

(defun draw ()
  (evt:pump-events)
  (update-swank)
  (setf *loop* (+ 0.01 *loop*))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'raymarcher *vertex-stream* :loop *loop* :eye-pos (v! 0 0 -5))
  (update-display))
