(in-package :cgl)
(named-readtables:in-readtable fn_::fn_lambda)

(defparameter *gravity* (v! 0 -10 0))

(defstruct particle
  (life 0.0 :type single-float)
  (pos (v! 0 -100 0) :type (simple-array single-float (3)))
  (speed (v! 0 0 0) :type (simple-array single-float (3)) ))

(defclass emitter ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (v! 0 0 0) :accessor rot)
   (stepper :initform (make-stepper 100) :initarg :stepper :accessor stepper)
   (step-rate :initform 0 :initarg :step-rate :accessor step-rate)
   (emit-stepper :initform 0.1 :initarg :emit-stepper :accessor emit-stepper)
   (emit-rate :initform 0.1 :initarg :emit-rate :accessor emit-rate)
   (emit-angle :initform (radians 10.0) :accessor emit-angle)
   (start-speed-range :initform (v! 0.001 0.002) :initarg :start-speed-range
                      :accessor start-speed-range)
   (particle-lifespan :initform 0 :initarg :particle-lifespan
                      :accessor particle-lifespan)
   (max-particles :initform 400 :initarg :max-particles)
   (particles :initarg :particles :accessor particles)
   (particle-verts :initarg :particle-verts :accessor particle-verts)
   (particle-vert-stream :initarg :particle-vert-stream :accessor particle-vert-stream)
   (particle-pos-tex :initarg :particle-pos-tex :accessor particle-pos-tex)
   (texture :initarg :texture :accessor texture)))

(defun make-emitter (num-of-particles emit-rate/ms lifespan texture)
  (let ((particles
         (make-array
          num-of-particles :element-type 'particle
          :initial-contents (loop :for i :below num-of-particles :collect
                               (make-particle))))
        (particle-verts (make-gpu-array (loop :for i :below num-of-particles
                                           :append (list (v! 0.0 0.0 0 0)
                                                         (v! 1.0 0.0 0 0)
                                                         (v! 1.0 1.0 0 0)
                                                         (v! 1.0 1.0 0 0)
                                                         (v! 0.0 1.0 0 0)
                                                         (v! 0.0 0.0 0 0)))
                                        :element-type :vec4
                                        :dimensions (* num-of-particles 6)))
        (pos-tex (make-texture nil :dimensions num-of-particles
                               :internal-format :rgba32f
                               :buffer-storage t)))
    (make-instance
     'emitter
     :stepper (make-stepper 50)
     :step-rate 50
     :emit-stepper (make-stepper emit-rate/ms)
     :emit-rate emit-rate/ms
     :particle-lifespan (float lifespan)
     :max-particles num-of-particles
     :particle-verts particle-verts
     :particle-vert-stream (make-buffer-stream particle-verts)
     :particles particles
     :particle-pos-tex pos-tex
     :texture texture)))

;;------------------------------------------------------------

(defun make-particle-active (particle emitter)
  (setf (particle-life particle) (particle-lifespan emitter)
        (particle-pos particle) (pos emitter)
        (particle-speed particle)
        (let* ((angle (random (emit-angle emitter)))
               (es (start-speed-range emitter))
               (s (+ (aref es 0) (random (- (aref es 1) (aref es 0)))))
               (o (* (sin angle) s))
               (y (* (cos angle) s))
               (radial (random (radians 360.0)))
               (x (* o (sin radial)))
               (z (* o (cos radial))))
          (v! x y z))))

(defun update-particle (p time/ms)
  ;; (update-particle-speed p time/ms)
  (update-particle-position p time/ms)
  (update-particle-life p time/ms))

(defun particle-livep (p) (> (particle-life p) 0))

(defun update-particle-life (p time/ms) (decf (particle-life p) time/ms))

;; (defun update-particle-speed (p time/ms) (particle-speed p))

(defun update-particle-position (p time/ms)
  (v3:incf (particle-pos p) (v3:v* (particle-speed p) (float time/ms))))

(defun emit-particle (em)
  (let ((unused (find-dead-particle em)))
    (when unused (make-particle-active unused em))))

(defun find-dead-particle (em)
  (find-if λ(not (particle-livep %)) (particles em)))

(defun update-emitter (em)
  (with-slots (step-rate stepper emit-rate emit-stepper) em
    (loop :for emit = (funcall emit-stepper) :while emit :do
       (emit-particle em))
    (loop :for stepped = (funcall stepper) :while stepped :do
       (map nil λ(when (particle-livep %) (update-particle % step-rate))
            (particles em))
       (push-g (map 'list λ(v! (particle-pos %) 1) (particles em))
                (particle-pos-tex em)))))

;;------------------------------------------------------------
;; drawing

(defun draw-emmiter (em camera)
  (let* ((world-to-cam-matrix (world->cam camera))
         (model-to-cam-matrix (m4:m* world-to-cam-matrix
                                     (m4:translation (pos em)))))
    (gmap #'particle-renderer (particle-vert-stream em)
          :vpos (particle-pos-tex em) :tex (texture em)
          :m2c model-to-cam-matrix :cam-to-clip (cam->clip *camera*))))

(defpipeline particle-renderer ((data :vec4) &uniform (vpos :sampler-buffer)
                                (tex :sampler-2d) (m2c :mat4) (cam-to-clip :mat4))
  (:vertex (let ((p-id (int (floor (/ gl-vertex-id 6)))))
             (out tc (s~ data :xy))
             (setf gl-position (+ data
                                  (* cam-to-clip
                                     m2c
                                     (v! (s~ (texel-fetch vpos p-id) :xyz)
                                         1))))))
  (:fragment (let ((fcol (texture tex tc)))
               (out output-color (v! (s~ fcol :xyz) (* (z fcol) 0.18))))))
