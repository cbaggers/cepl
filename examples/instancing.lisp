;; fragment point light - unfinished
(in-package :cepl)

;; NOTE: Ensure you have loaded cepl-image-helper & cepl-model-helper
;;       (or just load cepl-default)

;;--------------------------------------------------------------
;; setup

(defparameter *wibble* nil)
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *tex* nil)
(defparameter *loop-pos* 0.0)
(defparameter *pos-tex* nil)

(defclass entity ()
  ((gstream :initform nil :initarg :gstream :accessor gstream)
   (position :initform (v! 0 0 -1) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)
   (mesh :initarg :mesh :reader mesh)))

(defclass light ()
  ((position :initform (v! 20 20 -20) :initarg :pos :accessor pos)
   (radius :initform 1.0 :initarg :radius :accessor radius)))

(defun load-model (filename &optional hard-rotate)
  (let* ((result (second (model-parsers:load-file filename)))
         (mesh (make-instance 'meshes:mesh
                              :primitive-type :triangles
                              :vertices (first result)
                              :index (second result)))
         (mesh~1 (if hard-rotate
                     (meshes:transform-mesh mesh :rotation hard-rotate)
                     mesh)))
    (let ((gstream (make-buffer-stream
                    (meshes:vertices mesh) :index-array (meshes:indicies mesh))))
      (make-instance 'entity :rot (v! 1.57079633 1 0) :gstream gstream
                     :pos (v! 0 -0.4 -1) :mesh mesh~1))))

(defun init ()
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera *current-viewport*))
  (setf *wibble* (load-model "./bird/bird.3ds" (v! pi 0 0)))
  (setf *tex* (devil-helper:load-image-to-texture "./bird/char_bird_col.png"))
  (setf *pos-tex* (make-texture nil :dimensions 1000
                                :internal-format :rgba32f
                                :buffer-storage t))
  (push-g (loop :for i :below 1000 :collect
              (v! (- (random 20.0) 10) (- (random 20.0) 10)
                  (- -20 (random 10.0)) 1))
           *pos-tex*))

;;--------------------------------------------------------------
;; drawing

(defun-g instance-vert ((data g-pnt) &uniform (model-to-cam :mat4)
                        (cam-to-clip :mat4) (offsets :sampler-buffer))
  (values (let ((tpos (texel-fetch offsets gl-instance-id)))
            (+ (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
               tpos
               (v! 0 0 -4 7)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g instance-frag ((model-space-pos :vec3) (vertex-normal :vec3)
                  (diffuse-color :vec4) (tex-coord :vec2) &uniform
                  (model-space-light-pos :vec3) (light-intensity :vec4)
                  (ambient-intensity :vec4) (textur :sampler-2d)
                  (norm-map :sampler-2d))
  (let* ((light-dir (normalize (- model-space-light-pos
                                  model-space-pos)))
         (t-norm (- (* (s~ (texture norm-map tex-coord) :xyz) 2)
                    (v! 1 1 1)))
         (cos-ang-incidence
          (clamp (dot (normalize (* (+ vertex-normal t-norm) 0.5)) light-dir)
                 0.0 1.0))
         (t-col (texture textur (* (v! 1 -1) tex-coord))))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(defpipeline instanced-birds () (g-> #'instance-vert #'instance-frag)
  :post #'reshape)

(defun entity-matrix (entity)
  (reduce #'m4:m* (list (m4:translation (pos entity))
                        (m4:rotation-from-euler (rot entity))
                        (m4:scale (scale entity)))))

(defun draw ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((world-to-cam-matrix (world->cam *camera*))
         (model-to-cam-matrix (m4:m* world-to-cam-matrix
                                     (entity-matrix *wibble*)))
         ;;(normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
         (cam-light-vec (m4:mcol*vec4 (entity-matrix *wibble*)
                                      (v! (pos *light*) 1.0))))
    (with-instances 1000
      (map-g #'instanced-birds (gstream *wibble*)
             :model-space-light-pos (v:s~ cam-light-vec :xyz)
             :light-intensity (v! 1 1 1 0)
             :model-to-cam model-to-cam-matrix
             ;; :normal-model-to-cam normal-to-cam-matrix
             :ambient-intensity (v! 0.2 0.2 0.2 1.0)
             :textur *tex*
             :offsets *pos-tex*)))
  (update-display))

;;--------------------------------------------------------------
;; controls

(evt:observe (|mouse|)
  (when (typep e 'evt:mouse-motion)
    (let ((d (evt:delta e)))
      (setf (rot *wibble*) (v:+ (rot *wibble*) (v! (/ (v:y d) -100.0)
                                                   (/ (v:x d) -100.0)
                                                   0.0))))))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions *current-viewport*))
  (setf (frame-size *camera*) new-dimensions)
  (setf (viewport-resolution (viewport *gl-context*))
        new-dimensions)
  (instanced-birds nil :cam-to-clip (cam->clip *camera*)))

(observe (|window|) (when (eq (evt:action e) :resized) (reshape (data e))))

;;--------------------------------------------------------------
;; main loop

(let ((running nil))
  (defun run-loop ()
    (init)
    (setf running t)
    (loop :while running :do
       (continuable
         (step-demo)
         (update-swank))))
  (defun stop-loop () (setf running nil))
  (evt:observe (|sys|)
    (setf running (typep e 'evt:will-quit))))

(defun step-demo ()
  (evt:pump-events)
  (setf *loop-pos* (+ *loop-pos* 0.04))
  (setf (pos *light*) (v! (* 10 (sin *loop-pos*))
                          10
                          (* 10 (cos *loop-pos*))))
  (draw))
