;; fragment point light - unfinished
(in-package :cepl)
(named-readtables:in-readtable fn_::fn_lambda)

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
         (mesh (make-instance 'cgl::mesh
                              :primitive-type :triangles
                              :vertices (first result)
                              :index (second result)))
         (mesh~1 (if hard-rotate
                     (cgl::transform-mesh mesh :rotation hard-rotate)
                     mesh)))
    (let ((gstream (make-buffer-stream
                    (cgl::vertices mesh) :index-array (cgl::indicies mesh))))
      (make-instance 'entity :rot (v! 1.57079633 1 0) :gstream gstream
                     :pos (v! 0 -0.4 -1) :mesh mesh~1))))

(defun init ()
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera cgl:+default-resolution+))
  (reshape cgl:+default-resolution+)
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

(defpipeline frag-point-light
    ((data g-pnt) &uniform (model-to-cam :mat4)
     (cam-to-clip :mat4) (model-space-light-pos :vec3)
     (light-intensity :vec4) (ambient-intensity :vec4)
     (textur :sampler-2d) (offsets :sampler-buffer))
  (:vertex
   (let ((tpos (texel-fetch offsets gl-instance-id)))
     (setf gl-position
           (+ (* cam-to-clip (* model-to-cam (v! (cgl:pos data) 1.0)))
              tpos
              (v! 0 0 -4 7))))
   (out model-space-pos (cgl:pos data))
   (out vertex-normal (cgl:norm data))
   (out diffuse-color (v! 0.4 0 0.4 0))
   (out tex-coord (cgl:tex data)))
  (:fragment (let* ((light-dir (normalize (- model-space-light-pos
                                             model-space-pos)))
                    (cos-ang-incidence
                     (clamp (dot (normalize vertex-normal) light-dir)
                            0.0 1.0))
                    (t-col (texture textur (v! (x tex-coord)
                                            (- (y tex-coord))))))
               (out output-color (+ (* t-col light-intensity
                                       cos-ang-incidence)
                                    (* t-col ambient-intensity)))))
  (:post-compile (reshape cgl:+default-resolution+)))



(defun entity-matrix (entity)
  (reduce #'m4:m* (list (m4:translation (pos entity))
                        (m4:rotation-from-euler (rot entity))
                        (m4:scale (scale entity)))))

(defun draw ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((world-to-cam-matrix (world->cam *camera*))
         (model-to-cam-matrix (m4:m* world-to-cam-matrix
                                     (entity-matrix *wibble*)))
         ;;(normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
         (cam-light-vec (m4:mcol*vec4 (entity-matrix *wibble*)
                                      (v! (pos *light*) 1.0))))
    (with-instances (1000)
      (gmap #'frag-point-light (gstream *wibble*)
            :model-space-light-pos (v:s~ cam-light-vec :xyz)
            :light-intensity (v! 1 1 1 0)
            :model-to-cam model-to-cam-matrix
            ;; :normal-model-to-cam normal-to-cam-matrix
            :ambient-intensity (v! 0.2 0.2 0.2 1.0)
            :textur *tex*
            :offsets *pos-tex*)))
  (gl:flush)
  (cgl:update-display))

;;--------------------------------------------------------------
;; controls

(evt:observe (evt.sdl::*mouse*)
  (when (typep e 'evt.sdl:mouse-motion)
    (let ((d (evt.sdl:delta e)))
      (setf (rot *wibble*) (v:+ (rot *wibble*) (v! (/ (v:y d) -100.0)
                                                   (/ (v:x d) -100.0)
                                                   0.0))))))

;;--------------------------------------------------------------
;; window

(defun reshape (new-dimensions)
  (setf (frame-size *camera*) new-dimensions)
  (apply #'gl:viewport 0 0 new-dimensions)
  (frag-point-light nil :cam-to-clip (cam->clip *camera*)))

(evt:observe (evt.sdl::*window*)
  (when (eq (evt.sdl:action e) :resized)
    (reshape (evt.sdl:vec e))))

;;--------------------------------------------------------------
;; main loop

(let ((running nil))
  (defun run-demo ()
    (init)
    (setf running t)
    (loop :while running :do
       (continuable
         (step-demo)
         (update-swank))))
  (defun stop-demo () (setf running nil))
  (evt:observe (evt.sdl::*sys*)
    (setf running (typep e 'evt.sdl:will-quit))))

(defun step-demo ()
  (evt.sdl:pump-events)
  (setf *loop-pos* (+ *loop-pos* 0.04))
  (setf (pos *light*) (v! (* 10 (sin *loop-pos*))
                          10
                          (* 10 (cos *loop-pos*))))
  (draw))
