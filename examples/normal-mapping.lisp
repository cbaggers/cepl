;; fragment point light - unfinished
(in-package :cepl)
(named-readtables:in-readtable fn_:fn_lambda)

;;--------------------------------------------------------------
;; setup

(defparameter *wibble* nil)
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *tex* nil)
(defparameter *normal-map* nil)
(defparameter *loop-pos* 0.0)

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
  (let* ((result (first (model-parsers:load-file filename)))
         (mesh (make-instance 'cgl:mesh
                              :primitive-type :triangles
                              :vertices (first result)
                              :index (second result)))
         (mesh~1 (if hard-rotate
                     (cgl:transform-mesh mesh :rotation hard-rotate)
                     mesh)))
    (let ((gstream (make-buffer-stream
                    (cgl:vertices mesh) :index-array (cgl:indicies mesh))))
      (make-instance 'entity :rot (v! 1.57079633 1 0) :gstream gstream
                     :pos (v! 0 -0.3 -3) :mesh mesh~1))))

(defun init ()
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera cgl:+default-resolution+))
  (reshape cgl:+default-resolution+)
  (setf *wibble* (load-model "./wibble.3ds" (v! pi 0 0)))
  (setf *tex* (devil-helper:load-image-to-texture "./brick/col.png"))
  (setf *normal-map* (devil-helper:load-image-to-texture "./brick/norm.png")))

;;--------------------------------------------------------------
;; drawing

(defun-g nm-vert ((data g-pnt) &uniform (model-to-cam :mat4) (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (cgl:pos data) 1.0)))
          (pos data)
          (cgl:norm data)
          (v! 0.4 0 0.4 0)
          (cgl:tex data)))

(defun-g nm-frag ((model-space-pos :vec3) (vertex-normal :vec3)
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
         (t-col (texture textur tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(defpipeline frag-point-light () (g-> #'nm-vert #'nm-frag))
;;(:post-compile (reshape cgl:+default-resolution+))


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
    (map-g #'frag-point-light (gstream *wibble*)
          :model-space-light-pos (v:s~ cam-light-vec :xyz)
          :light-intensity (v! 1 1 1 0)
          :model-to-cam model-to-cam-matrix
          :norm-map *normal-map*
          :ambient-intensity (v! 0.2 0.2 0.2 1.0)
          :textur *tex*))
  (gl:flush)
  (cgl:update-display))

;;--------------------------------------------------------------
;; controls

(evt:observe (|mouse|)
  (when (and (typep e 'evt.sdl:mouse-motion)
             (eq (evt.sdl:button-state self :left) :down))
    (let ((d (evt.sdl:delta e)))
      (cond
        ((eq (evt.sdl:key-state |keyboard| :lshift) :down)
         (v3:incf (pos *wibble*)
                  (v! 0 0 (/ (v:y d) 100.0))))
        ((eq (evt.sdl:key-state |keyboard| :lctrl) :down)
         (v3:incf (pos *wibble*)
                  (v! 0 (/ (v:y d) -100.0) 0)))
        (t
         (v3:incf (rot *wibble*)
                  (v! (/ (v:y d) -100.0)
                      (/ (v:x d) -100.0)
                      0.0)))))))

;;--------------------------------------------------------------
;; window

(defun reshape (new-dimensions)
  (setf (frame-size *camera*) new-dimensions)
  (apply #'gl:viewport 0 0 new-dimensions)
  (frag-point-light nil :cam-to-clip (cam->clip *camera*)))

(observe (|window|) (when (eq (action e) :resized) (reshape (vec e))))

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
  (evt:observe (|sys|)
    (setf running (typep e 'evt.sdl:will-quit))))

(defun step-demo ()
  (evt.sdl:pump-events)
  (setf *loop-pos* (+ *loop-pos* 0.02))
  (setf (pos *light*) (v! (* 10 (sin *loop-pos*))
                          10
                          (* 10 (cos *loop-pos*))))
  (draw))
