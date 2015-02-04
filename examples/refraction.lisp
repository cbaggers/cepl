;; Simple refraction example
(in-package :cepl)
(named-readtables:in-readtable fn_::fn_lambda)

;;--------------------------------------------------------------
;; setup

(defparameter *bird* nil)
(defparameter *wibble* nil)
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *bird-tex* nil)
(defparameter *bird-tex2* nil)
(defparameter *wib-tex* nil)
(defparameter *loop-pos* 0.0)
(defparameter *geom-fbo* nil)

(defclass entity ()
  ((gstream :initform nil :initarg :gstream :accessor gstream)
   (position :initform (v! 0 0 -1) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)
   (mesh :initarg :mesh :reader mesh)))

(defclass light ()
  ((position :initform (v! 20 20 -20) :initarg :pos :accessor pos)
   (radius :initform 1.0 :initarg :radius :accessor radius)))

(defun load-model (file-path nth-mesh &optional hard-rotate)
  (let* ((imp-mesh (elt (classimp:meshes (classimp:import-into-lisp file-path))
                        nth-mesh))
         (result (model-parsers:mesh->gpu imp-mesh))
         (mesh (make-instance 'cgl::mesh
                              :primitive-type :triangles
                              :vertices (first result)
                              :index (second result)))
         (mesh~1 (if hard-rotate
                     (cgl::transform-mesh mesh :rotation hard-rotate)
                     mesh)))
    (let ((gstream (make-vertex-stream
                    (cgl::vertices mesh) :index-array (cgl::indicies mesh))))
      (make-instance 'entity :rot (v! 1.57079633 1 0) :gstream gstream
                     :pos (v! 0 -0.4 -1) :mesh mesh~1))))

(defun init ()
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera cgl:+default-resolution+))
  (reshape cgl:+default-resolution+)
  (setf *wibble* (load-model "./wibble.3ds" 0 (v! pi 0 0)))
  (setf (v:z (pos *wibble*)) -3.0)
  (setf *bird* (load-model "./bird/bird.3ds" 1 (v! pi 0 0)))
  (setf *bird-tex* (devil-helper:load-image-to-texture "./water.jpg"))
  (setf *bird-tex2* (devil-helper:load-image-to-texture "./bird/char_bird_col.png"))
  (setf *wib-tex* (devil-helper:load-image-to-texture "./brick/col.png"))
  (setf *geom-fbo* (make-fbo :c)))

;;--------------------------------------------------------------
;; drawing

(defpipeline first-pass
    ((data g-pnt) &uniform (model-to-cam :mat4)
     (cam-to-clip :mat4) (model-space-light-pos :vec3)
     (light-intensity :vec4) (ambient-intensity :vec4)
     (textur :sampler-2d))
  (:vertex (setf gl-position
                 (* cam-to-clip (* model-to-cam (v! (cgl:pos data) 1.0))))
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

(defpipeline hmm-pass
    ((data g-pnt) &uniform (model-to-cam :mat4)
     (cam-to-clip :mat4) (model-space-light-pos :vec3)
     (light-intensity :vec4) (ambient-intensity :vec4)
     (textur :sampler-2d) (bird-tex :sampler-2d) (fbo-tex :sampler-2d)
     (loop :float))
  (:vertex (setf gl-position
                 (* cam-to-clip (* model-to-cam (v! (cgl:pos data) 1.0))))
           (out tex-coord (cgl:tex data)))
  (:fragment (let* ((o (v! (mod (* loop 0.05) 1.0)
                           (mod (* loop 0.05) 1.0)))
                    (ot (* (s~ (texture textur (+ o tex-coord)) :xy) 0.1))
                    (a (texture textur tex-coord))
                    (b (+ (v! (* (x gl-frag-coord) (/ 1.0 640.0))
                              (* (y gl-frag-coord) (/ 1.0 480.0)))
                          (* (s~ a :xy) 0.020)
                          ot))
                    (c (texture fbo-tex b))
                    (r (* (texture bird-tex (* (v! 1 -1) tex-coord)) 0.1))
                    (f (+ r c)))
               (out output-color f)))
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
    (gmap #'first-pass (gstream *wibble*)
          :model-space-light-pos (v:s~ cam-light-vec :xyz)
          :light-intensity (v! 1 1 1 0)
          :model-to-cam model-to-cam-matrix
          ;; :normal-model-to-cam normal-to-cam-matrix
          :ambient-intensity (v! 0.3 0.3 0.3 1.0)
          :textur *wib-tex*))
  (let ((fbo
         (with-bind-fbo (*geom-fbo*)
           (gl:clear :color-buffer-bit :depth-buffer-bit)
           (let* ((world-to-cam-matrix (world->cam *camera*))
                  (model-to-cam-matrix (m4:m* world-to-cam-matrix
                                              (entity-matrix *wibble*)))
                  ;;(normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
                  (cam-light-vec (m4:mcol*vec4 (entity-matrix *wibble*)
                                               (v! (pos *light*) 1.0))))
             (gmap #'first-pass (gstream *wibble*)
                   :model-space-light-pos (v:s~ cam-light-vec :xyz)
                   :light-intensity (v! 1 1 1 0)
                   :model-to-cam model-to-cam-matrix
                   ;; :normal-model-to-cam normal-to-cam-matrix
                   :ambient-intensity (v! 0.2 0.2 0.2 1.0)
                   :textur *wib-tex*)))))
    (let* ((world-to-cam-matrix (world->cam *camera*))
           (model-to-cam-matrix (m4:m* world-to-cam-matrix
                                       (entity-matrix *bird*)))
           ;;(normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
           (cam-light-vec (m4:mcol*vec4 (entity-matrix *bird*)
                                        (v! (pos *light*) 1.0))))
      (with-fbo-slots (c) fbo
          (gmap #'hmm-pass (gstream *bird*)
                :model-space-light-pos (v:s~ cam-light-vec :xyz)
                :light-intensity (v! 1 1 1 0)
                :model-to-cam model-to-cam-matrix
                ;; :normal-model-to-cam normal-to-cam-matrix
                :ambient-intensity (v! 0.2 0.2 0.2 1.0)
                :textur *bird-tex* :fbo-tex (slot-value c 'cgl::texture)
                :bird-tex *bird-tex2*
                :loop *loop-pos*))))
  (gl:flush)
  (cgl:update-display))

;;--------------------------------------------------------------
;; controls

(evt:observe (evt.sdl:*mouse*)
  (when (typep e 'evt.sdl:mouse-motion)
    (when (eq (evt.sdl:button-state evt.sdl::*mouse* :left) :down)
      (let ((d (evt.sdl:delta e)))
        (cond
          ((eq (evt.sdl:key-state evt.sdl:*keyboard* :lctrl) :down)
           (v3:incf (pos *bird*) (v! (/ (v:x d) 480.0)
                                     (/ (v:y d) -640.0)
                                     0)))
          ((eq (evt.sdl:key-state evt.sdl:*keyboard* :lshift) :down)
           (v3:incf (pos *bird*) (v! 0 0 (/ (v:y d) 300.0))))
          (t
           (setf (rot *bird*) (v:+ (rot *bird*) (v! (/ (v:y d) -100.0)
                                                    (/ (v:x d) -100.0)
                                                    0.0)))))
        ))))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions cgl:+default-resolution+))
  (setf (frame-size *camera*) new-dimensions)
  (apply #'gl:viewport 0 0 new-dimensions)
  (first-pass nil :cam-to-clip (cam->clip *camera*))
  (hmm-pass nil :cam-to-clip (cam->clip *camera*)))

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
  (setf (pos *light*) (v! (* 10 (sin (* 0.01 *loop-pos*)))
                          10
                          (* 10 (cos (* 0.01 *loop-pos*)))))
  (draw))
