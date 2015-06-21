(in-package :cepl)

;;- - - - - - - - - - - - - - - - - -

(defvar box-data nil)
(defvar box-index nil)
(defvar box-stream nil)
(defvar brick nil)

(defstruct box
  (pos (v! 0 0 -10))
  (rot (q:identity-quat)))

(defvar box-a (make-box :pos (v! 0 0 -5)))
(defvar box-b (make-box :pos (v! 0.3 0 -3)))

;;- - - - - - - - - - - - - - - - - -

(defun model->world (x)
  (m4:m* (m4:translation (box-pos x)) (q:to-matrix4 (box-rot x))))

(defun world->clip (c)
  (m4:m* (cam->clip c) (world->cam c)))

(defun model->clip (m c)
  (m4:m* (world->clip c) (model->world m)))

;;- - - - - - - - - - - - - - - - - -

(defvar bp (make-blending-params))
(defvar camera (make-camera))
(defvar factor 0)

(defun-g box-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (norm vert)
          (tex vert)))

(defun-g box-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (f :float))
  (v! (s~ (texture tex tc) :xyz) f))

(defpipeline draw-box () (g-> #'box-vert #'box-frag))

;;- - - - - - - - - - - - - - - - - -

(defun step-demo ()
  (incf factor 0.02)
  (setf (box-rot box-a) (q:make-quat-from-axis-angle
                           (v! (sin factor) (cos factor) 1) 10)
        (box-rot box-b) (q:make-quat-from-axis-angle
                           (v! (sin (/ factor 5)) (cos (/ factor -3)) 1) 10))
  (clear)

  (map-g #'draw-box box-stream
         :model->clip (model->clip box-a camera)
         :tex brick)
  (with-blending bp
    (map-g #'draw-box box-stream
           :model->clip (model->clip box-b camera)
           :tex brick
           :f (+ 0.7 (* (sin factor) 0.3))))

  (update-display))

;;- - - - - - - - - - - - - - - - - -

(defun init ()
  (destructuring-bind (d i) (primitives:box-data)
    (setf box-data (make-gpu-array d :element-type 'g-pnt)
          box-index (make-gpu-array i :element-type :ushort)
          box-stream (make-buffer-stream box-data :index-array box-index)
          brick (devil-helper:load-image-to-texture
                 (merge-pathnames "brick/col.png" *examples-dir*)))))

(let ((running t))
  (defun run-loop ()
    (init)
    (loop :while running :do
       (continuable
         (evt:pump-events)
         (update-swank)
         (step-demo))))
  (defun stop-loop () (setf running nil)))

(evt:observe (e evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))
