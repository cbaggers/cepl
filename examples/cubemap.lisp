(in-package :cepl)

(defvar tx)
(defvar strm nil)
(defvar cam)

(defun-g vert ((vert :vec3) &uniform (mod-clip :mat4))
  (values (* mod-clip (v! vert 1))
          vert))

(defun-g frag ((tc :vec3) &uniform (tex :sampler-cube))
  (texture tex tc))

(defpipeline skybox () (g-> #'vert #'frag))

(defun make-cubemap-tex (&rest paths)
  (with-c-arrays (ca (mapcar #'devil-helper:load-image-to-c-array paths))
    (make-texture ca :internal-format :rgb8 :cubes t)))

(defun init ()
  (setf tx (make-cubemap-tex "left.png" "right.png" "up.png"
                             "down.png" "front.png" "back.png"))
  (let* ((bx (primitives:box-data))
         (data (make-gpu-array (first bx) :element-type 'g-pnt))
         (ind (make-gpu-array (primitives:swap-winding-order (second bx))
                              :element-type :ushort)))
    (setf strm (make-buffer-stream data :index-array ind
                                   :retain-arrays t)))
  (setf cam (make-camera)))

(defun step-demo ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'skybox strm :tex tx :mod-clip (m4:m* (cam->clip cam) (world->cam cam)))
  (update-display))

(defvar mouse-ang (v! 0 0))
(evt:observe (evt:|mouse|)
  (when (typep e 'evt:mouse-motion)
    (let ((d (evt:delta e)))
      (setf mouse-ang (v2:v+ (v! (/ (v:x d) -100.0) (/ (v:y d) -100.0))
                             mouse-ang)
            (dir cam) (v! (sin (v:x mouse-ang))
                          (sin (v:y mouse-ang))
                          (cos (v:x mouse-ang)))))))

(observe (|window|) (when (eq (action e) :resized) (reshape (vec e))))
(defun reshape (&optional (dims +default-resolution+))
  (apply #'gl:viewport 0 0 dims))

(live:main-loop :init init :step step-demo)
