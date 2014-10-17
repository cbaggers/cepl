;; vertex point light

(defparameter *near* 1.0)
(defparameter *far* 1000.0)
(defparameter *frustrum-scale* nil)
(defparameter *monkey* nil)
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *loop-pos* 0.0)
(defparameter *resolution* (v! 640 480))

(defglstruct vcn ()
  (position :vec3 :accessor pos)
  (diffuse-color :vec4 :accessor color)
  (normal :vec3 :accessor normal))

(defpipeline vert-point-light
    ((data vcn) &uniform
     (light-pos :vec3) (light-intensity :vec4)
     (ambient-intensity :vec4) (model-to-cam :mat4)
     (normal-model-to-cam :mat3) (cam-to-clip :mat4))
  (:vertex    
   (let* ((camera-pos (* model-to-cam (v! (pos data) 1.0)))
          (norm-cam-space (normalize (* normal-model-to-cam (normal data))))
          (dir-to-light (normalize 
                         (- light-pos (swizzle camera-pos :xyz))))
          (cos-ang-incidence (clamp (dot norm-cam-space dir-to-light)
                                    0.0 1.0)))
     (setf gl-position (* cam-to-clip camera-pos))
     (out (interp-color :smooth) (+ (* (color data)
                                       light-intensity
                                       cos-ang-incidence) 
                                    (* (color data) ambient-intensity)))))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480 *near* *far*)))

(defclass entity ()
  ((gstream :initform nil :initarg :gstream :accessor gstream)
   (position :initform (v! 0 0 -3) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))

(defclass light ()
  ((position :initform (v! 20 20 -20) :initarg :pos :accessor pos)
   (radius :initform 1.0 :initarg :radius :accessor radius)))

(defun load-lisp-model (filename)
  (let* ((monkey-data (utils:safe-read-from-string (utils:file-to-string filename)))
         (verts (loop for vert in (first monkey-data)
                   collect (list (v:* (v:merge-into-vector (first vert)) (v! 1 1 1)) 
                                 (v:merge-into-vector (second vert))
                                 (v:merge-into-vector (third vert)))))
         (gstream (make-vertex-stream
                   (make-gpu-array verts :element-type 'vcn
                                       :dimensions (length verts))
                   :length (length (second monkey-data))
                   :index-array (make-gpu-array 
                                    (second monkey-data)
                                    :dimensions (length (second monkey-data))
                                    :element-type :unsigned-short))))
    (make-instance 'entity :rot (v! -1.57079633 1 0) :gstream gstream)))

(defun init () 
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera *resolution*))
  (vert-point-light nil :cam-to-clip (cam->clip *camera*))
    
  ;;create monkey
  (setf *monkey* (load-lisp-model "monkey.data")))

(defun entity-matrix (entity)
  (reduce #'m4:m* (list (m4:translation (pos entity))
                        (m4:rotation-from-euler (rot entity))
                        (m4:scale (scale entity)))))

(defun draw ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((world-to-cam-matrix (world->cam *camera*))
         (model-to-cam-matrix (m4:m* world-to-cam-matrix 
                                     (entity-matrix *monkey*)))
         (normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
         (cam-light-vec (m4:mcol*vec4 world-to-cam-matrix 
                                      (v:merge-into-vector (pos *light*) 1.0))))
    (vert-point-light (gstream *monkey*)
                      :light-pos (v! (v-x cam-light-vec)
                                     (v-y cam-light-vec)
                                     (v-z cam-light-vec))
                      :light-intensity (v! 1 1 1 0)
                      :model-to-cam model-to-cam-matrix
                      :normal-model-to-cam normal-to-cam-matrix
                      :ambient-intensity (v! 0.1 0.1 0.1 1.0)))
  (gl:flush)
  (cgl:update-display))

(defun reshape (width height near far)
  (setf (frame-size *camera*) (v! width height)
        (near *camera*) near
        (far *camera*) far)
  (vert-point-light nil :cam-to-clip (cam->clip *camera*))
  (gl:viewport 0 0 width height))

(let ((running nil))
  (defun run-demo () 
    (init)
    (reshape 640 480 *near* *far*)  
    (setf running t)
    (loop :while running :do
       (when (step-demo)
         (setf running nil))
       (update-swank)))
  (defun stop-demo () (setf running nil)))

(defun step-demo ()
  (setf *loop-pos* (+ *loop-pos* 0.005))
  (setf (pos *light*) (v! (* 10 (sin *loop-pos*))
                          7
                          (* 10 (cos *loop-pos*))))
  (let ((end? nil))
    (case-events (event)
      (:quit () (setf end? t))
      (:windowevent (:event e :data1 x :data2 y)
                    (when (eql e sdl2-ffi:+sdl-windowevent-resized+)
                      (reshape x y *near* *far*))))
    (continuable (draw))
    end?))
