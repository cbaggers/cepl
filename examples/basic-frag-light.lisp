;; fragment point light - unfinished
(in-package :cepl)
(named-readtables:in-readtable fn_::fn_lambda)

(defparameter *near* 1.0)
(defparameter *far* 1000.0)
(defparameter *frustrum-scale* nil)
(defparameter *monkey* nil)
(defparameter *resolution* (v! 640 480))
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *loop-pos* 0.0)

(defglstruct vcn ()
  (position :vec3 :accessor pos)
  (diffuse-color :vec4 :accessor color)
  (normal :vec3 :accessor normal))

(defpipeline frag-point-light 
    ((data vcn) &uniform (model-to-cam :mat4) 
     (cam-to-clip :mat4) (model-space-light-pos :vec3)
     (light-intensity :vec4) (ambient-intensity :vec4))
  (:vertex (setf gl-position (* cam-to-clip (* model-to-cam 
                                               (v! (pos data) 1.0))))
           (out model-space-pos (pos data))
           (out vertex-normal (normal data))
           (out diffuse-color (color data)))
  (:fragment (let* ((light-dir (normalize (- model-space-light-pos 
                                             model-space-pos)))
                    (cos-ang-incidence
                     (clamp (dot (normalize vertex-normal) light-dir)
                            0.0 1.0)))
               (out output-color (+ (* diffuse-color light-intensity 
                                       cos-ang-incidence)
                                    (* diffuse-color
                                       ambient-intensity))))))

(defclass entity ()
  ((gstream :initform nil :initarg :gstream :accessor gstream)
   (position :initform (v! 0 0 -4) :initarg :pos :accessor pos)
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
                                    :element-type :unsigned-short
                                    :dimensions (length (second monkey-data))))))
    (make-instance 'entity :rot (v! -1.57079633 1 0) :gstream gstream)))

(defun init () 
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera *resolution*))
  (reshape *resolution*)
    
  ;;create monkey
  (setf *monkey* (load-lisp-model "/home/baggers/Code/quicklisp/local-projects/cepl/examples/monkey.data"))  

  ;;set options
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))


(defun entity-matrix (entity)
  (reduce #'m4:m* (list (m4:translation (pos entity))
                        (m4:rotation-from-euler (rot entity))
                        (m4:scale (scale entity)))))

(evt:defnode mouse-control-monkey
    (:source evt.sdl:|mouse| :var e :kind evt.sdl:terminal)
  (when (typep e 'evt.sdl:mouse-motion)
    (let ((d (evt.sdl:delta e)))
      (setf (rot *monkey*) (v:+ (rot *monkey*) (v! (/ (v:y d) -100.0)
                                                   (/ (v:x d) -100.0)
                                                   0.0))))))
(evt:defnode mouse-control-monkey
    (:source evt.sdl:|keyboard| :var e :kind evt.sdl:terminal)  
  (let ((key (evt.sdl:key e)))    
    (case key
      (:w (setf (pos *monkey*) (v:+ (pos *monkey*) (v! 0 0 -1))))
      (:s (setf (pos *monkey*) (v:+ (pos *monkey*) (v! 0 0 1)))))))

(defun draw ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((world-to-cam-matrix (world->cam *camera*))
         (model-to-cam-matrix (m4:m* world-to-cam-matrix 
                                     (entity-matrix *monkey*)))
         ;;(normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
         (cam-light-vec (m4:mcol*vec4 world-to-cam-matrix 
                                      (v:merge-into-vector (pos *light*) 1.0))))
    (frag-point-light (gstream *monkey*)
                      :model-space-light-pos (v! (v:x cam-light-vec)
                                     (v:y cam-light-vec)
                                     (v:z cam-light-vec))
                      :light-intensity (v! 1 1 1 0)
                      :model-to-cam model-to-cam-matrix
                      ;; :normal-model-to-cam normal-to-cam-matrix
                      :ambient-intensity (v! 0.1 0.1 0.1 1.0)))
  (gl:flush)
  (cgl:update-display))

(defun reshape (new-dimensions)
  (setf *resolution* new-dimensions
        (frame-size *camera*) new-dimensions)
  (gl:viewport 0 0 (v:x new-dimensions) (v:y new-dimensions))
  (frag-point-light nil :cam-to-clip (cam->clip *camera*)))

(let ((running nil))
  (defun run-demo () 
    (init)
    (setf running t)
    (evt:+ evt.sdl:|sys|
           λ(when (typep % 'evt.sdl:will-quit) 
              (setf running nil)))
    (evt:+ evt.sdl:|window| λ(when (eq :resize(evt.sdl:action %)) 
                               (reshape (evt.sdl:vec %))))
    (loop :while running :do
       (continuable
         (step-demo)         
         (update-swank))))
  (defun stop-demo () (setf running nil)))

(defun step-demo ()
  (setf *loop-pos* (+ *loop-pos* 0.01))
  (setf (pos *light*) (v! (* 10 (sin *loop-pos*))
                          10 
                          (* 10 (cos *loop-pos*))))
  (draw)
  (evt.sdl:pump-events))

