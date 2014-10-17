;; Basic Lighting

(defparameter *near* 1.0)
(defparameter *far* 1000.0)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *monkey* nil)
(defparameter *camera* nil)
(defparameter *light-direction* 0.0)
(defparameter *resolution* (v! 640 480))

(defglstruct vert-data ()
  (position :vec3)
  (diffuse-color :vec4)
  (normal :vec3))

(defpipeline prog-1
    ((vert vert-data) &uniform (dir-to-light :vec3)
     (light-intensity :vec4) (norm-model-to-cam :mat3)
     (cam-to-clip :mat4) (model-to-cam :mat4)
     (ambient-intensity :float))
  (:vertex (setf gl-position (* cam-to-clip
                                (* model-to-cam				   
                                   (v! (vert-data-position
                                          vert) 1.0))))
           (out (interp-color :smooth) 
                (+ (* light-intensity 
                     (clamp (dot (normalize
                                  (* norm-model-to-cam
                                     (vert-data-normal vert)))
                                 dir-to-light) 
                            0.0 1.0))
                   (* (v! 1.0 1.0 1.0 0.0) 
                      ambient-intensity))))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480 *near* *far*)))


(defstruct entity 
  (stream nil)
  (position (v! 0 0 -4))
  (rotation (v! 0 0 0))
  (scale (v! 1 1 1)))

(defun load-lisp-model (filename)
  (let* ((monkey-data (utils:safe-read-from-string (utils:file-to-string filename)))
         (verts (loop for vert in (first monkey-data)
                   collect (list (v:* (v:merge-into-vector (first vert)) (v! 1 1 1)) 
                                 (v:merge-into-vector (second vert))
                                 (v:merge-into-vector (third vert)))))
         (stream (make-vertex-stream
                  (make-gpu-array verts :element-type 'vert-data
                                      :dimensions (length verts))
                  :length (length (second monkey-data))
                  :index-array (make-gpu-array
                                   (second monkey-data)
                                   :element-type :unsigned-short
                                   :dimensions (length (second monkey-data))))))
    (make-entity :rotation (v! -1.57079633 1 0) :stream stream)))

(defun init () 
  (setf *camera* (make-camera *resolution*))
  (prog-1 nil :cam-to-clip (cam->clip *camera*))
  
  ;;create monkey
  (setf *monkey* (load-lisp-model "monkey.data")))

(defun entity-matrix (entity)
  (reduce #'m4:m* 
          (list (m4:translation (entity-position entity))
                (m4:rotation-from-euler (entity-rotation entity))
                (m4:scale (entity-scale entity)))))

(defun draw ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((world-to-cam-matrix (world->cam *camera*))
         (model-to-cam-matrix (m4:m* world-to-cam-matrix 
                                     (entity-matrix *monkey*)))
         (normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
         (light-vec (v! 0 1 0 0))
         (cam-light-vec (m4:mcol*vec4 world-to-cam-matrix
                                      light-vec)))
    (setf (entity-rotation *monkey*) 
          (v! (+ 0.002 (v-x (entity-rotation *monkey*)))
              (+ 0.001 (v-y (entity-rotation *monkey*)))
              (v-z (entity-rotation *monkey*))))
    (prog-1 (entity-stream *monkey*) 
            :dir-to-light (v! (v-x cam-light-vec) 
                              (v-y cam-light-vec)
                              (v-z cam-light-vec))
            :light-intensity  (v! 0 1 0 1)
            :model-to-cam model-to-cam-matrix
            :norm-model-to-cam normal-to-cam-matrix
            :ambient-intensity 0.2))
  (gl:flush)
  (cgl:update-display))

(defun reshape (width height near far)
  (setf (frame-size *camera*) (v! width height)
        (near *camera*) near
        (far *camera*) far)
  (prog-1 nil :cam-to-clip (cam->clip *camera*)))

(let ((running nil))
  (defun run-demo () 
    (init)
    (reshape 640 480 *near* *far*)  
    (setf running t)
    (loop :while running :do
       (case-events (event)
         (:quit () (setf running nil))
         (:windowevent (:event e :data1 x :data2 y)
                       (when (eql e sdl2-ffi:+sdl-windowevent-resized+)
                         (reshape x y *near* *far*))))
       (update-swank)
       (continuable (draw))))
  (defun stop-demo () (setf running nil)))
