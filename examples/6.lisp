;; Playing around with a diamond square implementation

;; [TODO] This whole demo is messy and out of date, fix it
;; not working

(in-package :cepl)

(defglstruct vert-data 
  (position :vec3)
  (color :vec4))

(defpipeline prog-1 ((vert vert-data) &uniform (cam-to-clip :mat4)
                     (world-to-cam :mat4) (model-to-world :mat4))
  (:vertex (setf gl-position (* cam-to-clip
                                (* world-to-cam 
                                   (* model-to-world
                                      (v! (vert-data-position vert) 1.0)))))
           (out (interp-color :smooth) (vert-data-color vert)))
  (:fragment (out output-color interp-color))
  (:post-compile (reshape 640 480)))


(setf *random-state* (make-random-state t))
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *terrain* nil)
(defparameter *camera* nil)
(defparameter *running* t)

(defstruct entity 
  (stream nil)
  (position (v! 0 0 -3))
  (rotation (v! 0 1.62 0))
  (scale (v! 1 1 1)))

(defstruct camera 
  (position (v! 0 0 0))
  (look-direction (v! 0 0 -1))
  (up-direction (v! 0 1 0)))

(defun point-camera-at (camera point)
  (setf (camera-look-direction camera)
        (v:normalize (v:- point (camera-position camera)))) camera)

(defun calculate-cam-look-at-w2c-matrix (camera)
  (let* ((look-dir (v3:normalize (camera-look-direction camera)))
         (up-dir (v3:normalize (camera-up-direction camera)))
         (right-dir (v3:normalize (v3:cross look-dir up-dir)))
         (perp-up-dir (v3:cross right-dir look-dir))
         (rot-matrix (m4:transpose
                      (m4:rotation-from-matrix3
                       (m3:make-from-rows right-dir
                                          perp-up-dir
                                          (v3:v-1 (v! 0 0 0)
                                                  look-dir)))))
         (trans-matrix 
          (m4:translation (v3:v-1 (v! 0 0 0)
                                  (camera-position camera)))))
    (m4:m* rot-matrix trans-matrix)))

;;----------------------------------------------

(defun gen-gs-terrain-model (&optional (depth 6) (square-size 20.0) raw)
  (destructuring-bind (verts indicies) 
      (get-terrain-verts-and-indices 
       (diamond-square :depth depth :random-start 50.0 :random-decay 0.54
                       :corner-seed '(120.0 -130.0 0.0 50.0)) 
       square-size)
    (if raw 
        (list verts indicies)
        (make-entity 
         :position (v! 0.0 -130.0 -15.0)
         :rotation (v! 0.0 -2.3 0.0)
         :stream (make-vertex-stream
                  (make-gpu-array verts :element-type 'vert-data
                                      :dimensions (length verts))
                  :index-array (make-gpu-array 
                                   indicies 
                                   :element-type :unsigned-short
                                   :dimensions (length indicies)))))))

(defun get-terrain-verts-and-indices (terrain square-size)
  (labels ((gen-vert (data x y)
             (list (v! (* square-size x) (aref data x y) (* square-size y))
                   (v! (random 1.0) (random 1.0) (random 1.0) 0.0))))
    (let* ((data terrain)
           (data-dimen (array-dimensions data)))
      (list (loop for y below (second data-dimen)
               append (loop for x below (first data-dimen)
                         collect (gen-vert data x y)))
            (let ((size (first data-dimen)))
              (loop for y below (1- size)
                 append
                   (loop for x below (1- size)
                      append `(,(+ x (* y size))
                                ,(+ x (* (1+ y) size))
                                ,(+ (1+ x) (* y size))
                           
                                ,(+ (1+ x) (* y size))
                                ,(+ x (* (1+ y) size))
                                ,(+ (1+ x) (* (1+ y) size))))))))))

(defun diamond-square (&key (depth 2) (corner-seed '(0.0 0.0 0.0 0.0))
                         (random-start 1.0) (random-decay 0.5))
  (let* ((size (expt 2 depth))
         (terrain (make-array `(,(1+ size) ,(1+ size)))))
    ;;set corners
    (setf (aref terrain 0 0) (first corner-seed)
          (aref terrain size 0) (second corner-seed)
          (aref terrain 0 size) (third corner-seed)
          (aref terrain size size) (fourth corner-seed))
    
    (loop :for i :from depth :downto 1
       :for x :from 1 :do 
       (let ((step-size (expt 2 i))) 
         (setf terrain
               (square-step size step-size
                            (* random-start random-decay x)
                            (diamond-step size step-size 
                                          (* random-start random-decay x)
                                          terrain)))
         (setf random-start (* random-start random-decay))))
    terrain))


(defun diamond-step (size step random-range terrain)
  (loop :for x :from 0 :below size :by step :do
     (loop :for y :from 0 :below size :by step
        :do (setf (aref terrain
                        (+ x (floor (/ step 2)))
                        (+ y (floor (/ step 2))))
                  (+ (- (random (* random-range 2.0)) 
                        random-range)
                     (/ (+ (aref terrain x y)
                           (aref terrain (+ x step) y)
                           (aref terrain x (+ y step))
                           (aref terrain (+ x step) (+ y step)))
                        4.0)))))
  terrain)

(defun square-step (size step random-range terrain)
  (let* ((hstep (floor (/ step 2)))
         (mod-val (+ size hstep)))
    (loop :for i :from hstep :by step
       :until (> (* hstep (floor (/ i mod-val))) size) :do
       (let ((x (mod i mod-val))
             (y (* hstep (floor (/ i mod-val)))))
         (setf (aref terrain x y)
               (+ (- (random (* random-range 2.0)) 
                     random-range)
                  (/ (+ (aref terrain (max 0 (- x hstep)) y)
                        (aref terrain x (max 0 (- y hstep)))
                        (aref terrain (min size (+ x hstep)) y)
                        (aref terrain x (min size (+ y hstep))))
                     4.0))))))
  terrain)


;----------------------------------------------

(defun init () 
  (setf *camera* (make-camera :position (v! 0.0 0.0 0.0)))
  (setf *frustrum-scale* (cepl-camera:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix *frustrum-scale*))
  (prog-1 nil :cam-to-clip *cam-clip-matrix*)
  ;;get terrain
  (setf *terrain* (gen-gs-terrain-model))
  ;;set options
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))  

(defun entity-matrix (entity)
  (reduce #'m4:m* (list (m4:translation (entity-position entity))
                        (m4:rotation-from-euler (entity-rotation entity))
                        (m4:scale (entity-scale entity)))))

;----------------------------------------------

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)
  (prog-1 (entity-stream *terrain*)
          :model-to-world (entity-matrix *terrain*)
          :world-to-cam (calculate-cam-look-at-w2c-matrix *camera*))
  (gl:flush)
  (cgl:update-display))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
        (* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1) *frustrum-scale*)
  (prog-1 nil :cam-to-clip *cam-clip-matrix*)
  (cgl:viewport 0 0 width height))

(defun step-game (draw-timer draw-stepper )
  (case-events (event)
    (:quit () (setf *running* nil))
    (:windowevent (:event e :data1 x :data2 y)
                  (when (eql e sdl2-ffi:+sdl-windowevent-resized+)
                    (reshape x y))))
  (setf (camera-position *camera*) (v:+ (camera-position *camera*)
                                        (v:* (camera-look-direction *camera*)
                                             0.001)))
  (on-step-call (draw-stepper (funcall draw-timer))
    (cepl-utils:update-swank)
    (continuable (draw))))

(defun run-demo () 
  (init)
  (setf *running* t)
  (let ((draw-timer (make-time-buffer))
        (draw-stepper (make-stepper (/ 1000.0 60))))
    (loop :while *running* :do 
       (step-game draw-timer draw-stepper))))
(defun stop-demo () (setf *running* nil))
