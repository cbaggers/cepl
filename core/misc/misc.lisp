(in-package :cepl.misc)

;; This file is just things that you ineviatably end up writing in lots of
;; projects. They are not fast or anything like that, but I keep needing
;; one or another of them when I'm hacking around

;;------------------------------------------------------------

(defun make-gpu-quad ()
  (make-buffer-stream
   (make-gpu-array
    (list (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
          (list (v! -1.0  -1.0 0 0) (v!  0.0   0.0))
          (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
          (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
          (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
          (list (v!  1.0   1.0 0 0) (v!  1.0   1.0)))
    :element-type 'g-pt
    :dimensions 6)
   :retain-arrays t))

(defvar *quad* nil)

(defun get-gpu-quad ()
  (or *quad* (setf *quad* (make-gpu-quad))))

;;------------------------------------------------------------

(defun-g draw-texture-vert ((vert g-pt) &uniform (depth :float))
  (values (v! (s~ (pos vert) :xy) depth 1s0)
          (tex vert)))

(defun-g draw-texture-frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defun-g draw-colored-quad-frag ((tc :vec2) &uniform (color :vec4))
  color)

(def-g-> draw-texture-pipeline ()
  #'draw-texture-vert #'draw-texture-frag)

(def-g-> draw-colored-quad-pipeline ()
  #'draw-texture-vert #'draw-colored-quad-frag)

(defun draw-texture (texture &key (swap t) clear (depth 0.9))
  (when clear (clear))
  (etypecase texture
    (texture
     (with-sampling (s texture)
       (map-g #'draw-texture-pipeline (get-gpu-quad)
              :tex s :depth depth)))
    (sampler
     (map-g #'draw-texture-pipeline (get-gpu-quad)
            :tex texture :depth depth)))
  (when swap (swap)))

(defun draw-colored-quad (color &key (swap t) clear (depth 1s0))
  (when clear (clear))
  (map-g #'draw-colored-quad-pipeline (get-gpu-quad)
         :color color :depth depth)
  (when swap (swap)))
