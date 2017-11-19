(in-package :%cepl.types)

;;------------------------------------------------------------

(declaim (type gpu-buffer +null-gpu-buffer+))
(defvar +null-gpu-buffer+
  (%make-gpu-buffer :arrays (make-array 0 :element-type 'gpu-array-bb)))

(declaim (type att +null-att+))
(defvar +null-att+
  (make-att))

(declaim (type texture +null-texture+))
(defvar +null-texture+
  (%%make-texture :type nil
                  :image-format nil))

(declaim (type vao-id +null-vao+))
(defvar +null-vao+ 0)

(declaim (type fbo +null-fbo+))
(defvar +null-fbo+
  (%%make-fbo :draw-buffer-map (cffi:null-pointer)))

(declaim (type gpu-array-t +null-texture-backed-gpu-array+))
(defvar +null-texture-backed-gpu-array+
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type nil))

(declaim (type gpu-array-bb +null-buffer-backed-gpu-array+))
(defvar +null-buffer-backed-gpu-array+
  (%make-gpu-array-bb :buffer +null-gpu-buffer+
                      :access-style :invalid
                      :element-type nil
                      :byte-size 0
                      :offset-in-bytes-into-buffer 0))

(declaim (type (array gpu-array-bb (*)) +uninitialized-buffer-array+))
(defvar +uninitialized-buffer-array+
  (make-array 0 :element-type 'gpu-array-bb
              :initial-element +null-buffer-backed-gpu-array+))

;;------------------------------------------------------------

(defun+ make-uninitialized-fbo ()
  (%%make-fbo
   :draw-buffer-map nil
   :clear-mask -13))

(defun+ make-uninitialized-texture (&optional buffer-backed-p)
  (if buffer-backed-p
      (%%make-buffer-texture
       :type :uninitialized
       :image-format :uninitialized
       :backing-array (make-uninitialized-gpu-array-bb))
      (%%make-texture
       :type :uninitialized :image-format :uninitialized)))

(defun+ make-uninitialized-gpu-array-bb (&optional buffer)
  (%make-gpu-array-bb
   :buffer (or buffer +null-gpu-buffer+)
   :access-style :uninitialized))

(defun+ make-uninitialized-gpu-array-t ()
  (%make-gpu-array-t
   :texture +null-texture+
   :texture-type :uninitialized))

(defun+ make-uninitialized-sampler (texture)
  (%make-sampler
   :texture texture
   :type :uninitialized))

(defun+ make-uninitialized-buffer-stream (primitive)
  (make-raw-buffer-stream :index-type :uninitialized
                          :primitive primitive))

(defun+ make-uninitialized-gpu-buffer ()
  (%make-gpu-buffer :id 0 :arrays +uninitialized-buffer-array+))

;;------------------------------------------------------------
