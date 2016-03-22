;; I started this to unify all the texture stuff and make it lispier
;; After a while I wrote the following
;;
;;      "While I see why I started abstracting this using classes
;;       We cannot extend core functionality of gl, thus uses
;;       extensible constructs is optimizing for a case that can
;;       never happen. We should go for structs, ubyte and macros
;;       to hide the ugly, optimize for helping the compiler"

;; This is here as a record then

(defclass %gltexture ()
  ((id :type (unsigned-byte 16))
   (mutablep :type boolean)
   (allocatedp :type boolean)))

(defclass %glarraytexture (%gltexture)
  ((length :type (unsigned-byte 16))))

(defclass texture-1d (%gltexture) ())
(defclass texture-2d (%gltexture) ())
(defclass texture-3d (%gltexture) ())
(defclass texture-rectangle (%gltexture) ())
(defclass texture-cube-map (%gltexture) ())
(defclass texture-1d-array (%glarraytexture) ())
(defclass texture-2d-array (%glarraytexture) ())
(defclass texture-cube-map-array (%glarraytexture) ())
(defclass texture-buffer (%gltexture)
  ((backing-array :type gpu-array)
   (owns-array :type boolean)))

(defstruct array-cube
  (positive-x nil :type gpu-array-t)
  (negative-x nil :type gpu-array-t)
  (positive-y nil :type gpu-array-t)
  (negative-y nil :type gpu-array-t)
  (positive-z nil :type gpu-array-t)
  (negative-z nil :type gpu-array-t))

(defstruct (array-texture-mipmap-level
             (:constructor %make-array-tex-mipmap-level)
             (:conc-name %atml-))
  (texture nil :type %glarraytexture)
  (mipmap-level 0 :type (unsigned-byte 8)))

(defun arraytexref (level subscript)
  (let ((texture (%atml-texture level))
        (mipmap-level (%atml-mipmap-level level)))
    (if (< subscript (slot-value texture 'length))
        (if (typep texture 'texture-cube-map-array)
            (%make-tex-cube texture mipmap-level subscript)
            (make-instance 'gpu-array-t :texture texture :level-num mipmap-level
                           :layer-num subscript))
        (error "CEPL: Index out of bounds"))))

(defun mipref (texture subscript)
  (typecase texture
    (texture-1d
     (make-instance 'gpu-array-t :texture texture :level-num subscript))
    (texture-2d
     (make-instance 'gpu-array-t :texture texture :level-num subscript))
    (texture-3d
     (make-instance 'gpu-array-t :texture texture :level-num subscript))
    (texture-rectangle (error "rectangle textures dont support mipmap levels"))
    (texture-buffer (error "buffer textures dont support mipmap levels"))
    (%glarraytexture
     (%make-array-tex-mipmap-level :texture texture :mipmap-level subscript))
    (texture-cube-map (%make-tex-cube texture subscript))
    (t (error "CEPL: Unknown texture type"))))

(defun %make-tex-cube (texture mipmap-level &optional (index 0))
  (make-array-cube
   :positive-x (make-instance 'gpu-array-t :texture texture
                              :level-num mipmap-level :face-num 0
                              :layer-num index)
   :negative-x (make-instance 'gpu-array-t :texture texture
                              :level-num mipmap-level :face-num 1
                              :layer-num index)
   :positive-y (make-instance 'gpu-array-t :texture texture
                              :level-num mipmap-level :face-num 2
                              :layer-num index)
   :negative-y (make-instance 'gpu-array-t :texture texture
                              :level-num mipmap-level :face-num 3
                              :layer-num index)
   :positive-z (make-instance 'gpu-array-t :texture texture
                              :level-num mipmap-level :face-num 4
                              :layer-num index)
   :negative-z (make-instance 'gpu-array-t :texture texture
                              :level-num mipmap-level :face-num 5
                              :layer-num index)))
