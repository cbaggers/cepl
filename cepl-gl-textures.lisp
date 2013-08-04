;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;
(in-package :cepl-gl)

(defparameter *valid-texture-storage-options* 
  '(((t nil nil 1 nil nil nil) :texture-1d)
    ((t nil nil 2 nil nil nil) :texture-2d)
    ((t nil nil 3 nil nil nil) :texture-3d)
    ((t t nil 1 nil nil nil) :texture-1d-array)
    ((t t nil 2 nil nil nil) :texture-2d-array)
    ((t nil t 2 nil nil nil) :texture-cube-map)
    ((t t t 2 nil nil nil) :texture-cube-map-array)
    ((nil nil nil 2 nil nil t) :texture-rectangle)
    ((nil nil nil 1 nil t nil) :texture-buffer)
    ((nil nil nil 2 t nil nil) :texture-2d-multisample)
    ((nil t nil 2 t nil nil) :texture-2d-multisample-array)))

;; [todo] this needs setting
(defparameter *mipmap-max-levels* 20)

(defun tex-storage-1d (target levels internal-format width)
  (%gl:tex-storage-1d target levels (gl::internal-format->int internal-format)
                      width))

(defun tex-storage-2d (target levels internal-format width height)
  (%gl:tex-storage-2d target levels (gl::internal-format->int internal-format)
                      width height))

(defun tex-storage-3d (target levels internal-format width height depth)
  (%gl:tex-storage-3d target levels (gl::internal-format->int internal-format)
                      width height depth))

;; dont export this
(defun gen-texture ()
  (first (gl:gen-textures 1)))

;; Textures are complex custom structures which can have:
;; mipmaps, layers(arrays), and cubes 
;; The leaves of these structures are (texture backed)gpu-arrays
;; of up to three dimensions.

(defun potp (x) (eql 0 (logand x (- x 1))))

(defun establish-texture-type (dimensions mipmap layers cubes pot multisample 
                               buffer rectangle)
  (declare (ignore pot))
  (cadr (assoc (list mipmap layers cubes dimensions multisample buffer rectangle)
               *valid-texture-storage-options*
               :test #'(lambda (a b)
                         (destructuring-bind
                               (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7)
                             (append a b)
                           (and (if b1 t (not a1)) (if b2 t (not a2))
                                (if b3 t (not a3)) (eql b4 a4)
                                (eql b5 a5) (eql b6 a6) (eql b7 a7)))))))

;; [TODO] how does mipmap-max affect this
;; [TODO] si the max layercount?
;; [TODO] should fail on layer-count=0?
(defun make-texture (dimensions &key (internal-format :rgba8) (mipmap nil) 
                                  (layer-count 1) (cubes nil)
                                  (rectangle nil) (multisample nil)
                                  (immutable t) (buffer-storage nil))
  (if (and immutable (not buffer-storage))
      ;; check for power of two - handle or warn
      (let ((array-type (establish-texture-type 
                         (if (listp dimensions) (length dimensions) 1)
                         mipmap (> layer-count 1) cubes 
                         (every #'potp dimensions) multisample 
                         buffer-storage rectangle)))
        (if array-type
            (if (and cubes (not (apply #'= dimensions)))
                (error "Cube textures must be square")
                (let ((texture (make-instance 
                                'immutable-texture
                                :texture-id (gen-texture)
                                :base-dimensions dimensions
                                :array-type array-type
                                :mipmap-levels 
                                (if mipmap
                                    (floor (log (apply #'max dimensions) 2))
                                    1)
                                :layer-count layer-count
                                :cubes cubes
                                :internal-format internal-format)))
                  (with-texture-bound (texture)
                    (allocate-texture texture))
                  texture))
            (error "This combination of texture features is invalid")))
      (error "Textures with mutable storage and buffer backed 
              textures are not yet implemented")))

(defun allocate-texture (texture)
  (if (allocatedp texture)
      (error "Attempting to reallocate a previously allocated texture")
      (let ((base-dimensions (base-dimensions texture))
            (array-type (slot-value texture 'array-type)))
        (case array-type
          ((:texture-1d :proxy-texture-1d) 
           (tex-storage-1d array-type
                           (slot-value texture 'mipmap-levels)
                           (slot-value texture 'internal-format)
                           (first base-dimensions)))
          ((:texture-2d :proxy-texture-2d :texture-1d-array :texture-rectangle
                        :proxy-texture-rectangle :texture-cube-map
                        :proxy-texture-cube-map :proxy-texture-1d-array)
           (tex-storage-2d array-type
                           (slot-value texture 'mipmap-levels)
                           (slot-value texture 'internal-format)
                           (first base-dimensions)
                           (second base-dimensions)))
          ((:texture-3d :proxy-texture-3d :texture-2d-array :texture-cube-array 
                        :proxy-texture-cube-array :proxy-texture-2d-array)
           (tex-storage-3d array-type
                           (slot-value texture 'mipmap-levels)
                           (slot-value texture 'internal-format)
                           (first base-dimensions)
                           (second base-dimensions)
                           (third base-dimensions))))
        (setf (slot-value texture 'allocated) t))))

(defclass immutable-texture ()
  ((texture-id :initarg :texture-id :reader texture-id)
   (base-dimensions :initarg :base-dimensions :accessor base-dimensions)
   (array-type :initarg :array-type)
   (mipmap-levels :initarg :mipmap-levels)
   (layer-count :initarg :layer-count)
   (cubes :initarg :cubes)
   (internal-format :initarg :internal-format) 
   (allocated :initform nil :reader allocatedp)))

;; (defclass immutable-texture-class1 (immutable-texture) ())
;; (defclass immutable-texture-class2 (immutable-texture) ())
;; (defclass immutable-texture-class3 (immutable-texture) ())

;; this is the datastructure that will represent images
;; Once this is working this will be merged into gpu-arrays
;; at that point you will create gpu-arrays and set their backing
;; to be buffer or texture. Well to be fair you'll create a texture
;; and use it's index to browse to the t-array. But still, you will
;; be able to glpull and glpush and expect to have cepl work out what
;; to do.
(defclass t-array ()
  ((texture-id :initarg :texture-id)
   (level-num :initarg :level-num)
   (layer-num :initarg :layer-num)
   (face-num :initarg :face-num)
   (internal-format :initarg :internal-format)))

(defmethod gl-pull-1 ((gl-object t-array) &optional destination)
  (declare (ignore destination))
  (print "Should now pull the t-array to a gl-array"))

(defmethod gl-push-1 ((gl-object t-array) (data gl-array))
  (print "Should now push the gl-array to the t-array"))

(defun texref (texture &key (mipmap-level 0) (layer 0) (cube-face 0))
  (when 'ranges-are-valid
    (make-instance 't-array :texture-id (slot-value texture 'texture-id)
                   :level-num mipmap-level
                   :layer-num layer
                   :face-num cube-face)))

(defmethod aref-gl ((texture immutable-texture) index)
  (with-slots ((mip-levels mipmap-levels)
               (layers layer-count)
               (cubes? cubes))
      texture
    (cond ((eql mip-levels 1) (print 'here-is-a-t-array))
          ((> mip-levels 1) (if (and (> index 0) (< index mip-levels))
                                (make-instance 'texture-mipmap-level
                                               :texture texture
                                               :level-num index)
                                (error "Index out of range. Mipmap-levels: ~a"
                                       mip-levels)))
          ((> layers 1) (if (and (> index 0) (< index mip-levels))
                                (make-instance 'texture-mipmap-level
                                               :texture texture
                                               :level-num index)
                                (error "Index out of range. Mipmap-levels: ~a"
                                       mip-levels)))
          (cubes? (print 'cubes))
          (t (error "This texture does not have any aref'able elements")))))

(defmethod print-object ((object immutable-texture) stream)
  (let ((m (slot-value object 'mipmap-levels))
        (l (slot-value object 'layer-count))
        (c (slot-value object 'cubes)))
    (format stream 
            "#.<GL-~a (~{~a~^x~})~:[~; mip-levels:~a~]~:[~; layers:~a~]>"
            (slot-value object 'array-type)
            (slot-value object 'base-dimensions)
            (when (> m 1) m) (when (> l 1) l) c)))

(defclass texture-mipmap-level ()
  ((texture :initarg :texture)
   (level-num :initarg :level-num)))

(defclass texture-array-layer ()
  ((texture :initarg :texture)
   (level-num :initarg :level-num)
   (layer-num :initarg :layer-num)))

(defclass texture-cube ()
  ((texture :initarg :texture)
   (level-num :initarg :level-num)
   (layer-num :initarg :layer-num)
   (face-num :initarg :face-num)))



(defun unbind-texture (type)
  (gl:bind-texture type 0))

(defun bind-texture (texture &optional type)
  (let ((array-type (slot-value texture 'array-type)))
    (if (or (null type) (eq type array-type))
        (gl:bind-texture array-type (texture-id texture))
        (if (eq :none array-type)
            (progn (gl:bind-texture type (texture-id texture))
                   (setf (slot-value texture 'array-type) type))
            (error "Texture has already been bound"))))
  texture)

;;use with safe-exit thingy?
(defmacro with-texture-bound ((texture &optional type) &body body)
  (let ((tex (gensym "texture"))
        (res (gensym "result")))
    `(let ((,tex ,texture)) 
       (bind-texture ,tex ,type)
       (let ((,res (progn ,@body)))
         (unbind-texture (slot-value ,tex 'array-type))
         ,res))))

;; bind works on any
;; (gl:bind-texture)
;; create

;; copy data to image
;; (case type
;;   (:texture-1d (gl:tex-sub-image-1d))
;;   ((:texture-cube-map-positive-x :texture-cube-map-negative-x
;;     :texture-cube-map-positive-y :texture-cube-map-negative-y
;;     :texture-cube-map-positive-z :texture-cube-map-negative-z
;;     :texture-1d-array :texture-2d) (gl:tex-sub-image-2d))
;;   ((:texture-3d :texture-2d-array) (gl:tex-sub-image-3d)))

;; create textures
;; copy data (from cpu to gpu) - texsubimage1d texsubimage2d texsubimage3d
;; copy data (from gpu to cpu) - get-tex-image
;; copy data (from frame-buffer to texture image) - leave for now
;; set texture params
;; get texture params

;; texture views
;; generate-mipmaps

;; texsubimage*d - pushing data
;; push data from current gl_read_buffer (gpu rather than from ram)
;; glPixelStore — set pixel storage modes

;;===============================================================
;; Image Formats 
;;===============

;; Remember: The pixel format is the format in which you provide the
;; texture data, the internal format is how OpenGL will store it internally

;; three basic kinds of image formats: color, depth, and depth/stencil
;; for now I will ignore stencil-only

;; Colors in OpenGL are stored in RGBA format. Image formats do not have
;; to store each component. When the shader samples such a texture, 
;; it will resolve to a 4-value vector

;; Stored 3 ways: normalized integers, floating-point, or integral.
;; normalized integer & floating-point resolves to fvector
;; Normalized integer have 2 kinds: unsigned and signed
;; integral formats resolves to ivector

;; |---------bits--------------------------| |--type---| (rgtc,bptc,
;; red green blue alpha shared depth stencil compression


;; ^^-- above is not unique. Need better system
(defun image-format (&rest args)
  (declare (ignore args))
  (error "Have not implemented this yet as I need a friendly user experience."))



;; 
