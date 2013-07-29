;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is a file for me to experiment with textures in
;; I'm really not sure of the best interface for this yet so 
;; we shall play!

(in-package :cepl-gl)

;; dont export this
(defun gen-texture ()
  (first (gl:gen-textures 1)))

;; [TODO] mutable or immutable?

;; Textures are objects that store one or more arrays of
;; data of some dimensionality.
;; Shaders can reference them with sampler type
(defclass texture () 
  ((id :initform (gen-texture) :accessor id)
   (type :initform :none :accessor texture-type)))

(defclass mutable-texture (texture) ())
(defclass immutable-texture (texture) ())

(defun bind-texture (texture &optional type)
  (if (or (null type) (eq type (texture-type texture)))
      (gl:bind-texture (texture-type texture) (id texture))
      (if (eq :none (texture-type texture))
          (progn (gl:bind-texture type (id texture))
                 (setf (texture-type texture) type))
          (error "Texture has already been bound")))
  texture)

;;use with safe-exit thingy?
(defmacro with-bind-texture ((texture &optional type) &body body)
  (let ((tex (gensym "texture"))
        (res (gensym "result")))
    `(let ((,tex ,texture)) 
       (bind-texture ,tex ,type)
       (let ((,res (progn ,@body)))
         (bind-texture 0 (texture-type ,tex))
         ,res))))

;; is storage a seperate entity?

(defgeneric tex-push (texture data internal-format width 
                      height pixel-data-format pixel-data-type
                      &optional level))

(defmethod tex-push ((texture mutable-texture) data 
                     internal-format width height
                     pixel-data-format pixel-data-type
                     &optional (level 0))
  (let ((tex-type (texture-type texture)))
    (case tex-type
      ((:texture-1d :proxy-texture-1d)
       (gl:tex-image-1d tex-type level internal-format width 0
                        pixel-data-format pixel-data-type data))
      ((:texture-2d :proxy-texture-2d)
       (gl:tex-image-2d tex-type level internal-format width 
                        height 0 pixel-data-format pixel-data-type data)))))

;; texture unit
;; (gl:active-texture )

