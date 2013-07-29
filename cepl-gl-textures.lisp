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

(defun gen-texture ()
  (first (gl:gen-textures 1)))

(defclass texture () 
  ((id :initform (gen-texture) :accessor id)
   (type :initform :none :accessor texture-type)))



(defun bind-texture (texture &optional type)
  (if (or (null type) (eq type (texture-type texture)))
      (gl:bind-texture (texture-type texture) (id texture))
      (if (eq :none (texture-type texture))
          (progn (gl:bind-texture type (id texture))
                 (setf (texture-type texture) type))
          (error "Texture has already been bound")))
  texture)
