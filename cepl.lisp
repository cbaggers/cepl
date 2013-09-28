;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is a dumping ground for homless functions
;; If anything is here currently then it needs to be rehomed or
;; the essence of the functionality decided, generalised and 
;; rehomed.

(in-package :cepl)

(defun get-gl-extensions ()
  (loop for i below (gl:get-integer :num-extensions)
     collect (%gl:get-string-i :extensions i)))

(defun cepl-post-context-initialize ()
  (let ((required-extensions '("GL_ARB_texture_storage"))
        (available-extensions (get-gl-extensions)))
    (loop :for ext :in required-extensions :do
       (when (not (find ext available-extensions :test #'equal))
         (error "Required OpenGL Extension '~a' not found"
                ext)))
    t))

(defun repl (&optional (width 1024) (height 768))
  (in-package :cepl)
  (if (sdl:init-sdl)
      (progn
        (sdl:window width height 
                    :icon-caption "CEPL REPL"
                    :title-caption "CEPL REPL")
        (if (cepl-post-context-initialize)
            (format t "-----------------~%    CEPL-REPL    ~%-----------------")
            (progn (sdl:quit)
                   (error "Failed to initialise CEPL"))))
      (error "Failed to initialise SDL")))

(defun quit ()
  (sdl:quit))

