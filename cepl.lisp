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
  (loop :for i :below (gl:get-integer :num-extensions)
     :collect (%gl:get-string-i :extensions i)))

(defun cepl-post-context-initialize ()
  (let ((available-extensions (get-gl-extensions)))
    (labels ((has-feature (x) (find x available-extensions :test #'equal)))
      (unless (has-feature "GL_ARB_texture_storage") 
        (setf cgl::*immutable-available* nil)))
    t))

(defun repl (&optional (width 640) (height 480))
  (in-package :cepl)
  (if (sdl2:init)
      (multiple-value-bind (context window)
          (sdl2::new-window :width width :height height :title "CEPL REPL")
        (if (and context window (cepl-post-context-initialize))
            (progn
              (setf cgl::*gl-window* window)              
              (setf (dval cgl::*gl-context*) context)
              (format t "-----------------~%    CEPL-REPL    ~%-----------------"))
            (progn (sdl2:quit)
                   (error "Failed to initialise CEPL"))))
      (error "Failed to initialise SDL")))

(defun quit ()
  (sdl2:quit))

