;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is a dumping ground for homeless functions
;; If anything is here currently then it needs to be rehomed or
;; the essence of the functionality decided, generalised and 
;; rehomed.

(in-package :cepl)

(defun get-gl-extensions ()
  (if (<= 3 (gl:major-version))
      (loop :for i :below (gl:get-integer :num-extensions)
         :collect (%gl:get-string-i :extensions i))
      ;; OpenGL version < 3
      (cl-utilities:split-sequence #\space (gl:get-string :extensions)
                                   :remove-empty-subseqs t)))

(defun cepl-post-context-initialize ()
  (let ((available-extensions (get-gl-extensions)))
    (labels ((has-feature (x) (find x available-extensions :test #'equal)))
      (unless (has-feature "GL_ARB_texture_storage") 
        (setf cgl::*immutable-available* nil)))
    t))

(defun repl (&optional (width 640) (height 480))
  (in-package :cepl)  
  (%repl width height))

(defun %repl (&optional (width 640) (height 480))  
  (if (sdl2:init)
      (multiple-value-bind (context window)
          (new-window :width width :height height :title "CEPL REPL")
        (if (and context window (cepl-post-context-initialize))
            (let ((context (make-instance 'cgl:gl-context :handle context)))
              (setf cgl::*gl-window* window)
              (setf (dval cgl::*gl-context*) context)
              (format t "-----------------~%    CEPL-REPL    ~%-----------------"))
            (progn (sdl2:quit)
                   (error "Failed to initialise CEPL"))))
      (error "Failed to initialise SDL")))

(defun quit ()
  (sdl2:quit))

(defun peek (x) (swank:inspect-in-emacs x))

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
   working while cepl runs"
  (base-macros:continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))
