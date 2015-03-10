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

(defparameter *examples-directory*
  (asdf:system-relative-pathname :cepl "examples"))

#+sb-thread
(defmacro on-main (&body b)
    `(let ((thread (first (last (sb-thread:list-all-threads)))))
       (sb-thread:interrupt-thread thread
				   #'(lambda () (sb-int:with-float-traps-masked (:underflow :overflow :inexact :invalid :divide-by-zero),@b)))))

#+ccl
(defmacro on-main (&body b)
  `(let ((thread (find 0 (all-processes) :key #'process-serial-number)))
     (process-interrupt thread (lambda () ,@b))))

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
  (setf cgl::+default-resolution+ (list width height))
  (if (sdl2:init)
      (progn #+(and ccl darwin)
             (sdl2:in-main-thread ()
               (%repl width height))
             #-(and ccl darwin)
             (%repl width height))
      (error "Failed to initialise SDL")))

(defun %repl (&optional (width 640) (height 480))
  #+(and ccl darwin)
  (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
  (multiple-value-bind (context window)
      (new-window :width width :height height :title "CEPL REPL")
    (if (and context window (cepl-post-context-initialize))
        (let ((context (make-instance 'cgl:gl-context :handle context)))
          (setf cgl::*gl-window* window)
          (setf cgl::*gl-context* context)
          (format t "-----------------~%    CEPL-REPL    ~%-----------------")
          (unless (>= (gl:major-version) 3)
            (error "Cepl requires OpenGL 3.1 or higher"))
          (%set-default-gl-options)
          (apply #'gl:viewport 0 0 cgl:+default-resolution+))
        (progn (sdl2:quit)
               (error "Failed to initialise CEPL")))))

(defun %set-default-gl-options ()
  (print "Setting default options")
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))

(defun quit ()
  (sdl2:quit))
