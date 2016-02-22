;;;; package.lisp

(defpackage (#| TMPL_VAR name |#)
  (:use #:cl #:cepl #:temporal-functions
	#:varjo-lang #:rtg-math
	(#| TMPL_IF skitter-sdl-p |#):skitter.sdl2.keys :skitter.sdl2.mouse-buttons(#| /TMPL_IF |#)))

;;;; Used to start session of swank (or similar)

(in-package #:(#| TMPL_VAR name |#))

(defun %run-session ()
  #+darwin
  (let ((extra-package-dirs '("/opt/local/lib/" "/usr/local/")))
    (mapcar
     (lambda (raw-path)
       (let ((port-dir (cl-fad:directory-exists-p raw-path)))
         (when (and port-dir
                    (not (member port-dir cffi:*foreign-library-directories*)))
           (push port-dir cffi:*foreign-library-directories*))))
     extra-package-dirs))
  (let (#+linux
        (style swank::*communication-style*)
        #-linux
        (style nil))
    (cepl.host:set-primary-thread-and-run
     (lambda () (#| TMPL_VAR start-repl-session |#)))))
