(in-package :cepl-osx)

(defun start ()
  (let ((port-dir (cl-fad:directory-exists-p "/opt/local/lib/")))
    (when (and port-dir
               (not (member port-dir cffi:*foreign-library-directories*)))
      (push port-dir cffi:*foreign-library-directories*)
      (sdl2:make-this-thread-main
       (lambda () (swank:create-server :style nil))))))
