(in-package :cepl-osx)

(defun start ()
  (let ((extra-package-dirs '("/opt/local/lib/" "/usr/local/lib")))
    (mapcar
     (lambda (raw-path)
       (let ((port-dir (cl-fad:directory-exists-p raw-path)))
         (when (and port-dir
                    (not (member port-dir cffi:*foreign-library-directories*)))
           (push port-dir cffi:*foreign-library-directories*))))
     extra-package-dirs)
    (sdl2:make-this-thread-main
     (lambda () (swank:create-server :style nil :dont-close t)))))
