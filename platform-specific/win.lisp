;; THIS IS UNTESTED IT PROBABLY DOESNT WORK

(in-package :cepl-win)

(defun start ()
  (sdl2:make-this-thread-main
   (lambda () (swank:create-server :style nil :dont-close t))))
