(in-package :cepl)

(defun osx-start ()
  (sdl2:make-this-thread-main
   (lambda () (swank:create-server :style nil))))
