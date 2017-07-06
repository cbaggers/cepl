(in-package #:cepl.build)

(defvar *cepl-release-mode* nil)

(defmacro release-unwind-protect (protected &body cleanup)
  (if *cepl-release-mode*
      `(multiple-value-prog1 ,protected
         ,@cleanup)
      `(unwind-protect ,protected ,@cleanup)))

(defmacro profile-block (name &body body)
  (declare (ignore name))
  `(progn ,@body))

(defun load-in-release-mode (&optional (cepl-host :cepl.sdl2))
  (assert (not (find-package :cepl)) ()
          "Sorry, CEPL has already been loaded so it is too late to build in release mode")
  (setf *cepl-release-mode* t)
  (print "-- Building CEPL in Release Mode --")
  (asdf:load-system :cepl :force t)
  (asdf:load-system cepl-host)
  t)
