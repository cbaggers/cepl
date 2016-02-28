(in-package :cepl.host)

;; This is what the backend has to implement
(defgeneric init ())
(defgeneric request-context
    (width height title fullscreen
     no-frame alpha-size depth-size stencil-size
     red-size green-size blue-size buffer-size
     double-buffer hidden resizable))
(defgeneric shutdown ())
(defgeneric set-primary-thread-and-run (func &rest args))
