(in-package :cepl.context)

(defclass gl-context ()
  ((cache :initform (make-hash-table))
   (handle :initarg :handle :reader handle)
   (window :initarg :window :reader window)
   (fbo :initarg :window :reader context-fbo)))
