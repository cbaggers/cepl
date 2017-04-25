(in-package :cepl.host)

(defclass cepl-host-api () ())

(defvar *declared-host* nil)
(defvar *current-host* nil)

(defun register-host (host-class-name)
  (assert (symbolp host-class-name) (host-class-name)
          "cepl.host: #'register-host takes the name of a class")
  (let ((host (check-host (make-instance host-class-name))))
    (if *declared-host*
        (replace-host host)
        (setf *declared-host* host))))

(defun replace-host (host-obj)
  ;; check if has higher api-version
  (when (not (eq (type-of *declared-host*) (type-of host-obj)))
    (error "Multiple CEPL host definitions found. Cannot continue")))

(defgeneric check-host (host)
  (:method (host)
    (declare (ignore host))
    (error "A host for cepl must be a subclass of one of the host-api classes")))
