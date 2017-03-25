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
  (if (not (eq (type-of *declared-host*) (type-of host-obj)))
      (error "Multiple CEPL host definitions found. Cannot continue")
      (error "IMPLEMENT ME! (replace-host ~s)" host-obj)))

(defgeneric check-host (host)
  (:method (host)
    (error "A host for cepl must be a subclass of one of the host-api classes")))

(defgeneric %init (host args))

(defgeneric initialize (&rest args &key &allow-other-keys)
  (:method (&rest args &key &allow-other-keys)
    (if *declared-host*
        (if *current-host*
            (warn "CEPL: Cannot reinitialize a CEPL host")
            (let ((host *declared-host*))
              (%init host args)
              (setf *current-host* host)))
        (error "CEPL.Host: No host found. Have you loaded a host?"))))
