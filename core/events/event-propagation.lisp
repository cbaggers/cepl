(in-package #:cepl.events)

;;----------------------------------------------------------------------
;; cepl event pump infrastructure

(let ((event-pump nil))
  (defun register-thunk-with-pump-events (thunk)
    (push thunk event-pump))
  (defun pump-events ()
    (loop :for p :in event-pump :do (funcall p))))

(defun inject-backend-event (event)
  (push-event all-events event))

(defun cepl-event-hook (event)
  "CEPL gets the first look at events so it can maintain some internal data.
   It is not allowed to stop or modify the event."
  (when (and (typep event 'win) (eq (slot-value event 'action) :resized))
    (jungl::%set-default-fbo-viewport (slot-value event 'data))))
