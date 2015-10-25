(in-package #:cepl.events)

;;----------------------------------------------------------------------
;; cepl event pump infrastructure

(let (event-pump)
  (defun pump-events ()
    (unless event-pump
      (setf event-pump (cepl-backend:get-event-pump cepl-backend:*backend*)))
    (funcall event-pump)))

(defun inject-event (event)
  (cepl-event-hook event)
  (push-event-to-subscribers |all-events| event))

(defun cepl-event-hook (event)
  "CEPL gets the first look at events so it can maintain some internal data.
   It is not allowed to stop or modify the event."
  (when (and (typep event 'win) (eq (slot-value event 'action) :resized))
    (cgl::%set-default-fbo-viewport (slot-value event 'data))))
