(in-package #:cepl.events)

(let (event-pump)
  (defun pump-events ()
    (unless event-pump
      (setf event-pump (cepl-backend:get-event-pump cepl-backend:*backend*)))
    (funcall event-pump)))

(defun inject-event (event)
  (cepl-event-hook event)
  (|all-events| event))

(defun cepl-event-hook (event)
  "CEPL gets the first look at events so it can maintain some internal data.
   It is not allowed to stop or modify the event."
  (when (and (typep event 'win) (eq (slot-value event 'action) :resized))
    (cgl::%set-default-fbo-viewport (slot-value event 'data))))


;; all events
(def-special-event-listener |all-events| (:parent nil :filter nil))


;; system events
(def-special-event-listener |sys|
    (:parent :all-events :filter #'will-quit-eventp))

(defun will-quit-eventp (event) (typep event 'will-quit))


;; context events
(def-special-event-listener |context|
    (:parent :all-events :filter #'context-eventp))

(defun context-eventp (event) (typep event 'context-created))

;; keyboard events
(let ((key-state (make-hash-table)))

  (def-special-event-listener |keyboard|
      (:parent :all-events :filter #'keyboard-eventp)
    ;; update key state
    (setf (gethash (key event) key-state) (state event)))

  (defun key-state (key) (gethash key key-state :up)))

(defun keyboard-eventp (event) (typep event 'key))


;; mouse events
(let ((button-state (make-hash-table)))
  (def-special-event-listener |mouse|
      (:parent :all-events :filter #'mouse0-eventp)
    ;; update button state
    (when (typep event 'mouse-button)
      (setf (gethash (button event) button-state) (state event))))

  (defun mouse-button-state (button) (gethash button button-state :up)))

(defun mouse0-eventp (x)
  (or (and (typep x 'mouse-scroll) (= (id x) 0))
      (and (typep x 'mouse-button) (= (id x) 0))
      (and (typep x 'mouse-motion) (= (id x) 0))))

;; window events
(def-special-event-listener |window|
    (:parent :all-events :filter #'window-eventp))

(defun window-eventp (event) (typep event 'win))
