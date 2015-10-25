(in-package #:cepl.events)

;;----------------------------------------------------------------------
;; all events

(defvar |all-events|
  (make-event-node
   :name :all-events
   :tags '(:everything :cepl-event-system-meta)))

;;----------------------------------------------------------------------
;; meta events

(defvar event-system-meta-node
  (make-event-node
   :name :cepl-event-system
   :tags :cepl-event-system-meta
   :subscribe-to |all-events|))

;;----------------------------------------------------------------------
;; cepl system events

(defvar |sys|
  (make-event-node
   :name 'cepl-internals
   :tags '(:cepl-internal :system)
   :filter #'will-quit-p
   :subscribe-to |all-events|))

;;----------------------------------------------------------------------
;; context events

(defvar |context|
  (make-event-node
   :name 'cepl-internals
   :tags '(:context)
   :filter #'context-created-p
   :subscribe-to |all-events|))

;;----------------------------------------------------------------------
;; cepl window events

(defvar |window|
  (make-event-node
   :name 'cepl-window
   :tags '(:window)
   :filter #'win-p
   :subscribe-to |all-events|))

;;----------------------------------------------------------------------
;; cepl mouse events
;;|keyboard|
(defun mouse0-eventp (x)
  (or (and (typep x 'mouse-scroll) (= (id x) 0))
      (and (typep x 'mouse-button) (= (id x) 0))
      (and (typep x 'mouse-motion) (= (id x) 0))))

(let ((button-state (make-hash-table)))
  (labels ((update-mouse-state (event)
              (when (typep event 'mouse-button)
                (setf (gethash (button event) button-state) (state event)))))
    (defvar |mouse|
      (make-event-node
       :name 'cepl-mouse
       :tags '(:mouse)
       :filter #'mouse0-eventp
       :body #'update-mouse-state
       :subscribe-to |all-events|))))

;;----------------------------------------------------------------------
;; cepl keyboard events

(let ((key-state (make-hash-table)))
  (labels ((update-key-states (event)
             (setf (gethash (key event) key-state) (state event))))
    (defvar |keyboard|
      (make-event-node
       :name 'cepl-keyboard
       :tags '(:keyboard)
       :filter #'cepl-keyboard-event-p
       :body #'update-key-states
       :subscribe-to |all-events|)))
  (defun key-state (key) (gethash key key-state :up)))
