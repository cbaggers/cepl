(in-package #:cepl.events)

(let (event-pump)
  (defun pump-events ()
    (unless event-pump
      (setf event-pump (cepl-backend:get-event-pump cepl-backend:*backend*)))
    (funcall event-pump)))

(defun map-evt (function event-source)
  (make-instance 'event-cell
                 :event (cells:c? (funcall function (event event-source)))))

(defun filter-evt (predicate event-source)
  (make-instance 'event-celln
                 :event (cells:c? (when (funcall predicate (event event-source))
                              (event event-source)))))

(defun merge-evt (evt-source-a evt-source-b)
  (make-instance 'event-cell
                 :event (cells:c? (or (event evt-source-a) (event evt-source-b)))))

;;--------------------------------------------
;; root nodes

(def-event-node |all-events| () :in)

(def-event-node |sys| (:parent |all-events|) (typep (event :parent) 'will-quit))

(def-event-node |mouse| (:parent |all-events|) (mouse0-eventp (event :parent))
  (pos :cell t :initform
       (c? (when (typep (event self) 'mouse-motion)
             (pos (event self)))))
  (state-tracker
   :cell t :initform (c? (when (typep (event self) 'mouse-button)
                           (setf (gethash (button (event self))
                                          (slot-value self 'button-state))
                                 (state (event self))))))
  (button-state :cell nil :initform (make-hash-table)))

(def-event-node |keyboard| (:parent |all-events|) (typep (event :parent) 'key)
  (state-tracker
   :cell t :initform (c? (when (typep (event self) 'key)
                           (setf (gethash (key (event self))
                                          (slot-value self 'key-state))
                                 (state (event self))))))
  (key-state :cell nil :initform (make-hash-table)))

(def-event-node |window| (:parent |all-events|) (typep (event :parent) 'win))


(defmethod key-state ((target |keyboard|) key)
  (gethash key (slot-value target 'key-state)
           :up))

(defmethod button-state ((target |mouse|) button-id)
  (gethash button-id (slot-value target 'button-state)
           :up))
