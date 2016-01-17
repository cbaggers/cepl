(in-package #:cepl.events)

;;======================================================================
;; Events

(defmethod timestamp ((object skitter-event))
  (skitter-event-timestamp object))

;;----------------------------------------------------------------------
;; backend event

;; an event that also contains the backend specific event is represents
(defstruct (cepl-backend-event (:include skitter-event))
  (backend-event nil ;;(error "backend event is mandatory")
                 :type t
                 :read-only t))

;;----------------------------------------------------------------------
;; cepl system events

(defstruct
    (context-created
      (:include skitter-event
                (source-node |context|))))

(defstruct
    (will-quit
      (:include cepl-backend-event
                (source-node |sys|))))

;;----------------------------------------------------------------------
;; cepl window events

(defstruct+methods
    (win
      (:include cepl-backend-event
                (source-node |window|)))
  (action (error "windown event requires an action name")
          :type keyword
          :read-only t
          :reader action)
  (data (error "windown event requires action data")
        :type list
        :read-only t
        :reader data))

;;----------------------------------------------------------------------
;; cepl mouse events

(defstruct+methods
    (cepl-mouse-event
     (:include cepl-backend-event
               (source-node |mouse|)))
  (mouse-id (error "mouse-scroll event requires mouse id")
            :type fixnum
            :read-only t
            :reader id))

(defstruct+methods (mouse-scroll (:include cepl-mouse-event))
  (vec (error "mouse-scroll event requires data")
       :type (simple-array single-float (2))
       :read-only t
       :reader cepl-generics::vec))

(defstruct+methods (mouse-button (:include cepl-mouse-event))
  (button (error "mouse-button event requires button name")
          :type keyword
          :read-only t
          :reader button)
  (state (error "mouse-button event requires state name")
         :type keyword
         :read-only t
         :reader state)
  (clicks (error "mouse-button event requires clicks count")
          :type fixnum
          :read-only t
          :reader clicks)
  (pos (error "mouse-button event requires position")
       :type (simple-array single-float (2))
       :read-only t
       :reader cepl-generics::pos))

(defstruct+methods (mouse-motion (:include cepl-mouse-event))
  (state (error "mouse-button event requires state name")
         :type keyword
         :read-only t
         :reader state)
  (delta (error "mouse-button event requires a delta")
         :type (simple-array single-float (2))
         :read-only t
         :reader delta)
  (pos (error "mouse-button event requires position")
       :type (simple-array single-float (2))
       :read-only t
       :reader cepl-generics::pos))

;;----------------------------------------------------------------------
;; cepl keyboard events

(defstruct+methods
    (cepl-keyboard-event
     (:include cepl-backend-event
               (source-node |keyboard|))))

(defstruct+methods (key (:include cepl-keyboard-event))
  (etype (error "mouse-button event requires etype name")
         :type keyword
         :read-only t
         :reader etype)
  (state (error "mouse-button event requires state name")
         :type keyword
         :read-only t
         :reader state)
  (repeating (error "mouse-button event requires repeating info")
             :type boolean
             :read-only t
             :reader repeating)
  (key (error "mouse-button event requires key name")
       :type keyword
       :read-only t
       :reader key))

;;======================================================================
;; Event Nodes

;;----------------------------------------------------------------------
;; cepl system events

(defvar |sys|
  (make-event-node
   :name 'cepl-internals
   :tags '(:cepl-internal :system)
   :filter #'will-quit-p
   :subscribe-to all-events))

;;----------------------------------------------------------------------
;; context events

(defvar |context|
  (make-event-node
   :name 'cepl-internals
   :tags '(:context)
   :filter #'context-created-p
   :subscribe-to all-events))

;;----------------------------------------------------------------------
;; cepl window events

(defvar |window|
  (make-event-node
   :name 'cepl-window
   :tags '(:window)
   :filter #'win-p
   :subscribe-to all-events))

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
       :subscribe-to all-events))))

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
       :subscribe-to all-events)))
  (defun key-state (key) (gethash key key-state :up)))
