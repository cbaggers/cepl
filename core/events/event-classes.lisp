(in-package #:cepl.events)

;;--------------------------------------------
;; Lisp events

(defvar *event-class-names*
  '(will-quit win mouse-scroll mouse-button mouse-motion key))

;; (defmacro evt:case-events (event &body event-handlers)
;;   (assert (and (symbolp event) (not (keywordp event))))
;;   `(let ((events (collect-sdl-events)))
;;      (loop :for ,event :in events :do
;;         (typecase ,event
;;           ,@(loop :for form :in event-handlers :collect
;;                (let ((type-name (first form)))
;;                  (assert (and (symbolp type-name)
;;                               (member type-name evt::*event-class-names*)))
;;                  `(,(symb-package :evt type-name) ,@(rest form))))))))

(defclass cepl-event ()
  ((timestamp :initform 0 :initarg :timestamp :reader timestamp
              :type fixnum)))

(defclass context-created (cepl-event) ())

(defclass will-quit (cepl-event) ())

(defclass win (cepl-event)
  ((action :initarg :action :initform 0 :reader action
           :type keyword)
   (data :initarg :data :reader data :type list)))

(defclass mouse-scroll (cepl-event)
  ((source-id :initarg :source-id :initform 0 :reader id
              :type fixnum)
   (vec :initarg :vec :type (simple-array single-float (3)))))

(defmethod cepl-generics::vec ((event mouse-scroll))
  (slot-value event 'vec))

(defclass mouse-button (cepl-event)
  ((source-id :initarg :source-id :initform 0 :reader id
              :type fixnum)
   (button :initarg :button :initform 0 :reader button
           :type keyword)
   (state :initarg :state :initform 0 :reader state
          :type keyword)
   (clicks :initarg :clicks :initform 0 :reader clicks
           :type fixnum)
   (pos :initarg :pos :type (simple-array single-float (3)))))

(defmethod cepl-generics::pos ((event mouse-button))
  (slot-value event 'pos))

(defclass mouse-motion (cepl-event)
  ((source-id :initarg :source-id :initform 0 :reader id
              :type fixnum)
   (state :initarg :state :initform 0 :reader state
          :type fixnum)
   (pos :initarg :pos :type (simple-array single-float (3)))
   (delta :initarg :delta :reader delta
          :type (simple-array single-float (3)))))

(defmethod cepl-generics::pos ((event mouse-motion))
  (slot-value event 'pos))

(defclass key (cepl-event)
  ((etype :initarg :etype :reader etype
          :type keyword)
   (state :initarg :state :initform 0  :reader state
          :type keyword)
   (repeating :initarg :repeating :initform 0 :reader repeating
           :type boolean)
   (key :initarg :key :initform 0 :reader key
        :type keyword)))
