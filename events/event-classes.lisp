(in-package #:cepl.events)

;;--------------------------------------------
;; Lisp events

(defclass cepl-event ()
  ((timestamp :initform 0 :initarg :timestamp :reader timestamp
              :type fixnum)))

(defclass will-quit (cepl-event) ())

(defclass win (cepl-event)
  ((action :initarg :action :initform 0 :reader action
           :type keyword)
   (data :initarg :data :reader data :type list)))

(defclass mouse-scroll (cepl-event)
  ((source-id :initarg :source-id :initform 0 :reader id
              :type fixnum)
   (vec :initarg :vec :type (simple-array (single-float 3)))))

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
   (pos :initarg :pos :type (simple-array (single-float 3)))))

(defmethod cepl-generics::pos ((event mouse-button))
  (slot-value event 'pos))

(defclass mouse-motion (cepl-event)
  ((source-id :initarg :source-id :initform 0 :reader id
              :type fixnum)
   (state :initarg :state :initform 0 :reader state
          :type fixnum)
   (pos :initarg :pos :type (simple-array (single-float 3)))
   (delta :initarg :delta :reader delta
          :type (simple-array (single-float 3)))))

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

;;--------------------------------------------
;; Event funcs
;;--------------------------------------------

(defun mouse0-eventp (x)
  (or (and (typep x 'mouse-scroll) (= (id x) 0))
      (and (typep x 'mouse-button) (= (id x) 0))
      (and (typep x 'mouse-motion) (= (id x) 0))))
