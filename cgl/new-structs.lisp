(in-package :cgl)

(defclass gl-struct-slot ()
  ((name :initarg :name :getter slot-name) 
   (type :initarg :type :getter slot-type) 
   (normalised :initarg :normalised :getter slot-normalisedp) 
   (getter :initarg :getter slot-getter)
   (setter :initarg :setter slot-setter)))

;;{TODO} proper errors here
(defmacro defglstruct (name (&key option) &body slot-descriptions)  
  (let ((slots (mapcar #'normalize-slot-description slot-descriptions)))
    (unless (valid-defglstruct-form) (error "invalid defglstruct form"))
    `(progn
       ,(make-varjo-struct-def name slots)
       ,(make-cstruct-def name slots)
       ,(make-autowrap-record-def name slots)
       ,@(remove nil (mapcar #'make-slot-getter slots))
       ,@(remove nil (mapcar #'make-slot-setter slots))
       ,(make-populate slots)
       )))


(defun normalize-slot-description (slot-description)
  slot-description)

(defun valid-defglstruct-form (name options slots)
  t)

;;------------------------------------------------------------

(defun make-varjo-struct-def (name slots)
  `(v-defstruct ,name () ,@(mapcar #'format-slot-for-varjo)))

(defun format-slot-for-varjo (slot)
  slot)

;;------------------------------------------------------------

(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     (mapcar #'format-slot-for-cstruct slots)))

(defun format-slot-for-cstruct (slot)
  (slot))

;;------------------------------------------------------------

(defun make-autowrap-def (name slots)
  `(&&&&&&&&&&&&&&&&&&&&&&&&&&&
    name
     (mapcar #'format-slot-for-autowrap slots)))

(defun format-slot-for-autowrap (slot)
  (slot))
