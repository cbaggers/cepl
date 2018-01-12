(in-package :cepl.context)

;;------------------------------------------------------------
;; Clear Color

(defun+ clear-color (&optional (cepl-context (cepl-context)))
  (%with-cepl-context-slots (clear-color) cepl-context
    clear-color))

(defn (setf clear-color)
    ((vec4-color vec4) &optional (cepl-context cepl-context (cepl-context)))
    vec4
  (assert (typep vec4-color 'vec4))
  (%with-cepl-context-slots (clear-color) cepl-context
    (%gl:clear-color (aref vec4-color 0)
                     (aref vec4-color 1)
                     (aref vec4-color 2)
                     (aref vec4-color 3))
    (setf clear-color vec4-color)))

;;------------------------------------------------------------
;; Color Mask


(define-context-func color-mask ((index (unsigned-byte 16)))
    (simple-array boolean (4))
    (color-masks)
  (aref color-masks index))

(define-context-func (setf color-mask) ((value (simple-array boolean (4)))
                                        (index (unsigned-byte 16)))
    (simple-array boolean (4))
    (color-masks)
  (setf (aref color-masks index) value)
  (%gl:color-mask-i index
                    (aref value 0)
                    (aref value 1)
                    (aref value 2)
                    (aref value 3))
  value)

(define-context-func color-masks ()
    (simple-array (simple-array boolean (4)) (*))
    (color-masks)
  color-masks)

(define-context-func (setf color-masks) ((value (simple-array boolean (4))))
    (simple-array boolean (4))
    (color-masks)
  (loop :for i :below (length color-masks) :do
     (setf (aref color-masks i) value))
  (%gl:color-mask (aref value 0)
                  (aref value 1)
                  (aref value 2)
                  (aref value 3))
  value)

;;------------------------------------------------------------
