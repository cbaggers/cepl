(in-package :cepl.context)

;;------------------------------------------------------------
;; Clear Color

(defun+ clear-color (cepl-context)
  (%with-cepl-context-slots (clear-color) cepl-context
    clear-color))

(defn (setf clear-color) ((vec4-color vec4) (cepl-context cepl-context))
    vec4
  (assert (typep vec4-color 'rtg-math.types:vec4))
  (%with-cepl-context-slots (clear-color) cepl-context
    (%gl:clear-color (v:x vec4-color) (v:y vec4-color)
                     (v:z vec4-color) (v:w vec4-color))
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
  (%gl:color-mask-i index (x value) (y value) (z value) (w value))
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
  (%gl:color-mask (x value) (y value) (z value) (w value))
  value)

;;------------------------------------------------------------
