(in-package :cepl.boxes)

(defmethod print-object ((obj box) stream)
  (format stream "#<BOX ~a>"
          (c-array-element-type (box-data obj))))

;; (make-ubo 'bar :last-slot-length 50)
;; (make-ssbo 'bar :last-slot-length 50)
;; (make-box 'bar :last-slot-length 50)

(defun make-box (type &key last-slot-length)
  (let* ((trailing-element-size
          (cepl.types::unsized-slot-element-byte-size type))
         (trailing-size
          (if trailing-element-size
              (* last-slot-length trailing-element-size)
              (error "~s does not have a unsized array in the last slot"
                     type)))
         (data (cepl.c-arrays::make-c-array-internal
                nil 1 type 1 trailing-size)))
    (%make-box :data data
               :index 0
               :owns-c-array t
               :last-slot-length last-slot-length)))

(defun box-value (box)
  (aref-c (box-data box) 0))

(defun box-value-type (box)
  (c-array-element-type (box-data box)))

:box-value
:box-value-type

;; (make-ubo-from some-gpu-array 500)
;; (make-ssbo-from some-gpu-array 500)
;; (make-box-from some-c-array 500)

(defun make-box-from (c-array &optional (index 0))
  ;; {TODO} assert c-array is 1d
  (%make-box :data c-array
             :index index
             :owns-c-array nil
             :last-slot-length 0))

(defmethod pull-g ((obj box))
  (pull-g (aref-c (box-data obj) (box-index obj))))

(defmethod pull1-g ((obj box))
  (pull1-g (aref-c (box-data obj) (box-index obj))))

(defmethod push-g ((data list) (dst box))
  (push-g data (aref-c (box-data dst) (box-index dst))))

(defun box-data-type (box)
  (c-array-element-type (box-data box)))
