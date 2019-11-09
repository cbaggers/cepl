(in-package :cepl.boxes)

(defmethod print-object ((obj box) stream)
  (format stream "#<BOX ~a>"
          (c-array-element-type (box-data obj))))

;; (make-ubo 'bar :last-slot-length 50)
;; (make-ssbo 'bar :last-slot-length 50)
;; (make-box 'bar :last-slot-length 50)

(defun make-box (type &key last-slot-length)
  (let ((data (make-c-array nil :dimensions 1
                            :element-type type)))
    (%make-box :data data
               :index 0
               :owns-c-array t)))

;; (make-ubo-from some-gpu-array 500)
;; (make-ssbo-from some-gpu-array 500)
;; (make-box-from some-c-array 500)

(defun make-box-from (c-array &optional (index 0))
  ;; {TODO} assert c-array is 1d
  (%make-box :data c-array
             :index index
             :owns-c-array nil))

(defmethod pull-g ((obj box))
  (pull-g (aref-c (box-data obj) (box-index obj))))

(defmethod pull1-g ((obj box))
  (pull1-g (aref-c (box-data obj) (box-index obj))))

(defmethod push-g ((data list) (dst box))
  (push-g data (aref-c (box-data dst) (box-index dst))))

(defun box-data-type (box)
  (c-array-element-type (box-data box)))
