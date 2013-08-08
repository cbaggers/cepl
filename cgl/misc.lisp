(in-package :cgl)

(defun free-managed-resources ()
  (free-all-vaos-in-pool)
  (free-all-buffers-in-pool))

;; [TODO] There can be only one!!
(defun lispify-name (name)
  "take a string and changes it to uppercase and replaces
   all underscores _ with minus symbols -"
  (string-upcase (substitute #\- #\_ name)))


(defun gpu! (type &rest values)
  (if (and (not values) (typep type 'cgl::c-array))
      (make-gpu-array type)
      (make-gpu-array values :element-type type)))

(defun gl! (type &rest values)
  (make-c-array type (length values) values))
