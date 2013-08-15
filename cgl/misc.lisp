(in-package :cgl)

(defun free-managed-resources ()
  (free-all-vaos-in-pool)
  (free-all-buffers-in-pool))

;; [TODO] There can be only one!!
(defun lispify-name (name)
  "take a string and changes it to uppercase and replaces
   all underscores _ with minus symbols -"
  (string-upcase (substitute #\- #\_ name)))

(defun cls ()
  (clear :color-buffer-bit))
