(in-package :cepl.fbos)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; doco for mode-rgb and mode-alpha, I need doco files
;; "Choices are:
;;
;; :func-add - The source and destination colors are added to each other.
;;             O = sS + dD. The The s and d are blending parameters that are
;;             multiplied into each of S and D before the addition.
;;
;; :func-subtract - Subtracts the destination from the source. O = sS - dD.
;;                  The source and dest are again multiplied by blending
;;                  parameters.
;;
;; :func-reverse-subtract - Subtracts the source from the destination.
;;                          O = sD - dS. The source and dest are multiplied by
;;                          blending parameters.
;;
;; :min - The output color is the component-wise minimum value of the source
;;            and dest colors. So performing :min in the RGB equation means that
;;            Or = min(Sr, Dr), Og = min(Sg, Dg), and so forth.
;;            The parameters s and d are ignored for this equation.
;;
;; :max - The output color is the component-wise maximum value of the source and
;;        dest colors. The parameters s and d are ignored for this equation."

(defun mode-rgb (attachment)
  (typecase attachment
    (attachment (cepl.blending:with-blending-param-slots (:attachment attachment)
                  (mode-rgb attachment)))
    (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
           (mode-rgb attachment)))))

(defun mode-alpha (attachment)
  (typecase attachment
    (attachment (cepl.blending:with-blending-param-slots (:attachment attachment)
                  (mode-alpha attachment)))
    (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
           (mode-alpha attachment)))))

(defun source-rgb (attachment)
  (typecase attachment
    (attachment (cepl.blending:with-blending-param-slots (:attachment attachment)
                  (source-rgb attachment)))
    (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
           (source-rgb attachment)))))

(defun source-alpha (attachment)
  (typecase attachment
    (attachment (cepl.blending:with-blending-param-slots (:attachment attachment)
                  (source-alpha attachment)))
    (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
           (source-alpha attachment)))))

(defun destination-rgb (attachment)
  (typecase attachment
    (attachment (cepl.blending:with-blending-param-slots (:attachment attachment)
                  (destination-rgb attachment)))
    (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
           (destination-rgb attachment)))))

(defun destination-alpha (attachment)
  (typecase attachment
    (attachment (cepl.blending:with-blending-param-slots (:attachment attachment)
                  (destination-alpha attachment)))
    (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
           (destination-alpha attachment)))))

(defun blending (attachment)
  (%attachment-blending-enabled attachment))

(defun (setf blending) (value attachment)
  (setf (%attachment-blending-enabled attachment) (not (null value))))

(let ((major-v 0))
  (defun per-attachment-blending-available-p ()
    (when (= major-v 0) (setf major-v (cl-opengl:get* :major-version)))
    (>= major-v 4))
  (labels ((check-version-for-per-attachment-params ()
             (unless (per-attachment-blending-available-p)
               (error "You are currently using a v~s gl context, this doesn't support per attachment blend mode settings. You will only be able to change blend params on the first attachment. You can however enable blending on any number of attachments and they will inherit their params from attachment 0" (version-float *gl-context*)))))

    (defun (setf mode-rgb) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (cepl.blending:with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (mode-rgb attachment) value))))
        (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
               (setf (mode-rgb attachment) value)))))

    (defun (setf mode-alpha) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (cepl.blending:with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (mode-alpha attachment) value))))
        (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
               (setf (mode-alpha attachment) value)))))

    (defun (setf source-rgb) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (cepl.blending:with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (source-rgb attachment) value))))
        (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
               (setf (source-rgb attachment) value)))))

    (defun (setf source-alpha) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (cepl.blending:with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (source-alpha attachment) value))))
        (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
               (setf (source-alpha attachment) value)))))

    (defun (setf destination-rgb) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (cepl.blending:with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (destination-rgb attachment) value))))
        (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
               (setf (destination-rgb attachment) value)))))

    (defun (setf destination-alpha) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (cepl.blending:with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (destination-alpha attachment) value))))
        (fbo (cepl.blending:with-blending-param-slots (:fbo attachment)
               (setf (destination-alpha attachment) value)))))))
