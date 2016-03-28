(in-package :cepl.blending)

(defun blending-params (target)
  (typecase target
    (fbo (%fbo-blending-params target))
    (attachment (make-blending-params
                 :mode-rgb (mode-rgb target)
                 :mode-alpha (mode-alpha target)
                 :source-rgb (source-rgb target)
                 :source-alpha (source-alpha target)
                 :destination-rgb (destination-rgb target)
                 :destination-alpha (destination-alpha target)))))

(defun (setf blending-params) (value target)
  (typecase target
    (fbo (setf (%fbo-blending-params target) value))
    (attachment (setf (%attachment-blending-params target) value))))

(defmacro with-blending-param-slots ((&key fbo attachment) &body body)
  (cond
    (fbo
     `(macrolet
          ((mode-rgb (x)
	     `(blending-params-mode-rgb (%fbo-blending-params ,x)))
           (mode-alpha (x)
	     `(blending-params-mode-alpha (%fbo-blending-params ,x)))
           (source-rgb (x)
	     `(blending-params-source-rgb (%fbo-blending-params ,x)))
           (source-alpha (x)
	     `(blending-params-source-alpha (%fbo-blending-params ,x)))
           (destination-rgb (x) `(blending-params-destination-rgb
                                  (%fbo-blending-params ,x)))
           (destination-alpha (x) `(blending-params-destination-alpha
                                    (%fbo-blending-params ,x))))
        ,@body))
    (attachment
     `(macrolet
          ((mode-rgb (x) `(blending-params-mode-rgb
                           (%attachment-blending-params ,x)))
           (mode-alpha (x) `(blending-params-mode-alpha
                             (%attachment-blending-params ,x)))
           (source-rgb (x) `(blending-params-source-rgb
                             (%attachment-blending-params ,x)))
           (source-alpha (x) `(blending-params-source-alpha
                               (%attachment-blending-params ,x)))
           (destination-rgb (x) `(blending-params-destination-rgb
                                  (%attachment-blending-params ,x)))
           (destination-alpha (x) `(blending-params-destination-alpha
                                    (%attachment-blending-params ,x))))
        ,@body))))

(defvar *blend-color* (v! 0 0 0 0))

(defun blend-func-namep (keyword)
  (not (null (member keyword '(:zero
                               :one
                               :src-color
                               :one-minus-src-color
                               :dst-color
                               :one-minus-dst-color
                               :src-alpha
                               :one-minus-src-alpha
                               :dst-alpha
                               :one-minus-dst-alpha
                               :constant-color
                               :one-minus-constant-color
                               :constant-alpha
                               :one-minus-constant-alpha
                               :src-alpha-saturate
                               :src1-color
                               :one-minus-src1-color
                               :src1-alpha
                               :one-minus-src1-alpha)))))


;; We have another case to deal with. Per buffer blending params
;; is a >v4.0 feature, before that we could only enable for disable
;; blending per buffer.
;; Hmm, we need a flag for this in the attachment
;; see fbo.lisp's #'replace-attachment-array for the catch logic
;; hmm we need to draw down the permutations

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - can enable or disable blend per attachment
;; - >v4 can set params per attachment

;; default blend disable
;; if enabled, default param-override = nil

;; blend disabled - nothing to see here :)
;; blend enabled, no override - just call (gl:enable :blend *)
;; blend enabled, override - only valid >v4, (gl:enable :blend *) and then set
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar %current-blend-params nil)

(defun current-blend-params ()
  (copy-blending-params
   (or %current-blend-params
       (attachment-viewport (attachment %default-framebuffer 0)))))

(defmacro with-blending (blending-params &body body)
  (let ((b-params (gensym "blending-params")))
    `(let* ((,b-params ,blending-params))
       (%with-blending nil nil ,b-params
         ,@body))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; {TODO} Huge performance costs will be made here, unneccesary enable/disable
;;        all over the place. However will be VERY easy to fix with state-cache
;;        Do it.
(defmacro %with-blending (fbo pattern explicit-blend-params &body body)
  (assert (not (and explicit-blend-params (or fbo pattern))))
  (cond
    ;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ((and (null pattern) (null explicit-blend-params))
     (error "invalid blending pattern"))
    ;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (explicit-blend-params
     ;; The user wants blending to be set by a blending params struct
     `(let ((%current-blend-params ,explicit-blend-params))
        (%gl:enable :blend)
        (%blend-using-params ,explicit-blend-params)
        ,@body
        (%gl:disable :blend)))
    ((eq pattern t)
     ;; We cant, at compile time, tell which attachments will be used so loop
     ;; through the attachment list and set everything up
     `(let ((attachments (%fbo-attachment-color ,fbo))
            (per-attachment-blendingp (per-attachment-blending-available-p))
            (%current-blend-params (%fbo-blending-params ,fbo)))
        ;; if we dont support per attachemnt blend params then we use the
        ;; params from the fbo
        (when (not per-attachment-blendingp) (%blend-fbo ,fbo))
        ;; enable all the attachment that have requested blending
        (%loop-enabling-attachments attachments)
        ;; if we support per attachment blending then we go set their params
        (when per-attachment-blendingp
          (%loop-setting-per-attachment-blend-params ,fbo))
        ;; the important bit :)
        ,@body
        ;; go disable all the attachments that were enabled
        (%loop-disabling-attachments attachments)))
    ;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (t ;; We have a pattern that tells us which attachments will be drawn into
     ;;   This means we dont have to loop and search for attachments, so we
     ;;   can unroll the loop.
     (%gen-attachment-blend pattern fbo body))))

(defun %gen-attachment-blend (attachments fbo body)
  (let ((a-syms (cepl-utils:n-of* (gensym "attachment") (length attachments)))
        (override-syms (cepl-utils:n-of* (gensym "override")
					 (length attachments))))
    ;; First we want to get all the lookups of attachment state done
    `(let* ,(cons
             `(%current-blend-params (%fbo-blending-params ,fbo))
             (loop :for a :in attachments :for s :in a-syms
                :for o :in override-syms :append
                `((,s (%attachment ,fbo ,a))
                  (,o (%attachment-override-blending ,s)))))
       (let ((per-attachment-blendingp (per-attachment-blending-available-p)))
         ;; If any of the attachments are inheriting the blending from the fbo
         ;; or if we cant provide per attachment blending, when we need to
         ;; use the fbo params
         (unless (or per-attachment-blendingp (and ,@override-syms))
           (%blend-fbo ,fbo))
         ;; Enable and set up any blend params needed
         (if per-attachment-blendingp
             (progn
               ,@(loop :for a :in a-syms :for o :in override-syms
                    :for i :from 0 :collect
                    `(when (blending ,a)
                       (%gl:enable-i :blend ,i)
                       (if ,o
                           (%blend-attachment-i ,a ,i)
                           (%blend-fbo-i ,fbo ,i)))))
             (progn
               ,@(loop :for a :in a-syms :for i :from 0 :collect
                    `(when (blending ,a) (%gl:enable-i :blend ,i))))))
       ;; The meat
       ,@body
       ;; go disable all the attachments that were enabled
       ,@(loop :for a :in a-syms :for i :from 0 :collect
            `(when (blending ,a) (%gl:disable-i :blend ,i))))))

(defmacro %loop-enabling-attachments (attachments)
  `(loop :for a :across ,attachments :for i :from 0 :do
      (when (blending a) (%gl:enable-i :blend i))))

(defmacro %loop-disabling-attachments (attachments)
  `(loop :for a :across ,attachments :for i :from 0 :do
      (when (blending a) (%gl:disable-i :blend i))))

(defun %loop-setting-per-attachment-blend-params (fbo)
  (loop :for a :across (%fbo-attachment-color fbo) :for i :from 0 :do
     (when (blending a)
       (if (%attachment-override-blending a)
           (%blend-attachment-i a i)
           (%blend-fbo-i fbo i)))))

(defun %blend-fbo (fbo)
  (%blend-using-params (%fbo-blending-params fbo)))

(defun %blend-using-params (params)
  (%gl:blend-equation-separate (blending-params-mode-rgb params)
                               (blending-params-mode-alpha params))
  (%gl:blend-func-separate (blending-params-source-rgb params)
                           (blending-params-destination-rgb params)
                           (blending-params-source-alpha params)
                           (blending-params-destination-alpha params)))

(defun %blend-fbo-i (fbo i)
  (with-blending-param-slots (:fbo fbo)
    (%gl:blend-equation-separate-i i (mode-rgb fbo) (mode-alpha fbo))
    (%gl:blend-func-separate-i i
                               (source-rgb fbo)
                               (destination-rgb fbo)
                               (source-alpha fbo)
                               (destination-alpha fbo))))

(defun %blend-attachment-i (attachment i)
  (with-blending-param-slots (:attachment attachment)
    (%gl:blend-equation-separate-i i (mode-rgb attachment)
                                   (mode-alpha attachment))
    (%gl:blend-func-separate-i i
                               (source-rgb attachment)
                               (destination-rgb attachment)
                               (source-alpha attachment)
                               (destination-alpha attachment))))


;;----------------------------------------------------------------------

(defun mode-rgb (attachment)
  (typecase attachment
    (attachment (with-blending-param-slots (:attachment attachment)
                  (mode-rgb attachment)))
    (fbo (with-blending-param-slots (:fbo attachment)
           (mode-rgb attachment)))))

(defun mode-alpha (attachment)
  (typecase attachment
    (attachment (with-blending-param-slots (:attachment attachment)
                  (mode-alpha attachment)))
    (fbo (with-blending-param-slots (:fbo attachment)
           (mode-alpha attachment)))))

(defun source-rgb (attachment)
  (typecase attachment
    (attachment (with-blending-param-slots (:attachment attachment)
                  (source-rgb attachment)))
    (fbo (with-blending-param-slots (:fbo attachment)
           (source-rgb attachment)))))

(defun source-alpha (attachment)
  (typecase attachment
    (attachment (with-blending-param-slots (:attachment attachment)
                  (source-alpha attachment)))
    (fbo (with-blending-param-slots (:fbo attachment)
           (source-alpha attachment)))))

(defun destination-rgb (attachment)
  (typecase attachment
    (attachment (with-blending-param-slots (:attachment attachment)
                  (destination-rgb attachment)))
    (fbo (with-blending-param-slots (:fbo attachment)
           (destination-rgb attachment)))))

(defun destination-alpha (attachment)
  (typecase attachment
    (attachment (with-blending-param-slots (:attachment attachment)
                  (destination-alpha attachment)))
    (fbo (with-blending-param-slots (:fbo attachment)
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
                      (with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (mode-rgb attachment) value))))
        (fbo (with-blending-param-slots (:fbo attachment)
               (setf (mode-rgb attachment) value)))))

    (defun (setf mode-alpha) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (mode-alpha attachment) value))))
        (fbo (with-blending-param-slots (:fbo attachment)
               (setf (mode-alpha attachment) value)))))

    (defun (setf source-rgb) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (source-rgb attachment) value))))
        (fbo (with-blending-param-slots (:fbo attachment)
               (setf (source-rgb attachment) value)))))

    (defun (setf source-alpha) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (source-alpha attachment) value))))
        (fbo (with-blending-param-slots (:fbo attachment)
               (setf (source-alpha attachment) value)))))

    (defun (setf destination-rgb) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (destination-rgb attachment) value))))
        (fbo (with-blending-param-slots (:fbo attachment)
               (setf (destination-rgb attachment) value)))))

    (defun (setf destination-alpha) (value attachment)
      (typecase attachment
        (attachment (progn
                      (check-version-for-per-attachment-params)
                      (with-blending-param-slots (:attachment attachment)
                        (setf (%attachment-override-blending attachment) t
                              (destination-alpha attachment) value))))
        (fbo (with-blending-param-slots (:fbo attachment)
               (setf (destination-alpha attachment) value)))))))


;;----------------------------------------------------------------------

;; functions below were written to help me understand the blending process
;; they are not something to use in attachments. I'm not sure how to expose
;; these (or if I should). I like the idea of cpu side debugging using this
;; but in issolation it doesnt really mean much. Probably only makes sense in
;; a software renderer.

;; (defun zero
;;     (source destination &key (target-rgb t) (blend-color *blend-color*))
;;   (declare (ignore source destination blend-color))
;;   (if target-rgb
;;       (v! 0 0 0)
;;       0))

;; (defun one
;;     (source destination &key (target-rgb t) (blend-color *blend-color*))
;;   (declare (ignore source destination blend-color))
;;   (if target-rgb
;;       (v! 1 1 1)
;;       1))

;; (defun src-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:s~ source :xyz))
;;       (* (v:w (if target-source source destination))
;;          (v:w source))))

;; (defun one-minus-src-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v:s~ source :xyz)))
;;       (* (v:w (if target-source source destination))
;;          (- 1 (v:w source)))))

;; (defun dst-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:s~ destination :xyz))
;;       (* (v:w (if target-source source destination))
;;          (v:w destination))))

;; (defun one-minus-dst-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v:s~ destination :xyz)))
;;       (* (v:w (if target-source source destination))
;;          (- 1 (v:w destination)))))

;; (defun src-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v! (v:w source) (v:w source) (v:w source)))
;;       (* (v:w (if target-source source destination))
;;          (v:w source))))

;; (defun one-minus-src-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v! (v:w source) (v:w source) (v:w source))))
;;       (* (v:w (if target-source source destination))
;;          (- 1 (v:w source)))))

;; (defun dst-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v! (v:w destination) (v:w destination) (v:w destination)))
;;       (* (v:w (if target-source source destination))
;;          (v:w destination))))

;; (defun one-minus-dst-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v! (v:w destination) (v:w destination) (v:w destination))))
;;       (* (v:w (if target-source source destination))
;;          (v:w destination))))

;; (defun constant-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:s~ blend-color :xyz))
;;       (* (v:w (if target-source source destination))
;;          (v:w blend-color))))

;; (defun one-minus-constant-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v:s~ blend-color :xyz)))
;;       (* (v:w (if target-source source destination))
;;          (- 1 (v:w blend-color)))))

;; (defun constant-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore ))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v! (v:w blend-color) (v:w blend-color) (v:w blend-color)))
;;       (* (v:w (if target-source source destination))
;;          (v:w blend-color))))

;; (defun one-minus-constant-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v! (v:w blend-color) (v:w blend-color) (v:w blend-color))))
;;       (* (v:w (if target-source source destination))
;;          (- 1 (v:w blend-color)))))

;; ;; Destination color multiplied by the minimum of the source and (1 – destination)
;; (defun src-alpha-saturate
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*))
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (let ((factor (min (v:w source) (- 1 (v:w destination)))))
;;              (v! factor factor factor)))
;;       (* (v:w (if target-source source destination))
;;          1)))

;; (defun src1-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*) source-2)
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:s~ source-2 :xyz))
;;       (* (v:w (if target-source source destination))
;;          (v:w source-2))))

;; (defun one-minus-src1-color
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*) source-2)
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v:s~ source-2 :xyz)))
;;       (* (v:w (if target-source source destination))
;;          (- 1 (v:w source-2)))))

;; (defun src1-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*) source-2)
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v! (v:w source-2) (v:w source-2) (v:w source-2)))
;;       (* (v:w (if target-source source destination))
;;          (v:w source-2))))

;; (defun one-minus-src1-alpha
;;     (source destination &key (target-rgb t) (target-source t)
;;                           (blend-color *blend-color*) source-2)
;;   (declare (ignore blend-color))
;;   (if target-rgb
;;       (v:* (v:s~ (if target-source source destination) :xyz)
;;            (v:- (v! 1 1 1) (v! (v:w source-2) (v:w source-2) (v:w source-2))))
;;       (* (v:w (if target-source source destination))
;;          (- 1 (v:w source-2)))))
