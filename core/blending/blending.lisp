(in-package :cepl.blending)

(defun+ blending-params (fbo &optional attachment-name)
  (if attachment-name
      (attachment-blending fbo attachment-name)
      (%fbo-blending-params fbo)))

(defun+ (setf blending-params) (value fbo &optional attachment-name)
  (if attachment-name
      (setf (attachment-blending fbo attachment-name) value)
      (setf (%fbo-blending-params fbo) value)))

(defmacro with-blending-param-slots (blending-params &body body)
  (let ((g (gensym "bparams")))
    `(let ((,g ,blending-params))
       (symbol-macrolet
           ((mode-rgb (blending-params-mode-rgb ,g))
            (mode-alpha (blending-params-mode-alpha ,g))
            (source-rgb  (blending-params-source-rgb ,g))
            (source-alpha (blending-params-source-alpha ,g))
            (destination-rgb (blending-params-destination-rgb ,g))
            (destination-alpha (blending-params-destination-alpha ,g)))
         ,@body))))

(defvar *blend-color* (v! 0 0 0 0))

(defun+ blend-func-namep (keyword)
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

(defn current-blend-params () (or null blending-params)
  (declare (profile t))
  (with-cepl-context (ctx)
    (%with-cepl-context-slots (default-framebuffer) ctx
      (copy-blending-params
       (or %current-blend-params
           (attachment-blending default-framebuffer 0))))))

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
     `(let ((per-attachment-blendingp (per-attachment-blending-available-p))
            (%current-blend-params (blending-params ,fbo)))
        ;; if we dont support per attachemnt blend params then we use the
        ;; params from the fbo
        (when (not per-attachment-blendingp) (%blend-fbo ,fbo))
        ;; enable all the attachment that have requested blending
        (loop-enabling-attachments ,fbo)
        ;; if we support per attachment blending then we go set their params
        (when per-attachment-blendingp
          (%loop-setting-per-attachment-blend-params ,fbo))
        ;; the important bit :)
        ,@body
        ;; go disable all the attachments that were enabled
        (loop-disabling-attachments ,fbo)))
    ;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (t ;; We have a pattern that tells us which attachments will be drawn into
     ;;   This means we dont have to loop and search for attachments, so we
     ;;   can unroll the loop.
     (%gen-attachment-blend pattern fbo body))))

(defun+ %gen-attachment-blend (attachments fbo body)
  (let ((blendp-syms (cepl-utils:n-of* (gensym "attachment") (length attachments)))
        (override-syms (cepl-utils:n-of* (gensym "override")
                                         (length attachments)))
        (g (gensym)))
    ;; First we want to get all the lookups of attachment state done
    `(let* ,(cons
             `(%current-blend-params (%fbo-blending-params ,fbo))
             (loop :for a :in attachments :for b :in blendp-syms
                :for o :in override-syms :append
                `((,g (attachment-blending ,fbo ,a))
                  (,b (not (null ,g)))
                  (,o (when (blending-params-p ,g) ,g)))))
       (let ((per-attachment-blendingp (per-attachment-blending-available-p)))
         ;; If any of the attachments are inheriting the blending from the fbo
         ;; or if we cant provide per attachment blending, when we need to
         ;; use the fbo params
         (unless (or per-attachment-blendingp (and ,@override-syms))
           (%blend-fbo ,fbo))
         ;; Enable and set up any blend params needed
         (if per-attachment-blendingp
             (progn
               ,@(loop :for b :in blendp-syms :for o :in override-syms
                    :for i :from 0 :collect
                    `(when ,b
                       (%gl:enable-i :blend ,i)
                       (if ,o
                           (%blend-i ,o ,i)
                           (%blend-i (blending-params ,fbo) ,i)))))
             (progn
               ,@(loop :for b :in blendp-syms :for i :from 0 :collect
                    `(when ,b (%gl:enable-i :blend ,i))))))
       ;; The meat
       ,@body
       ;; go disable all the attachments that were enabled
       ,@(loop :for b :in blendp-syms :for i :from 0 :collect
            `(when ,b (%gl:disable-i :blend ,i))))))

(defun+ loop-enabling-attachments (fbo)
  (loop :for a :across (%fbo-color-arrays fbo) :for i :from 0 :do
     (when (att-blend a) (%gl:enable-i :blend i))))

(defun+ loop-disabling-attachments (fbo)
  (loop :for a :across (%fbo-color-arrays fbo) :for i :from 0 :do
     (when (att-blend a) (%gl:disable-i :blend i))))

(defun+ %loop-setting-per-attachment-blend-params (fbo)
  (loop :for a :across (%fbo-color-arrays fbo) :for i :from 0 :do
     (when (att-blend a)
       (if (att-bparams a)
           (%blend-i (att-bparams a) i)
           (%blend-i (%fbo-blending-params fbo) i)))))

(defun+ %blend-i (params i)
  (with-blending-param-slots params
    (%gl:blend-equation-separate-i i mode-rgb mode-alpha)
    (%gl:blend-func-separate-i
     i source-rgb destination-rgb source-alpha destination-alpha)))

(defun+ %blend-fbo (fbo)
  (%blend-using-params (%fbo-blending-params fbo)))

(defun+ %blend-using-params (params)
  (%gl:blend-equation-separate (blending-params-mode-rgb params)
                               (blending-params-mode-alpha params))
  (%gl:blend-func-separate (blending-params-source-rgb params)
                           (blending-params-destination-rgb params)
                           (blending-params-source-alpha params)
                           (blending-params-destination-alpha params)))


;;----------------------------------------------------------------------

(defun+ mode-rgb (fbo &optional attachment-name)
  (with-blending-param-slots (blending-params fbo attachment-name)
    mode-rgb))

(defun+ mode-alpha (fbo &optional attachment-name)
  (with-blending-param-slots (blending-params fbo attachment-name)
    mode-alpha))

(defun+ source-rgb (fbo &optional attachment-name)
  (with-blending-param-slots (blending-params fbo attachment-name)
    source-rgb))

(defun+ source-alpha (fbo &optional attachment-name)
  (with-blending-param-slots (blending-params fbo attachment-name)
    source-alpha))

(defun+ destination-rgb (fbo &optional attachment-name)
  (with-blending-param-slots (blending-params fbo attachment-name)
    destination-rgb))

(defun+ destination-alpha (fbo &optional attachment-name)
  (with-blending-param-slots (blending-params fbo attachment-name)
    destination-alpha))

(let ((major-v 0))
  (defun+ per-attachment-blending-available-p ()
    (when (= major-v 0) (setf major-v (cl-opengl:get* :major-version)))
    (>= major-v 4))
  (labels ((check-version-for-per-attachment-params ()
             (unless (per-attachment-blending-available-p)
               (error "You are currently using a v~s gl context, this doesn't support per attachment blend mode settings. You will only be able to change blend params on the first attachment. You can however enable blending on any number of attachments and they will inherit their params from attachment 0" (version-float *gl-context*)))))

    (defun+ (setf mode-rgb) (value fbo &optional attachment-name)
      (when attachment-name (check-version-for-per-attachment-params))
      (with-blending-param-slots (blending-params fbo attachment-name)
        (setf mode-rgb value)))

    (defun+ (setf mode-alpha) (value fbo &optional attachment-name)
      (when attachment-name (check-version-for-per-attachment-params))
      (with-blending-param-slots (blending-params fbo attachment-name)
        (setf mode-alpha value)))

    (defun+ (setf source-rgb) (value fbo &optional attachment-name)
      (when attachment-name (check-version-for-per-attachment-params))
      (with-blending-param-slots (blending-params fbo attachment-name)
        (setf source-rgb value)))

    (defun+ (setf source-alpha) (value fbo &optional attachment-name)
      (when attachment-name (check-version-for-per-attachment-params))
      (with-blending-param-slots (blending-params fbo attachment-name)
        (setf source-alpha value)))

    (defun+ (setf destination-rgb) (value fbo &optional attachment-name)
      (when attachment-name (check-version-for-per-attachment-params))
      (with-blending-param-slots (blending-params fbo attachment-name)
        (setf destination-rgb value)))

    (defun+ (setf destination-alpha) (value fbo &optional attachment-name)
      (when attachment-name (check-version-for-per-attachment-params))
      (with-blending-param-slots (blending-params fbo attachment-name)
        (setf destination-alpha value)))))


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
