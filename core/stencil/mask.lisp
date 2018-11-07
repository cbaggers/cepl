(in-package :cepl.context)

;;------------------------------------------------------------
;; Stencil Mask

(defn-inline %stencil-mask ((face symbol)
                            (cepl-context cepl-context))
    (values stencil-mask (or null stencil-mask))
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (%with-cepl-context-slots (current-stencil-mask-front
                             current-stencil-mask-back)
      cepl-context
    (ecase face
      (:front (values current-stencil-mask-front nil))
      (:back (values current-stencil-mask-back nil))
      (:front-and-back (values current-stencil-mask-front
                               current-stencil-mask-back)))))

(defn stencil-mask ((face symbol)
                    &optional (cepl-context cepl-context (cepl-context)))
    (values stencil-mask (or null stencil-mask))
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline %stencil-mask)
           (profile t))
  (%stencil-mask face cepl-context))

(define-compiler-macro stencil-mask
    (face &optional cepl-context)
  (if cepl-context
      `(%stencil-mask ,face ,cepl-context)
      `(%stencil-mask ,face (cepl-context))))

;;------------------------------------------------------------

(defn (setf stencil-mask) ((mask stencil-mask)
                           (face symbol)
                           &optional (cepl-context cepl-context (cepl-context)))
    stencil-mask
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (let ((enum (ecase face
                (:front #.(gl-enum :front))
                (:back #.(gl-enum :back))
                (:front-and-back #.(gl-enum :front-and-back)))))
    (%apply-stencil-mask enum mask cepl-context))
  mask)

(define-compiler-macro (setf stencil-mask)
    (&whole whole mask face &optional cepl-context)
  (let ((enum (case face
                (:front #.(gl-enum :front))
                (:back #.(gl-enum :back))
                (:front-and-back #.(gl-enum :front-and-back))
                (otherwise face))))
    (cond
      ((symbolp enum) whole)

      (cepl-context `(%apply-stencil-mask
                      ,enum ,mask ,cepl-context))

      (t `(%apply-stencil-mask
           ,enum ,mask (cepl-context))))))

(defn %apply-stencil-mask ((face (signed-byte 32))
                           (mask stencil-mask)
                           (cepl-context cepl-context))
    stencil-mask
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (profile t))
  (%with-cepl-context-slots (current-stencil-mask-front
                             current-stencil-mask-back)
      cepl-context
    ;;
    ;; update context
    (cond
      ((= face #.(gl-enum :front))
       (unless (= mask current-stencil-mask-front)
         (gl:stencil-mask-separate face mask)
         (setf current-stencil-mask-front mask)))
      ((= face #.(gl-enum :back))
       (unless (= mask current-stencil-mask-back)
         (gl:stencil-mask-separate face mask)
         (setf current-stencil-mask-back mask)))
      (t (setf current-stencil-mask-front mask)
         (setf current-stencil-mask-back mask)
         (gl:stencil-mask-separate face mask))))
  mask)
