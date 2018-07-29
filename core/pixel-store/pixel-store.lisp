(in-package :cepl.context)

;;----------------------------------------------------------------------
;; Pixel Store Parameters

(define-context-func unpack-alignment ()
    (integer 1 8)
    (unpack-alignment)
  unpack-alignment)

(define-context-func (setf unpack-alignment)
    ((row-alignment (integer 1 8))
     &optional (force boolean))
    (integer 1 8)
    (unpack-alignment)
  (when (or force (/= row-alignment unpack-alignment))
    (%gl:pixel-store-i #.(gl-enum :unpack-alignment) row-alignment)
    (setf unpack-alignment row-alignment))
  row-alignment)

(define-context-func pack-alignment ()
    (integer 1 8)
    (pack-alignment)
  pack-alignment)

(define-context-func (setf pack-alignment) ((row-alignment (integer 1 8))
                                            &optional (force boolean))
    (integer 1 8)
    (pack-alignment)
  (when (or force (/= row-alignment pack-alignment))
    (%gl:pixel-store-i #.(gl-enum :pack-alignment) row-alignment)
    (setf pack-alignment row-alignment))
  row-alignment)

;; Dont need these yet, but the code was easy so here they are

;; (define-context-func unpack-swap-bytes ()
;;     boolean
;;     (unpack-swap-bytes)
;;   unpack-swap-bytes)

;; (define-context-func (setf unpack-swap-bytes) ((row-swap-bytes boolean))
;;     boolean
;;     (unpack-swap-bytes)
;;   (%gl:pixel-store-i #.(gl-enum :unpack-swap-bytes) row-swap-bytes)
;;   (setf unpack-swap-bytes row-swap-bytes))

;; (define-context-func pack-swap-bytes ()
;;     boolean
;;     (pack-swap-bytes)
;;   pack-swap-bytes)

;; (define-context-func (setf pack-swap-bytes) ((row-swap-bytes boolean))
;;     boolean
;;     (pack-swap-bytes)
;;   (%gl:pixel-store-i #.(gl-enum :pack-swap-bytes) row-swap-bytes)
;;   (setf pack-swap-bytes row-swap-bytes))


;; (define-context-func unpack-lsb-first ()
;;     boolean
;;     (unpack-lsb-first)
;;   unpack-lsb-first)

;; (define-context-func (setf unpack-lsb-first) ((row-lsb-first boolean))
;;     boolean
;;     (unpack-lsb-first)
;;   (%gl:pixel-store-i #.(gl-enum :unpack-lsb-first) row-lsb-first)
;;   (setf unpack-lsb-first row-lsb-first))

;; (define-context-func pack-lsb-first ()
;;     boolean
;;     (pack-lsb-first)
;;   pack-lsb-first)

;; (define-context-func (setf pack-lsb-first) ((row-lsb-first boolean))
;;     boolean
;;     (pack-lsb-first)
;;   (%gl:pixel-store-i #.(gl-enum :pack-lsb-first) row-lsb-first)
;;   (setf pack-lsb-first row-lsb-first))



;; (define-context-func unpack-row-length ()
;;     c-array-index
;;     (unpack-row-length)
;;   unpack-row-length)

;; (define-context-func (setf unpack-row-length) ((row-row-length c-array-index))
;;     c-array-index
;;     (unpack-row-length)
;;   (%gl:pixel-store-i #.(gl-enum :unpack-row-length) row-row-length)
;;   (setf unpack-row-length row-row-length))

;; (define-context-func pack-row-length ()
;;     c-array-index
;;     (pack-row-length)
;;   pack-row-length)

;; (define-context-func (setf pack-row-length) ((row-row-length c-array-index))
;;     c-array-index
;;     (pack-row-length)
;;   (%gl:pixel-store-i #.(gl-enum :pack-row-length) row-row-length)
;;   (setf pack-row-length row-row-length))


;; (define-context-func unpack-image-height ()
;;     c-array-index
;;     (unpack-image-height)
;;   unpack-image-height)

;; (define-context-func (setf unpack-image-height) ((row-image-height c-array-index))
;;     c-array-index
;;     (unpack-image-height)
;;   (%gl:pixel-store-i #.(gl-enum :unpack-image-height) row-image-height)
;;   (setf unpack-image-height row-image-height))

;; (define-context-func pack-image-height ()
;;     c-array-index
;;     (pack-image-height)
;;   pack-image-height)

;; (define-context-func (setf pack-image-height) ((row-image-height c-array-index))
;;     c-array-index
;;     (pack-image-height)
;;   (%gl:pixel-store-i #.(gl-enum :pack-image-height) row-image-height)
;;   (setf pack-image-height row-image-height))

;;----------------------------------------------------------------------
