(in-package :cepl.context)

;;------------------------------------------------------------
;; Depth Range

(define-context-func depth-range-vec2 () vec2
    (depth-range)
  depth-range)

(define-context-func (setf depth-range-vec2) ((vec2-range vec2)) vec2
    (depth-range)
  (assert (typep vec2-range 'vec2))
  (%gl:depth-range (float (aref vec2-range 0) 0d0)
                   (float (aref vec2-range 1) 0d0))
  (setf depth-range vec2-range))

;;------------------------------------------------------------
;; Depth Clamp

(define-context-func depth-clamp () boolean
    (depth-clamp)
  depth-clamp)

(define-context-func (setf depth-clamp) ((value boolean)) boolean
    (depth-clamp)
  (if value
      (gl:enable :depth-clamp)
      (gl:disable :depth-clamp))
  (setf depth-clamp value))

;;------------------------------------------------------------
;; Depth Mask

(define-context-func depth-mask () boolean
    (depth-mask)
  depth-mask)

(define-context-func (setf depth-mask) ((value boolean)) boolean
    (depth-mask)
  (if value
      (%gl:depth-mask t)
      (%gl:depth-mask nil))
  (setf depth-mask value))

;;------------------------------------------------------------
;; Depth Test

(define-context-func depth-test-function () (or symbol function)
    (depth-func)
  depth-func)

(define-context-func (setf depth-test-function) ((function (or symbol function)))
    (or symbol function)
    (depth-func)
  (if function
      (progn
        (gl:enable :depth-test)
        (cond
          ;;
          ((eq function depth-func) depth-func)
          ((or (eq function ':never) (eq function #'never))
           (%gl:depth-func #.(gl-enum :never))
           (setf depth-func #'never))
          ;;
          ((or (eq function ':less) (eq function #'<))
           (%gl:depth-func #.(gl-enum :less))
           (setf depth-func #'<))
          ;;
          ((or (eq function ':equal) (eq function #'=))
           (%gl:depth-func #.(gl-enum :equal))
           (setf depth-func #'=))
          ;;
          ((or (eq function ':lequal) (eq function #'<=))
           (%gl:depth-func #.(gl-enum :lequal))
           (setf depth-func #'<=))
          ;;
          ((or (eq function ':greater) (eq function #'>))
           (%gl:depth-func #.(gl-enum :greater))
           (setf depth-func #'>))
          ;;
          ((or (eq function ':notequal) (eq function #'/=))
           (%gl:depth-func #.(gl-enum :notequal))
           (setf depth-func #'/=))
          ;;
          ((or (eq function ':gequal) (eq function #'>=))
           (%gl:depth-func #.(gl-enum :gequal))
           (setf depth-func #'>=))
          ;;
          ((or (eq function ':always) (eq function #'always))
           (%gl:depth-func #.(gl-enum :always))
           (setf depth-func #'always))
          (t (error "CEPL: Invalid function for depth-test-function: ~a"
                    function))))
      ;; function was nil
      (progn
        (gl:disable :depth-test)
        (setf depth-func nil))))

(define-context-func force-depth-test-function ((function (or null function)))
    (or symbol function)
    (depth-func)
  (if function
      (progn
        (gl:enable :depth-test)
        (cond
          ;;
          ((eq function #'never)
           (%gl:depth-func #.(gl-enum :never))
           (setf depth-func #'never))
          ;;
          ((eq function #'<)
           (%gl:depth-func #.(gl-enum :less))
           (setf depth-func #'<))
          ;;
          ((eq function #'=)
           (%gl:depth-func #.(gl-enum :equal))
           (setf depth-func #'=))
          ;;
          ((eq function #'<=)
           (%gl:depth-func #.(gl-enum :lequal))
           (setf depth-func #'<=))
          ;;
          ((eq function #'>)
           (%gl:depth-func #.(gl-enum :greater))
           (setf depth-func #'>))
          ;;
          ((eq function #'/=)
           (%gl:depth-func #.(gl-enum :notequal))
           (setf depth-func #'/=))
          ;;
          ((eq function #'>=)
           (%gl:depth-func #.(gl-enum :gequal))
           (setf depth-func #'>=))
          ;;
          ((eq function #'always)
           (%gl:depth-func #.(gl-enum :always))
           (setf depth-func #'always))
          (t (error "CEPL: Invalid function for depth-test-function: ~a"
                    function))))
      ;; function was nil
      (progn
        (gl:disable :depth-test)
        (setf depth-func nil))))

;;------------------------------------------------------------
