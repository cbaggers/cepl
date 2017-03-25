(in-package :cepl.host)

;;============================================================
;; API 1
;;============================================================

(defclass api-1 (cepl-host-api)
  (;;
   ;; This must be a boolean indicating if the host can support
   ;; multiple contexts
   (supports-multiple-contexts-p
    :initarg :supports-multiple-contexts-p)
   ;;
   ;; This must be a boolean indicating if the host can support
   ;; multiple surfaces
   (supports-multiple-surfaces-p
    :initarg :supports-multiple-surfaces-p)
   ;;
   ;; The init function can take any number of &key arguments but
   ;; must also have &allow-other-keys in the argument list. No
   ;; non-keyword arguments are allowed
   (init-function
    :initarg :init-function)
   ;;
   ;; The shutdown function takes no arguments
   (shutdown-function
    :initarg :shutdown-function)
   ;;
   ;; This make-window function must have the following signature
   (make-window-function
    :initarg :make-window-function)
   ;;
   ;; The make-context function must have the following signature
   (make-context-function
    :initarg :make-context-function)
   ;;
   ;; The step function takes a window object as it's only argument
   (step-function
    :initarg :step-function)
   ;;
   ;; The swap function takes a window object as it's only argument
   (swap-function
    :initarg :swap-function)
   ;;
   ;; The event-callback function takes a single function as it's argument
   ;; that function must take an event as it's only argument.
   (register-event-callback-function
    :initarg :register-event-callback-function)
   ;;
   ;; The make-context function takes a gl-context and a surface
   ;; and makes the gl-context current on that surface.
   (make-context-current-function
    :initarg :make-context-current-function)))

(defmethod check-host ((host api-1))
  (assert (and (slot-boundp host 'supports-multiple-contexts-p)
               (slot-boundp host 'supports-multiple-surfaces-p)
               (slot-boundp host 'init-function)
               (slot-boundp host 'shutdown-function)
               (slot-boundp host 'make-window-function)
               (slot-boundp host 'make-context-function)
               (slot-boundp host 'step-function)
               (slot-boundp host 'swap-function)
               (slot-boundp host 'register-event-callback-function)
               (slot-boundp host 'make-context-current-function)))
  (with-slots (supports-multiple-surfaces-p
               supports-multiple-contexts-p
               init-function
               shutdown-function
               make-window-function
               make-context-function
               step-function
               swap-function
               register-event-callback-function
               make-context-current-function)
      host
    (assert (or (eq supports-multiple-surfaces-p t)
                (eq supports-multiple-surfaces-p nil)))
    (assert (or (eq supports-multiple-contexts-p t)
                (eq supports-multiple-contexts-p nil)))
    (assert (every #'functionp
                   (list init-function
                         shutdown-function
                         make-window-function
                         make-context-function
                         step-function
                         swap-function
                         register-event-callback-function
                         make-context-current-function)))
    host))

(defmethod %init ((host api-1) (args list))
  (with-slots (init-function
               step-function
               swap-function
               register-event-callback-function
               make-context-current-function)
      host
    (set-step-func step-function)
    (set-swap-func swap-function)
    (set-register-event-callback-func register-event-callback-function)
    (set-make-gl-context-current-on-surface make-context-current-function)
    (apply init-function args)))

(defmethod %supports-multiple-contexts-p ((host api-1) &key &allow-other-keys)
  (slot-value host 'supports-multiple-contexts-p))

(defmethod %supports-multiple-surfaces-p ((host api-1) &key &allow-other-keys)
  (slot-value host 'supports-multiple-surfaces-p))
