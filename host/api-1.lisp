(in-package :cepl.host)

;;============================================================
;; API 1
;;============================================================

(defclass api-1 (cepl-host-api)
  ( ;;
   ;; This must be a boolean indicating if the host can support
   ;; multiple contexts
   supports-multiple-contexts-p
   ;;
   ;; This must be a boolean indicating if the host can support
   ;; multiple surfaces
   supports-multiple-surfaces-p
   ;;
   ;; The init function can take any number of &key arguments but
   ;; must also have &allow-other-keys in the argument list. No
   ;; non-keyword arguments are allowed
   init-function
   ;;
   ;; The shutdown function takes no arguments
   shutdown-function
   ;;
   ;; This make-surface function must have the following signature:
   ;; (&key width height title fullscreen
   ;;       no-frame alpha-size depth-size stencil-size
   ;;       red-size green-size blue-size buffer-size
   ;;       double-buffer hidden resizable)
   ;;
   ;; the make-surface & make-context functiosn have overlapping
   ;; signatures as in different apis different objects own the data
   ;;
   make-surface-function
   ;;
   ;; This destroy-surface function takes a surface as its only argument
   destroy-surface-function
   ;;
   ;; The make-context function must have the following signature:
   ;; (surface version width height title fullscreen
   ;;  no-frame alpha-size depth-size stencil-size
   ;;  red-size green-size blue-size buffer-size
   ;;  double-buffer hidden resizable)
   ;;
   ;; the make-surface & make-context functions have overlapping
   ;; signatures as in different apis different objects own the data
   ;;
   make-context-function
   ;;
   ;; The step function takes a window object as it's only argument
   step-function
   ;;
   ;; The swap function takes a window object as it's only argument
   swap-function
   ;;
   ;; The event-callback function takes a single function as it's argument
   ;; that function must take an event as it's only argument.
   register-event-callback-function
   ;;
   ;; The make-context function takes a gl-context and a surface
   ;; and makes the gl-context current on that surface.
   make-context-current-function
   ;;
   ;; The surface-size function takes a window object as it's only argument
   surface-size-function
   ))

(defmethod check-host ((host api-1))
  (assert (slot-boundp host 'supports-multiple-contexts-p))
  (assert (slot-boundp host 'supports-multiple-surfaces-p))
  (assert (slot-boundp host 'init-function))
  (assert (slot-boundp host 'shutdown-function))
  (assert (slot-boundp host 'make-surface-function))
  (assert (slot-boundp host 'destroy-surface-function))
  (assert (slot-boundp host 'make-context-function))
  (assert (slot-boundp host 'step-function))
  (assert (slot-boundp host 'swap-function))
  (assert (slot-boundp host 'register-event-callback-function))
  (assert (slot-boundp host 'make-context-current-function))
  (assert (slot-boundp host 'surface-size-function))
  (with-slots (supports-multiple-surfaces-p
               supports-multiple-contexts-p
               init-function
               shutdown-function
               make-surface-function
               destroy-surface-function
               make-context-function
               step-function
               swap-function
               register-event-callback-function
               make-context-current-function
               surface-size-function)
      host
    (assert (or (eq supports-multiple-surfaces-p t)
                (eq supports-multiple-surfaces-p nil)))
    (assert (or (eq supports-multiple-contexts-p t)
                (eq supports-multiple-contexts-p nil)))
    (assert (every #'functionp
                   (list init-function
                         shutdown-function
                         make-surface-function
                         destroy-surface-function
                         make-context-function
                         step-function
                         swap-function
                         register-event-callback-function
                         make-context-current-function
                         surface-size-function)))
    host))

(defmethod %init ((host api-1) (args list))
  (with-slots (init-function
               step-function
               swap-function
               register-event-callback-function
               make-context-current-function
               surface-size-function)
      host
    (set-step-func step-function)
    (set-swap-func swap-function)
    (set-register-event-callback-func register-event-callback-function)
    (set-make-gl-context-current-on-surface make-context-current-function)
    (set-window-size-func surface-size-function)
    (apply init-function args)))

(defmethod %supports-multiple-contexts-p ((host api-1) &key &allow-other-keys)
  (slot-value host 'supports-multiple-contexts-p))

(defmethod %supports-multiple-surfaces-p ((host api-1) &key &allow-other-keys)
  (slot-value host 'supports-multiple-surfaces-p))

(defmethod %make-surface ((host api-1)
                          &key (width 600) (height 600) (title "CEPL")
                            (fullscreen nil) (no-frame nil) (alpha-size 0)
                            (red-size 8) (green-size 8) (blue-size 8)
                            (depth-size 16) (stencil-size 8) (buffer-size 32)
                            (double-buffer t) (hidden nil) (resizable t)
                            &allow-other-keys)
  (with-slots (make-surface-function) host
    (funcall make-surface-function
             width height title fullscreen
             no-frame alpha-size depth-size stencil-size
             red-size green-size blue-size buffer-size
             double-buffer hidden resizable)))

(defmethod %make-gl-context ((host api-1)
                             &key surface version
                               (width 600) (height 600) (title "CEPL")
                               (fullscreen nil) (no-frame nil) (alpha-size 0)
                               (red-size 8) (green-size 8) (blue-size 8)
                               (depth-size 16) (stencil-size 8) (buffer-size 32)
                               (double-buffer t) (hidden nil) (resizable t)
                               &allow-other-keys)
  (assert surface)
  (with-slots (make-context-function) host
    (funcall make-context-function
             surface version width height title fullscreen
             no-frame alpha-size depth-size stencil-size
             red-size green-size blue-size buffer-size
             double-buffer hidden resizable)))

(defmethod %supports-multiple-surfaces-p ((host api-1) &key &allow-other-keys)
  (slot-value host 'supports-multiple-surfaces-p))
