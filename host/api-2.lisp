(in-package :cepl.host)

;;============================================================
;; API 2
;;============================================================

(defclass api-2 (api-1)
  (;;
   ;; The make-gl-context-shared-with-current-context function must have the
   ;; following signature:
   ;; (current-context surface version double-buffer
   ;;  alpha-size depth-size stencil-size buffer-size
   ;;  red-size green-size blue-size)
   ;;
   ;; the function must return 2 values, the new context and the surface that
   ;; context is current on. The function may use the current-surface if that
   ;; is allowed or may make a hidden surface.
   ;;
   ;; the make-surface & make-shared-context functions have overlapping
   ;; signatures as in different apis different objects own the data
   ;;
   make-gl-context-shared-with-current-context-function))

(defmethod check-host ((host api-2))
  (call-next-method host)
  (assert (slot-boundp host 'make-gl-context-shared-with-current-context-function))
  (with-slots (make-gl-context-shared-with-current-context-function) host
    (assert (functionp make-gl-context-shared-with-current-context-function))
    host))

(defmethod %make-gl-context-shared-with-current-context
    ((host api-2)
     &key
       current-gl-context
       surface
       version
       (double-buffer t) (alpha-size 0)
       (red-size 8) (green-size 8) (blue-size 8)
       (depth-size 16) (stencil-size 8)
       (buffer-size 32)
       &allow-other-keys)
  (assert (and current-gl-context surface))
  (with-slots (make-gl-context-shared-with-current-context-function) host
    (funcall make-gl-context-shared-with-current-context-function
             current-gl-context surface version double-buffer
             alpha-size depth-size stencil-size buffer-size
             red-size green-size blue-size)))

;; The following are handled by api-1
;;
;; %init
;; %supports-multiple-contexts-p
;; %supports-multiple-surfaces-p
;; %make-surface
;; %make-gl-context
;; %set-surface-size
;; %surface-fullscreen-p
;; %set-surface-fullscreen
;; %surface-title
;; %set-surface-title
