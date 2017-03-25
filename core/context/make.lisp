(in-package :cepl.context)

;;----------------------------------------------------------------------

(defun on-gl-context (cepl-context)
  (map nil #'funcall *on-context*)
  (with-slots (gl-context uninitialized-resources current-viewport
                          default-viewport default-framebuffer)
      cepl-context
    (let ((vp (cepl.fbos:attachment-viewport default-framebuffer 0)))
      (setf current-viewport vp
            default-viewport vp))
    ;;
    ;; Set GL Defaults
    (set-context-defaults cepl-context)
    ;;
    (initialize-all-delayed uninitialized-resources)
    (setf uninitialized-resources nil)
    ;;
    (cepl.host::set-default-swap-arg *gl-window*)
    (format t "New context v~s.~s"
            (major-version gl-context)
            (minor-version gl-context))
    (funcall 'cepl:cls)))

;;----------------------------------------------------------------------

(defun set-context-defaults (cepl-context)
  ;; Enable depth testing and use 'less than' for testing
  (setf (depth-test-function cepl-context)
        #'<)
  ;; Writing to depth buffer enabled by default
  (setf (depth-mask cepl-context) t)
  ;; Set the default depth range
  (setf (depth-range-vec2 cepl-context)
        (v! 0 1))
  ;; Enable the depth clamp
  (setf (depth-clamp cepl-context)
        t)
  ;; Enable backface culling
  (setf (cull-face cepl-context)
        :back)
  ;; Set culling winding order
  (setf (front-face cepl-context)
        :ccw)
  ;; Default clear color
  (setf (clear-color cepl-context)
        (v! 0 0 0 0)))

;;----------------------------------------------------------------------

(defmethod on-host-initialized ((context cepl-context))
  (unless (initialized-p context)
    (when (remove context *contexts*)
      (assert (cepl.host:supports-multiple-contexts-p) ()
              "CEPL: Sorry your current CEPL host does not currently support multiple contexts"))
    (with-slots (surfaces current-surface gl-context gl-version) context
      ;; make the surfaces
      (let ((host-surfaces (mapcar #'make-surface-from-pending surfaces)))
        (assert (>= (length host-surfaces) 1))
        (setf surfaces host-surfaces)
        (setf current-surface (first surfaces)))
      ;; make the gl-context
      (let ((raw-context (cepl.host::make-gl-context :gl-version gl-version)))
        (ensure-cepl-compatible-setup)
        (setf gl-context
              (make-instance
               'gl-context
               :handle raw-context
               :version-major (gl:major-version)
               :version-minor (gl:minor-version)
               :version-float (coerce (+ (gl:major-version)
                                         (/ (gl:minor-version) 10))
                                      'single-float))))
      ;;
      ;; Setup default fbo
      (let ((default-fbo (cepl.fbos::%make-default-framebuffer
                          (cepl.host:window-size current-surface) t t)))
        (setf (slot-value context 'default-framebuffer) default-fbo))
      ;;
      ;; hack until we support things properly
      (setf *gl-context* gl-context
            *gl-window* current-surface)
      ;;
      ;; initialize all the pending objects
      (on-gl-context context)
      ;;
      ;;
      context)))

(defun make-surface-from-pending (pending-surface)
  (with-slots (title width height fullscreen
                     resizable no-frame hidden)
      pending-surface
    (cepl.host::make-surface
     :title title :width width :height height
     :fullscreen fullscreen :resizable resizable
     :no-frame no-frame :hidden hidden)))
