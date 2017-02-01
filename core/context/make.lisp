(in-package :cepl.context)

;;----------------------------------------------------------------------

(defun %make-gl-context (&key (width 640) (height 480) (title "") fullscreen
                           no-frame (alpha-size 0) (depth-size 16)
                           (stencil-size 8) (red-size 8) (green-size 8)
                           (blue-size 8) (buffer-size 32) (double-buffer t)
                           hidden (resizable t) gl-version)
  (destructuring-bind (context-handle window)
      (cepl.host:request-context
       width height title fullscreen
       no-frame alpha-size depth-size stencil-size
       red-size green-size blue-size buffer-size
       double-buffer hidden resizable gl-version)
    (let ((new-gl-context (make-instance
                           'gl-context
                           :handle context-handle
                           :window window
                           :version-major (gl:major-version)
                           :version-minor (gl:minor-version)
                           :version-float (coerce
                                           (+ (gl:major-version)
                                              (/ (gl:minor-version)
                                                 10))
                                           'single-float)))
          (dimensions (list width height)))
      (ensure-cepl-compatible-setup)
      (setf *gl-context* new-gl-context
            *gl-window* (window new-gl-context))
      (let ((default-fbo (cepl.fbos::%make-default-framebuffer dimensions t t)))
        (setf (slot-value *cepl-context* 'default-framebuffer) default-fbo))
      (on-gl-context *cepl-context* new-gl-context))))

;;----------------------------------------------------------------------

(defun on-gl-context (cepl-context new-gl-context)
  (map nil #'funcall *on-context*)
  (with-slots (gl-context uninitialized-resources current-viewport
                          default-viewport default-framebuffer)
      cepl-context
    (setf gl-context new-gl-context)
    (let ((vp (cepl.fbos:attachment-viewport default-framebuffer 0)))
      (setf current-viewport vp
            default-viewport vp))
    ;;
    ;; Set GL Defaults
    (set-context-defaults cepl-context)
    ;;
    (initialize-all-delayed uninitialized-resources)
    (setf uninitialized-resources nil))
  ;;
  (cepl.host::set-default-swap-arg *gl-window*)
  (format t "New context v~s.~s"
          (major-version new-gl-context)
          (minor-version new-gl-context))
  (funcall 'cepl:cls))

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
