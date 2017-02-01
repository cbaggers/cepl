(in-package :cepl.context)

(defun make-context (&key (width 640) (height 480) (title "") fullscreen
		       no-frame (alpha-size 0) (depth-size 16) (stencil-size 8)
		       (red-size 8) (green-size 8) (blue-size 8) (buffer-size 32)
		       (double-buffer t) hidden (resizable t) gl-version)
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
      (%set-default-gl-options)
      (setf *gl-context* new-gl-context
            *gl-window* (window new-gl-context))
      (let ((default-fbo (cepl.fbos::%make-default-framebuffer dimensions t t)))
        (setf (slot-value *cepl-context* 'default-framebuffer) default-fbo))
      (map nil #'funcall *on-context*)
      (on-gl-context *cepl-context* new-gl-context)
      (cepl.host::set-default-swap-arg *gl-window*)
      (format t "New context v~s.~s"
	      (major-version new-gl-context)
	      (minor-version new-gl-context))
      (cepl:cls))))

(defmethod set-context-defaults (cepl-context)
  ;; Enable depth testing and use 'less than' for testing
  (setf (cepl.context:depth-test-function cepl-context)
        #'<)
  ;; Writing to depth buffer enabled by default
  (setf (cepl.context:depth-mask cepl-context) t)
  ;; Set the default depth range
  (setf (cepl.context:depth-range-vec2 cepl-context)
        (v! 0 1))
  ;; Enable the depth clamp
  (setf (cepl.context:depth-clamp cepl-context)
        t))
