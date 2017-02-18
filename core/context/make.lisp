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
                           'gl-context :handle context-handle :window window))
          (dimensions (list width height)))
      (ensure-cepl-compatible-setup)
      (format t "New context v~s.~s"
	      (major-version new-gl-context)
	      (minor-version new-gl-context))
      (%set-default-gl-options)
      (setf *gl-context* new-gl-context
            *gl-window* (window new-gl-context))
      (setf (slot-value new-gl-context 'fbo)
            (cepl.fbos::%make-default-framebuffer dimensions t t))
      (map nil #'funcall *on-context*)
      (on-gl-context *cepl-context* new-gl-context)
      (cepl.host::set-default-swap-arg *gl-window*)
      (cepl:cls))))
