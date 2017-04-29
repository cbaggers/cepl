(in-package :cepl.host)

(let (step-func
      swap-func
      win-size-func
      (reg-event-callback-func nil)
      (event-callbacks-cache nil)
      (make-current-func (lambda (c s) c s)))

  (defun set-step-func (func)
    "Call this and pass the function that will be called every time
     #'cepl:step-host is called"
    (setf step-func func))

  (defun set-swap-func (func)
    "Call this and pass the function that will be called every time #'cepl:swap
     is called"
    (setf swap-func func))

  (defun set-window-size-func (func)
    "Call this and pass the function that will be called when the cepl needs to
     query the window size"
    (setf win-size-func func))

  (defun set-register-event-callback-func (func)
    "Call this and pass the function that will be called when the cepl needs to
     query the window size"
    (setf reg-event-callback-func func)
    (map nil #'register-event-listener event-callbacks-cache)
    func)

  (defun set-make-gl-context-current-on-surface (func)
    "Call this and pass the function that will be called when the cepl needs to
     make the context current"
    (setf make-current-func func))

  (defun host-step (win)
    "not external"
    (funcall step-func win))

  (defun host-swap (win)
    "not external"
    (funcall swap-func win))

  (defun window-size (win)
    "When given the host-specific window handle will return the size of the window"
    (funcall win-size-func win))

  (defun make-gl-context-current-on-surface (gl-context surface)
    (funcall make-current-func gl-context surface))

  (defun register-event-listener (function)
    "not external"
    (if reg-event-callback-func
        (funcall reg-event-callback-func function)
        (push function event-callbacks-cache))))

;;----------------------------------------------------------------------

(defun initialize (&rest args &key &allow-other-keys)
  (if *current-host*
      (warn "CEPL: Cannot reinitialize a CEPL host")
      (let ((host (or *declared-host*
                      (make-instance 'api-0))))
        (%init host args)
        (setf *current-host* host))))

;;----------------------------------------------------------------------

(defun make-gl-context (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: make-gl-context cannot be called yet as CEPL has not been initialized")
  (apply #'%make-gl-context *current-host* args))

;;----------------------------------------------------------------------

(defun make-surface (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: make-gl-context cannot be called yet as CEPL has not been initialized")
  (apply #'%make-surface *current-host* args))

;;----------------------------------------------------------------------

(defmethod %supports-multiple-surfaces-p (host &key &allow-other-keys)
  (declare (ignore host))
  nil)

(defun supports-multiple-surfaces-p (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: supports-multiple-surfaces-p cannot be called yet as CEPL has not been initialized")
  (apply #'%supports-multiple-surfaces-p *current-host* args))

;;----------------------------------------------------------------------

(defmethod %supports-multiple-contexts-p (host &key &allow-other-keys)
  (declare (ignore host))
  nil)

(defun supports-multiple-contexts-p (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: supports-multiple-contexts-p cannot be called yet as CEPL has not been initialized")
  (apply #'%supports-multiple-contexts-p *current-host* args))

;;----------------------------------------------------------------------
