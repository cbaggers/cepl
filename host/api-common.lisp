(in-package :cepl.host)

(let (step-func
      swap-func
      win-size-func
      (reg-event-callback-func (lambda (x) x))
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
    (setf reg-event-callback-func func))

  (defun set-make-gl-context-current-on-surface (func)
    "Call this and pass the function that will be called when the cepl needs to
     query the window size"
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
    (funcall make-current-func gl-context surface)))

;;----------------------------------------------------------------------

(defgeneric %init (host args))

(defun initialize (&rest args &key &allow-other-keys)
  (if *current-host*
      (warn "CEPL: Cannot reinitialize a CEPL host")
      (let ((host (or *declared-host*
                      (make-instance 'api-0))))
        (%init host args)
        (setf *current-host* host))))

;;----------------------------------------------------------------------

(defgeneric %make-gl-context (host &key &allow-other-keys))

(defun make-gl-context (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: make-gl-context cannot be called yet as CEPL has not been initialized")
  (apply #'%make-gl-context *current-host* args))

;;----------------------------------------------------------------------

(defgeneric %make-surface (host &rest args &key &allow-other-keys))

(defun make-surface (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: make-gl-context cannot be called yet as CEPL has not been initialized")
  (apply #'%make-surface *current-host* args))

;;----------------------------------------------------------------------

(defgeneric %supports-multiple-surfaces-p (host &key &allow-other-keys)
  (:method (host &key &allow-other-keys)
    (declare (ignore host))
    nil))

(defun supports-multiple-surfaces-p (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: supports-multiple-surfaces-p cannot be called yet as CEPL has not been initialized")
  (apply #'%supports-multiple-surfaces-p *current-host* args))

;;----------------------------------------------------------------------

(defgeneric %supports-multiple-contexts-p (host &key &allow-other-keys)
  (:method (host &key &allow-other-keys)
    (declare (ignore host))
    nil))

(defun supports-multiple-contexts-p (&rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: supports-multiple-contexts-p cannot be called yet as CEPL has not been initialized")
  (apply #'%supports-multiple-contexts-p *current-host* args))

;;----------------------------------------------------------------------

(defgeneric %set-surface-size (host surface width height &key &allow-other-keys)
  (:method (host surface width height &key &allow-other-keys)
    (declare (ignore host surface width height))
    nil))

(defun set-surface-size (surface width height &rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: set-surface-size cannot be called yet as CEPL has not been initialized")
  (apply #'%set-surface-size *current-host* surface width height args))

;;----------------------------------------------------------------------

(defgeneric %surface-fullscreen-p (host surface &key &allow-other-keys)
  (:method (host surface &key &allow-other-keys)
    (declare (ignore host surface))
    nil))

(defun surface-fullscreen-p (surface &rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: surface-fullscreen-p cannot be called yet as CEPL has not been initialized")
  (apply #'%surface-fullscreen-p *current-host* surface args))

(defgeneric %set-surface-fullscreen (host surface state &key &allow-other-keys)
  (:method (host surface state &key &allow-other-keys)
    (declare (ignore host surface state))
    nil))

(defun set-surface-fullscreen (surface state &rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: set-surface-fullscreen cannot be called yet as CEPL has not been initialized")
  (apply #'%set-surface-fullscreen *current-host* surface state args))

;;----------------------------------------------------------------------

(defgeneric %surface-title (host surface &key &allow-other-keys)
  (:method (host surface &key &allow-other-keys)
    (declare (ignore host surface))
    nil))

(defun surface-title (surface &rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: surface-title cannot be called yet as CEPL has not been initialized")
  (apply #'%surface-title *current-host* surface args))

(defgeneric %set-surface-title (host surface title &key &allow-other-keys)
  (:method (host surface title &key &allow-other-keys)
    (declare (ignore host surface title))
    nil))

(defun set-surface-title (surface title &rest args &key &allow-other-keys)
  (assert *current-host* () "CEPL: surface-title cannot be called yet as CEPL has not been initialized")
  (apply #'%set-surface-title *current-host* surface title args))

;;----------------------------------------------------------------------
