(in-package :cepl.host)

;; This is what the backend has to implement

(defgeneric init (&optional init-flags)
  (:documentation
   "Implement this method and initialize your system inside it.
    This is called as the first step of cepl initializing.
    init-flags are host specific flags that can be passed from cepl"))

(defgeneric request-context
    (width height title fullscreen
     no-frame alpha-size depth-size stencil-size
     red-size green-size blue-size buffer-size
     double-buffer hidden resizable gl-version)
  (:documentation
   "Implement this method and return a list containing:
    - gl-context as the first element
    - window as the second element"))

(defgeneric shutdown ()
  (:documentation
   "Implement this with code to shutdown the host. This will be called
    when cepl is shutdown"))

(defgeneric set-primary-thread-and-run (func &rest args)
  (:documentation
   "This will be called from the UI thread (if required by the OS)
    Implement and use this if you need to do some setup from that thread"))

(let (step-func
      swap-func
      swap-arg
      win-size-func)
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

  (defun set-default-swap-arg (win-handle)
    "not external"
    (setf swap-arg win-handle))

  (defun host-step (&optional (win swap-arg) tpref)
    "not external"
    (funcall step-func win tpref))

  (defun host-swap (&optional (win swap-arg))
    "not external"
    (funcall swap-func win))

  (defun window-size (win)
    "When given the host-specific window handle will return the size of the window"
    (funcall win-size-func win)))
