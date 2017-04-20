(in-package :cepl.host)

;;============================================================
;; API 0
;;
;; This is the 'odd one out' amongst these api files because
;; it was written before I was thinking about versioning the
;; host api.
;;
;; It is the fallback. If no backend is found supporting a
;; more recent version of the host api then this will be used.
;;
;;============================================================
;;
;; Shim for the new system

(defclass api-0 (cepl-host-api) ())
(defmethod check-host ((host api-0)) host)
(defmethod %init ((host api-0) (args list))
  (declare (ignore args host))
  (handler-case
      (find-method #'cepl.host:init nil nil)
    (error () (error "Cepl.Host: Init could not be found. Have you loaded a host?")))
  (init))

(defvar *api-0-context-singleton*)
(defvar *api-0-window-singleton*)
(defvar *api-0-make-context-called* nil)
(defvar *api-0-make-window-called* nil)

(defun %api-0-populate-singletons
    (&key (width 640) (height 480) (title "") fullscreen
       no-frame (alpha-size 0) (depth-size 16)
       (stencil-size 8) (red-size 8) (green-size 8)
       (blue-size 8) (buffer-size 32) (double-buffer t)
       hidden (resizable t) gl-version &allow-other-keys)
  (destructuring-bind (context-handle window)
      (request-context
       width height title fullscreen
       no-frame alpha-size depth-size stencil-size
       red-size green-size blue-size buffer-size
       double-buffer hidden resizable gl-version)
    (setf *api-0-window-singleton* window
          *api-0-context-singleton* context-handle))
  (values))

(defmethod %make-surface ((host api-0) &rest args &key &allow-other-keys)
  (if *api-0-make-context-called*
      (error "CEPL: Cannot make multiple surfaces with legacy host api")
      (progn
        (apply #'%api-0-populate-singletons args)
        *api-0-window-singleton*)))

(defmethod %make-gl-context ((host api-0) &key &allow-other-keys)
  (if *api-0-make-context-called*
      (error "CEPL: Cannot make multiple contexts with legacy host api")
      (progn
        (setf *api-0-make-context-called* t)
        (or *api-0-context-singleton*
            (error "CEPL Internal Bug: Must call make-window before make-context when using legacy host api")))))

;; no longer used but legacy hosts still expect to find it
(defun set-default-swap-arg (win-handle)
  win-handle)

;;------------------------------------------------------------
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
