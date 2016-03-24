(in-package :cepl)

(defun repl (&optional (width 320) (height 240))
  (init width height "CEPL REPL" t)
  (format t "~%-----------------~%    CEPL-REPL    ~%-----------------~%"))

(defun init (&optional (width 320) (height 240) (title "CEPL") (resizable t))
  (handler-case
      (find-method #'cepl.host:init nil nil)
    (error () (error "Cepl.Host: Init could not be found. Have you loaded a host?")))
  (cepl.host:init)
  (cepl.context:make-context :width width :height height :resizable resizable
			     :title title)
  (cepl.lifecycle::change-state :interactive)
  t)

(defun init-repl-link (&key (port 4005))
  ;; handle some osx package managers
  #+darwin
  (let ((extra-package-dirs '("/opt/local/lib/" "/usr/local/")))
    (mapcar
     (lambda (raw-path)
       (let ((port-dir (cl-fad:directory-exists-p raw-path)))
         (when (and port-dir
                    (not (member port-dir cffi:*foreign-library-directories*)))
           (push port-dir cffi:*foreign-library-directories*))))
     extra-package-dirs))

  ;; start swank or slynk in a way that allows repl usage with windows&osx
  ;; window-manager thread crap
  (cond
    ((find-package :swank)
     (cepl.host:set-primary-thread-and-run
      (lambda ()
	(let (#+linux
	      (style (cepl-utils:ni-val :swank :*COMMUNICATION-STYLE*))
	      #-linux
	      (style nil))
	  (cepl-utils:ni-call :swank :create-server :style style :dont-close t :port port)))))
    ((find-package :slynk)
     (cepl.host:set-primary-thread-and-run
      (lambda ()
	(let (#+linux
	      (style (cepl-utils:ni-val :slynk :*COMMUNICATION-STYLE*))
	      #-linux
	      (style nil))
	  (cepl-utils:ni-call :slynk :create-server :style style :dont-close t :port port)))))))

(defun quit () (cepl.lifecycle::change-state :shutting-down))

(defun step-host ()
  (cepl.host::host-step))

(defun swap ()
  (cepl.host::host-swap))

(defun cls ()
  (with-fbo-bound (%default-framebuffer :with-viewport nil :with-blending nil)
    (clear) (cepl.host::host-swap)
    (clear) (cepl.host::host-swap)))


(in-package :cepl)

(docs:define-docs
  (defun repl
      "
This function is a legacy item at this stage, but is still here as it feels
nice.

It calls #'init to make a resizable window and prints out a message in the repl.
")

  (defun init
      "
This is how we initialize CEPL. It is important to do this before using any api
that touches the gpu.

When you call this it does a few things:
- Asks the host to initialize itself
- Asks the host for an opengl context and window
- Wraps the gl-context in CEPL's own context object
- Sets up some internals systems

And finally returns t.

CEPL is now ready to use.
")

  (defun quit
      "
Call this to shutdown CEPL.

Aswell from it's own internal work, CEPL will ask the host to shut itself down.
")

  (defun step-host
      "
Call this to ask the host update its own internals.

This description is a bit nebulous as cepl doesnt impose what the host should do
when this call is made; however it is usual to call #'step-host every tick of
a main-loop and so often hosts will use this to do per-tick jobs like polling
for input events.
")

  (defun swap
      "
Call this ask the host to swap the buffers of the default framebuffer.

We usually do this when we have finished drawing a given frame.
")

  (defun cls
      "
CLS is here as it reminds me of qbasic and that makes me happy.

It calls #'clear and #'swap twice so dont use this in your actually rendering
code. It can be handy though if you want to clear the screen from the repl.
"))
