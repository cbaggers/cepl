(in-package :cepl)

(defun repl (&optional (width 320) (height 240) requested-gl-version)
  (init width height "CEPL REPL" t requested-gl-version)
  (format t "~%-----------------~%    CEPL-REPL    ~%-----------------~%")
  (cls))

(defun init (&optional (width 320) (height 240) (title "CEPL") (resizable t)
               requested-gl-version host-init-flags)
  (when (or width height title resizable requested-gl-version)
    (warn "Setting the following from cepl:init is not longer supported:~{~%~a~}"
          '(#:width #:height #:title #:resizable #:requested-gl-version)))
  ;;
  ;; Initialize Host
  (unless cepl.host::*current-host*
    (apply #'cepl.host::initialize host-init-flags))
  ;;
  ;; Initalized the already created CEPL contexts
  (loop :for context :in cepl.context::*contexts* :do
     (cepl.context::on-host-initialized context))
  ;;
  ;; Inform the world that CEPL is live
  (cepl.lifecycle::change-state :interactive)
  t)

(defun quit () (cepl.lifecycle::change-state :shutting-down))

(defun step-host (&optional (context *cepl-context*))
  (with-slots (cepl.context::current-surface) context
    (cepl.host::host-step cepl.context::current-surface))
  context)

(defun swap (&optional (context *cepl-context*))
  (with-slots (cepl.context::current-surface) context
    (cepl.host::host-swap cepl.context::current-surface))
  context)

(defun cls ()
  (with-slots (default-framebuffer) *cepl-context*
    (with-fbo-bound (default-framebuffer :target :framebuffer
                      :with-viewport nil
                      :with-blending nil)
      (clear) (swap)
      (clear) (swap))))


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

As well as its own internal work, CEPL will ask the host to shut itself down.
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
