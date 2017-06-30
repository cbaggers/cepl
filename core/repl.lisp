(in-package :cepl)

(defun2 repl (&optional (width 320) (height 240)
               #+darwin (gl-version 4.1)
               #-darwin gl-version)
  "Initialize CEPL and open a window. If the gl-version argument is nil then
   the default for the OS will be used."
  (initialize-cepl :gl-version gl-version)
  (cepl.context::legacy-add-surface (cepl-context) "CEPL" width height nil t
                                    nil nil t gl-version)
  (format t "~%-----------------~%    CEPL-REPL    ~%-----------------~%")
  (cls))

(defun2 initialize-cepl (&key gl-version host-init-flags)
  ;;
  ;; Initialize Host
  (unless cepl.host::*current-host*
    (apply #'cepl.host::initialize host-init-flags))
  ;;
  ;; Initalized the already created CEPL contexts
  (loop :for context :in cepl.context::*contexts* :do
     (cepl.context::patch-uninitialized-context-with-version context gl-version)
     (cepl.context::on-host-initialized context))
  ;;
  ;; Inform the world that CEPL is live
  (cepl.lifecycle::change-state :interactive)
  t)

(defun2 quit () (cepl.lifecycle::change-state :shutting-down))

(defun2 register-event-listener (function)
  "Register a function to be called on every event.
   The function must take 1 argument, which will be the event."
  (cepl.host::register-event-listener function))

(defun2 step-host (&optional (context (cepl-context)))
  (%with-cepl-context (cepl.context::current-surface) context
    (cepl.host::host-step cepl.context::current-surface))
  context)

(defun2 swap (&optional (context (cepl-context)))
  (%with-cepl-context (cepl.context::current-surface) context
    (cepl.host::host-swap cepl.context::current-surface))
  context)

(defun2 cls ()
  (%with-cepl-context (default-framebuffer) (cepl-context)
    (with-fbo-bound (default-framebuffer :target :framebuffer
                      :with-viewport nil
                      :with-blending nil)
      (clear) (swap)
      (clear) (swap))
    default-framebuffer))


(in-package :cepl)

(docs:define-docs
  (defun repl
      "
This function is a legacy item at this stage, but is still here as it feels
nice.

It calls #'initialize-cepl to make a resizable window and prints out a message
in the repl.
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
