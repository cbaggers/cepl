(in-package :cepl)

(deferror make-project-missing-default-implementation () ()
    "Sorry the :default-implementation argument must be provided

You may rightly be wondering why you are being asked what implementation you
mostly use when you are just trying to make a project.
It's a fair question though the answer is a little convoluted:

TLDR: Certain OS's window management operations have to be done from the
      first thread. Rather than require you to send those operations to the
      main thread we make a script that starts your lisp session in a way
      that makes doing this from the repl easy, however on osx/win you then
      use a script that starts your lisp session. The choice of implementation
      is what lisp will be used [by default] by the script.

Long Version:

On OSX [and possibly windows 10?] you are only allowed to run window manager
operations on thread 0. The end result of this is that, for systems like sdl2,
you need to pump the events on that thread. This is made more complicated by the
fact that your repl may run in a separate thread [if you are using swank in it's
default mode for example.

If CEPL were an engine this would be super easy, we would control the main loop,
ensure it run on the correct thread and that would be that. However that does
not fit with the goals of this project, in cepl we want to be explore anything
from the repl. So what else could we do?

So far I feel there are two main alternatives:

- Start the session such that everything [including the repl] run in thread 0
  by default.
- Give you some macro to run a form on thread 0. [For the rest of this document
  we will call this macro 'on-main]

The disadvantage of the second approach is that you need to remember to use it
all the time, or add some hook to your repl so it is wrapped around every form.
If you don't do this then your code is potentially being run threads based on
whether it is being call from a window event [which HAS to be on thread 0] or
the repl [which can be on some other thread].

The above is bad as many real-time graphics programs or games are performance
[and sometimes thread] sensitive, and the value of the repl is being able to try
things and have some confidence that the same code will behave in the same way
in your project.

For this reason [and others] cepl prefers the first approach, where you start
your lisp session [with certain thread settings] and then connect to it using
your editor/repl. It is quite simple a choice between an ugly thing once at the
start of your session or many ugly things throughout the session.

Another benefit is for people running dual-graphics cards [e.g. optimus] who
will want to run their lisp session with the powerful gpu and the editor on the
integrated one.

Possible Alternatives:
One nice alternative would be able to start your session [in something like
swank] and then move the thread the repl is evaluating on to thread 0. This
would avoid the need for the 'start session script' but wouldn't fix the issue
faced by those running multiple graphics card setups.")

(deferror make-project-needs-quickproject () ()
    "make-project: Can't find quickproject

cepl uses the excellent quickproject to make it's projects, please load
quickproject and then run this again.")

(defvar *default-template-dir*
  (asdf:system-relative-pathname :cepl "project-template/default"))

(defvar *swank-template-dir*
  (asdf:system-relative-pathname :cepl "project-template/swank"))

(defun make-project (pathname &key name (host :cepl.sdl2) (repl :swank)
				depends-on)
  ;; this has a bunch of little hacks to make the experience of making
  ;; project's better, we can add lots of little helpers here when they
  ;; pick the only valid option. See the skitter.sdl2 example for an
  ;; example
  (let ((qp (find-package :quickproject)))
    (unless qp
      (error 'make-project-needs-quickproject))
    (when (eq pathname :why)
      (error 'make-project-missing-default-implementation))
    (let* ((pathname (pathname-as-directory pathname))
	   (name (or name (cepl-utils:ni-call
			   :quickproject :pathname-project-name
			   pathname)))
	   ;; if you are using skitter and also cepl.sdl2 then
	   ;; you actually will want skitter.sdl2
	   (depends-on (if (and (member :skitter depends-on)
				(eq host :cepl.sdl2))
			   (cons :cepl.skitter.sdl2 (remove :skitter depends-on))
			   depends-on))
	   ;; with skitter.sdl2 there are two input packages that are
	   ;; good to have :use'd by default so we add them
	   (skitter-sdl-p (member :skitter.sdl2 depends-on))
	   ;;
	   (swank-p (or (eq repl :slime) (eq repl :swank))))
      (cepl-utils:ni-call
       :quickproject :make-project
       pathname
       :depends-on `(:cepl
		     :temporal-functions
		     ,host
		     ,@(when (or (eq repl :swank) (eq repl :slime))
			     `(:swank.live))
		     ,@(when (or (eq repl :sly) (eq repl :slynk))
			     `(:livesupport))
		     ,@(utils:listify depends-on))
       :name name
       :template-directory (if swank-p
			       *swank-template-dir*
			       *default-template-dir*)
       :template-parameters (list :start-repl-session (gen-thing repl)
				  :skitter-sdl-p skitter-sdl-p))
      name)))

(defun gen-thing (repl)
  (if (eq repl :swank)
      "(swank:create-server :style style :dont-close t)"
      "(identity)"))
