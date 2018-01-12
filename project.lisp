(in-package :cepl)

(deferror make-project-missing-default-implementation () ()
    "You may rightly be wondering why you have you use an odd method to start
your lisp session when using swank/sly with cepl.

It's a fair question though the answer is a little convoluted:

On OSX [and possibly windows 10?] you are only allowed to run window manager
operations on thread 0. The end result of this is that, for systems like sdl2,
you need to pump the events on that thread. This is made more complicated by the
fact that your repl may run in a separate thread [if you are using swank in its
default mode for example.

If CEPL were an engine this would be super easy, we would control the main loop,
ensure it run on the correct thread and that would be that. However that does
not fit with the goals of this project, in cepl we want to be explore anything
from the repl. So what else could we do?

So far I feel there are two main alternatives:

- Start the session such that everything [including the repl] run in thread 0
  by default and let you make threads as you need.
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

Luckily in the `docs/single-thread-swank.md` file you can get a function that
starts slime in the required mode from inside emacs.")

(deferror make-project-needs-quickproject () ()
    "make-project: Can't find quickproject

cepl uses the excellent quickproject to make its projects, please load
quickproject and then run this again.")

(defvar *template-dir*
  (asdf:system-relative-pathname :cepl "project-template/"))

(defun make-project (pathname &key name (host :cepl.sdl2) (repl :slime)
                                (depends-on '(:skitter :dirt)))
  ;; this has a bunch of little hacks to make the experience of making
  ;; project's better, we can add lots of little helpers here when they
  ;; pick the only valid option. See the skitter.sdl2 example for an
  ;; example
  (let ((name (if (keywordp name) (symbol-name name) name))
        (qp (find-package :quickproject))
        (depends-on (cepl-utils:listify depends-on)))
    (when (eq pathname :why)
      (error 'make-project-missing-default-implementation))
    (unless qp
      (error 'make-project-needs-quickproject))
    (let* ((pathname (uiop:pathname-directory-pathname pathname))
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
           (swank-p (or (eq repl :swank) (eq repl :slime)))
           (slynk-p (or (eq repl :sly) (eq repl :slynk)))
           (livesupport-p (or swank-p slynk-p)))
      (cepl-utils:ni-call
       :quickproject :make-project
       pathname
       :depends-on `(:cepl
                     :rtg-math.vari
                     ,host
                     ,@(when swank-p `(:swank))
                     ,@(when slynk-p `(:slynk))
                     ,@(when livesupport-p `(:livesupport))
                     ,@depends-on)
       :name name
       :template-directory *template-dir*
       :template-parameters (list :skitter-sdl-p skitter-sdl-p
                                  :livesupport-p livesupport-p))
      name)))

(docs:define-docs
  (defun make-project
      "
This function is a simple way to make a lisp project with all the
supporting libraries to get up and running with cepl quickly.

It uses the excellent quickproject project, so before starting be sure
to run the following in your repl:

    (ql:quickload :quickproject)

By default it assumes you want to use sdl2, skitter and dirt, and that
you will be using slime as the communication layer between lisp and your editor.

Valid values for the :repl argument are currently :slime or :slynk.
"))
