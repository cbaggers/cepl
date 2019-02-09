# SLIME and threading on MACOS

SLIME, by default, creates a bunch of threads to help with its internal operations. This can be problematic when working with OpenGL (and thus CEPL), as compiling a `.lisp` file will run in a different thread to the thread the `REPL` runs in.

Luckily, `slime` can be run with different communication `styles`, including a single threaded mode.

By default this can be a little fiddly from Emacs, so the easier way is to create a `~/.swank.lisp` file (if you dont already have one) and add the following code:

```
(setf swank:*communication-style* nil)
```

Then you can run `slime` as usual.

After this, simply `ql:quickload` your project like normal.


## Q: Why is the macOS start procedure more complicated?

*A:* This platform has restrictions over which thread is allowed to interact with the window manager (I will call this the 'UI thread'). This would be fine except that, when developing, most Common Lisp programmers use a system like `slime` or `sly` to connect their editor to their Lisp session, and those systems normally run code in a different thread.

The choices are then: frequently dispatch jobs to the 'UI thread' (and accept that overhead) or start `slime`/`sly` in a way that guarantees the thread. In CEPL we choose the latter as, although it does add one step to starting your project, it means you can ignore the detail whilst you are working.

To get a full breakdown of the above issue, run `(cepl:make-project :why)` in your repl.
