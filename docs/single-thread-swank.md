# Slime and threading on Windows and OSX

Slime by default creates a bunch of threads to help with its internal operations. This can be problematic when working with OpenGL (and thus CEPL) as compiling a `.lisp` file will run in a different thread to the thread the `REPL` runs in.

Luckily `slime` can be run with different communication `styles`, including a single threaded mode.
By default this is a little fiddly from emacs so if you add the following to your `.emacs` file:

```
(defun slime-style-init-command (port-filename _coding-system extra-args)
  "Return a string to initialize Lisp."
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                  (concat slime-path slime-backend))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,(slime-to-lisp-filename (expand-file-name loader))
                     :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,(slime-to-lisp-filename port-filename)
			,@extra-args)))))

(defun slime-style (&optional style)
  (interactive
   (list (intern-soft (read-from-minibuffer "Style: " "nil"))))
  (lexical-let ((style style))
    (slime-start
     :init (lambda (x y)
	     (slime-style-init-command
	      x y `(:style ,style :dont-close t))))))
```

Then you can run `slime` in single-threaded mode by:

- pressing `M-x`
- entering `slime-style`
- pressing `return` or `enter`
- choosing `nil` as the style (this should be the default)
- pressing `return` or `enter`

After this simple `ql:quickload` your project like normal.


## Starting Outside of Emacs

If you have used `cepl:make-project` to make your project you will also find a `run-session.lisp` file in the root of your project.

If you run `sbcl --load "run-session.lisp` (or the equivalent for your lisp distribution) it will start swank at port 4005 and load your project.

You can then `slime-connect` to it as usual.


## Q: Why is the Windows/OSX start procedure more complicated?

*A:* Both these platforms have restrictions over which thread is allowed to interact with the window manager (I will call this the 'UI thread'). This would be fine except that, when developing, most common-lisp programmers use a system like `slime` or `sly` to connect their editor to their lisp session and those systems normally run your code in a different thread.

The choices are then to frequently dispatch jobs to the 'UI thread' (and accept that overhead) or start `slime`/`sly` in a way that guarentees the thread. In cepl we choose the latter as, although it does add one step to starting your project, it means you can ignore the detail whilst you are working.

To get a full breakdown of the above issue run `(cepl:make-project :why)` in your repl.
