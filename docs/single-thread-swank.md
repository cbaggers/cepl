# SLIME and threading on OSX

SLIME, by default, creates a bunch of threads to help with its internal operations. This can be problematic when working with OpenGL (and thus CEPL), as compiling a `.lisp` file will run in a different thread to the thread the `REPL` runs in.

Luckily `slime` can be run with different communication `styles`, including a single threaded mode.
By default this is a little fiddly from Emacs, so if you add the following to your `.emacs` file:

```
(require 'cl)

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


## Q: Why is the OSX start procedure more complicated?

*A:* This platform has restrictions over which thread is allowed to interact with the window manager (I will call this the 'UI thread'). This would be fine except that, when developing, most Common Lisp programmers use a system like `slime` or `sly` to connect their editor to their Lisp session, and those systems normally run code in a different thread.

The choices are then: frequently dispatch jobs to the 'UI thread' (and accept that overhead) or start `slime`/`sly` in a way that guarentees the thread. In CEPL we choose the latter as, although it does add one step to starting your project, it means you can ignore the detail whilst you are working.

To get a full breakdown of the above issue run `(cepl:make-project :why)` in your repl.

## Q: I got a `symbol's function definition is void: lexical-let` error, what happened?

Looks like the `cl` package wasn't required; please add `(require 'cl)` to your `.emacs` file
