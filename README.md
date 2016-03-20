## CEPL (Code Evaluate Play Loop) - [ALPHA]

CEPL is a lispy and REPL friendly library for working with OpenGL.

It's goal is to feel like lisp just always had support for GPU programming in the standard.

The usual approach to using it is to start it at the beginning of your lisp session and leave it open for the duration of your work. You can then treat the window it creates as just another output for your graphics, analogous to how `*standard-output*` is treated for text.

CEPL is in alpha. The API is closing in the what it needs to be but there are still many bugs.

See the [cepl.examples repository](https://github.com/cbaggers/cepl.examples) for some examples of how cepl can be used

Videos: http://www.youtube.com/playlist?list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y

-----------------------------------------------------------------------------------------

### Requirements

All of the following will be download automatically by quicklisp

* cffi
* cl-autowrap
* cl-plus-c
* cl-fad
* cl-opengl
* cl-ppcre
* named-readtables
* fn
* rtg-math
* varjo

#### C Library dependency

Cepl uses OpenGL so you need to make sure this is available on your machine. Installing your GPU drivers will usually handle this.

#### Cepl's Host

Cepl abstracts working with OpenGL but is not responsible for creating a window or GL context, this is handled by a `Host`. Right now the only supported host is `SDL2`, the system to load is called `cepl.sdl2`, you can find it here: [cepl.sdl2](https://github.com/cbaggers/cepl.sdl2)

-----------------------------------------------------------------------------------------

### Getting Started

The best way to get started is to make a new project that uses cepl. Do the following in your repl to get set up:

- First, run `(ql:quickload :cepl)`
- Then run `(ql:quickload :quickproject)`. Cepl uses this to create a lisp project using it's own templates
- Then run `(cepl:make-project "my-proj")`. This will use quickproject to make a new project with all the correct dependencies. Remember that cepl does not handle window managers or input so by default your new project will use the following
 - cepl for the graphics
 - [cepl.sdl2](https://github.com/cbaggers/cepl.sdl2) for the host
 - [skitter](https://github.com/cbaggers/skitter) for handling input and events
 - [cepl.devil](https://github.com/cbaggers/cepl.devil) for loading images

You are now ready to get started. How this is done varies a little between OS:

#### Linux

Simply run:
- `(ql:quickproject "my-proj")`
- `(in-package :my-proj)`
- and lastly `(cepl:repl)`

This will bring use the host (`sdl2`) to create a window & initialize opengl. You now are ready to go, have fun!

#### Windows & OSX

- In your new `my-proj` directory you will find a file called `run-session.lisp`
- Run your lisp, loading this file. I use sbcl so it looks like this `sbcl --load "run-session.lisp`.
- You can then connect your editor to this session (at port 4005) and carry on as usual. I use `slime` and `emacs` so I type `M-x slime-connect` and hit the `return` key twice.
- Now you can run `(in-package :my-proj)` and `(cepl:repl)`.

This will bring use the host (`sdl2`) to create a window & initialize opengl. You now are ready to go, have fun!

#### Q: Why is the Windows/OSX start procedure more complicated?

*A:* Both these platforms have restrictions over which thread is allowed to interact with the window manager (I will call this the 'UI thread'). This would be fine except that, when developing, most common-lisp programmers use a system like `slime` or `sly` to connect their editor to their lisp session and those systems normally run your code in a different thread.

The choices are then to frequently dispatch jobs to the 'UI thread' (and accept that overhead) or start `slime`/`sly` in a way that guarentees the thread. In cepl we choose the latter as, although it does add one step to starting your project, it means you can ignore the detail whilst you are working.

To get a full breakdown of the above issue run `(cepl:make-project :why)` in your repl.

**Windows C Library Hack**

If you are having issues getting the c libraries to load and just need to rule out whether lisp can find them, try putting them in the same folder as the lisp exe. For example `C:\Program Files\sbcl\`.
