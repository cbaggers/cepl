## CEPL (Code Evaluate Play Loop) - [Beta]

CEPL is a lispy and REPL-friendly Common Lisp library for working with OpenGL.

Its definition of success is making the user feel that GPU programming has always been part of the languages standard.

The usual approach to using CEPL is to start it at the beginning of your Lisp session and leave it open for the duration of your work. You can then treat the window it creates as just another output for your graphics, analogous to how `*standard-output*` is treated for text.

CEPL is in beta. The API is close to what it needs to be but there are still many bugs to fix, features to add, and experiences to smooth out.

See the [cepl.examples repository](https://github.com/cbaggers/cepl.examples) for some examples of how CEPL can be used

Videos: http://www.youtube.com/playlist?list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y

### Documentation

Currently we have full documentation of every exported symbol in the CEPL package. You can find this [here: CEPL API Docs](http://techsnuffle.com/cepl/api.html)

Guides will be provided in future, however these take much longer to write.

I can also be reached by my email (techsnuffle [at] gmail · com) and sometimes on #lispgames IRC. Come to #lispgames anyway though, there are some lovely folks, all lispy dialects welcome!

### Requirements

All of the following will be downloaded automatically by quicklisp

* cffi
* cl-autowrap
* cl-fad
* cl-opengl
* cl-plus-c
* cl-ppcre
* documentation-utils
* fn
* named-readtables
* rtg-math
* varjo


#### C Library dependency

CEPL uses OpenGL so you need to make sure it is available on your machine. Installing your GPU drivers will usually handle this.


#### CEPL's Host

CEPL abstracts working with OpenGL but is not responsible for creating a window or GL context; this is handled by a `Host`. Right now the only supported host is `SDL2`; the system to load is called `cepl.sdl2`, you can find it here: [cepl.sdl2](https://github.com/cbaggers/cepl.sdl2)


### Getting Started

_Note:_ On `Windows` and `OSX`, `slime` users may want to add the code specifed in `docs/single-thread-swank.md` to their Emacs config file, and use the command `slime-style` which will start `slime` in a more OpenGL friendly mode. Then follow the rest of this as usual.

To load CEPL and the default host (`sdl2`) do the following:

- `(ql:quickload :cepl.sdl2)`
- `(cepl:repl)`

You should see an empty window appear, OpenGL is now initialized, and you can use CEPL as you like.


### Making a CEPL Project

The best way to get started is to make a new project that uses CEPL. Do the following in your REPL to get set up:

- First, run `(ql:quickload :cepl)`
- Then run `(ql:quickload :quickproject)`. CEPL uses this to create a lisp project using its own templates
- Then run `(cepl:make-project "my-proj")`. This will use quickproject to make a new project with all the correct dependencies. Remember that cepl does not handle window managers or input so by default your new project will use the following
 - cepl for the graphics
 - [cepl.sdl2](https://github.com/cbaggers/cepl.sdl2) for the host
 - [skitter](https://github.com/cbaggers/skitter) for handling input and events
 - [cepl.devil](https://github.com/cbaggers/cepl.devil) for loading images

You are now ready to get started. Simply run:
- `(ql:quickload "my-proj")`
- `(in-package :my-proj)`
- and finally (if you havent already) `(cepl:repl)`


#### Windows C Library Hack

If you are having issues getting the C libraries to load and just need to rule out whether Lisp can find them, try putting them in the same folder as the lisp exe. For example `C:\Program Files\sbcl\`.
