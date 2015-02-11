CEPL (Code Evaluate Play Loop) - [PRE-ALPHA]
============================================

*"Lisp isn't a language, it's a building material." - Alan Kay*

The goal of CEPL is to provide building materials for realtime graphics demos and games.

CEPL will not be an engine for a certain class of game but will provide primitives to aid
in building graphical tools or engines.

CEPL aims to provide or extend wrappers around core technologies (opengl, sdl, etc) and make
them lispy and REPL friendly. The latter of those two points is worth noting. Too often an attempt
to abstract complex ideas can produce tools which are great for handling the complex situation but they can end up ballooning simple examples and intimidating beginners.

CEPL is in pre-alpha. Everything is subject to change and *many* bugs are present in the code. Have fun!

See the ./examples folder for experiments with opengl which is the current focus of the devlopment.

Videos: http://www.youtube.com/playlist?list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y

-----

**Requirements**

* cl-opengl
* cl-sdl2
* varjo (which you can find at https://github.com/cbaggers/varjo)
* cl-utilities
* cl-ppcre
* symbol-munger

If you are using quicklisp then drop this in your local-projects directory and run the following in your repl:
    (ql:quickload :cepl)
    (cepl:repl)

**Running on OSX**

OSX has a couple of issues worth noting.
* You must be using a version of sbcl compiled wih native threads enabled.
* By default OSX gives programs a opengl 2.1 context. Cepl requests a higher one but will fail if this isnt successful. If you get an error based on opengl version please file a bug report.

**Driving Ideas**
(making (making a game) a game)
