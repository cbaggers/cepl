CEPL (Code Evaluate Play Loop)
==============================

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
* lispbuilder-mini *(or lispbuilder <see end for note>)*
* varjo *used to transalate common-lisp into glsl*
* cl-utilities
* cl-ppcre
* symbol-munger
* classimp *(requires assimp3)*

If you are using quicklisp then drop this in your local-projects directory and run the following in your repl:
    (ql:quickload :cepl)
    (cepl:repl)

*Note regarding lispbuilder*
Lispbuilder is awesome but compatibilty is not complete yet as 
lispbuilder-mini is changing so fast. As soon as possible though
I will make sure that you can just uncomment a couple of lines
in the package.lisp file and be under way using lispbuilder.

**Driving Ideas**
(making (making a game) a game)
