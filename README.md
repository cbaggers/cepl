# This is broken right now as am in middle of massive refactor

CEPL (Code Evaluate Play Loop) - [ALPHA]
============================================

*"Lisp isn't a language, it's a building material." - Alan Kay*

The goal of CEPL is to provide building materials for realtime graphics demos and games.

CEPL will not be an engine for a certain class of game but will provide primitives to aid
in building graphical tools or engines.

CEPL aims to provide or extend wrappers around core technologies (opengl, sdl, etc) and make
them lispy and REPL friendly. The latter of those two points is worth noting. Too often an attempt
to abstract complex ideas can produce tools which are great for handling the complex situation but they can end up ballooning simple examples and intimidating beginners.

CEPL is in alpha. The API is closing in the what it needs to be but there are still many bugs.

See the ./examples folder for experiments with opengl which is the current focus of the devlopment.

Videos: http://www.youtube.com/playlist?list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y

-----------------------------------------------------------------------------------------

**Requirements**

All of the following will be download automatically by quicklisp

* cffi
* cl-autowrap
* cl-plus-c
* cl-fad
* cl-opengl
* swank
* cl-ppcre
* named-readtables
* fn

These three will hopefully be in quicklisp at some point, until then download them at the links provided.

* rtg-math - https://github.com/cbaggers/rtg-math
* varjo - https://github.com/cbaggers/varjo

-----------------------------------------------------------------------------------------

If this is your first time running cepl, use the 'Getting Started' instructions below. Whilst not neccesary it really does help debugging if one of the steps goes wrong.

**Running on Linux or Windows**
- Start slime
- `(ql:quickload :cepl-default)`
- `(cepl:repl)`

**Running on OSX**

If using sbcl

- run platform-specific/osx-sbcl-launch.sh from the terminal
- slime-connect
- `(ql:quickload :cepl-default)`
- `(cepl:repl)`

else

- Start your lisp's repl in the terminal
- `(ql:quickload :cepl-osx)`
- `(cepl-osx:start)`
- slime-connect
- `(ql:quickload :cepl-default)`
- `(cepl:repl)`

-----------------------------------------------------------------------------------------

Getting Started
===============
- `cd` to your quicklisp/local-projects folder
- clone `this repo`& `varjo`

Now most days you will use `(ql:quickload :cepl-default)` to load all of cepl and it's supporting libs. However this first time please do the following. The reason for following these longer steps is to make it easy to see where the issues are.

Step 1
------
`(ql:quickload :cepl)` Cepl is a standalone package so there should be no issues here. It is, however, useless without a backend to create the opengl context and provide input events.

Step 2
------
`(ql:quickload :cepl-backend-sdl)` This is currently the only supported backend. Common lisp will need to be able to find the sdl2 c library. On linux PLEASE use your package manager. On osx use whichever package manager you prefer. On windows download from here: https://www.libsdl.org/download-2.0.php

At this point you should be able to run the following examples. See further down for advice on running the examples.
```
triangle.lisp
basic-3d-objects.lisp
texture-example.lisp
ubo-test.lisp
moving-triangles.lisp
raymarcher.lisp
```

Step 3
------
`(ql:quickload :cepl-image-helper)` This uses the equally excellent cl-devil wrapper around the devil c library. You can guess the steps by now... Linux or osx -> Package Manager, Windows -> http://openil.sourceforge.net/download.php

At this point you should be able to run the following examples
```
bloom.lisp
cubemap.lisp
```

Step 4
------
`(ql:quickload :cepl-model-helper)` This relies on the wonderful classimp library which is a wrapper around the assimp c library. Again for linux and osx use your package manager. On windows download it here http://assimp.sourceforge.net/main_downloads.html

You should now be able to run the following examples
```
instancing.lisp
normal-mapping.lisp
refraction.lisp
```

DONE
----
Let's run an example :)

**Windows C Library Hack**

If you are having issues getting the c libraries to load and just need to rule out whether lisp can find them, try putting them in the same folder as the lisp exe. For example `C:\Program Files\sbcl\`.

-----------------------------------------------------------------------------------------

Running an example
------------------
To do this we launch the cepl repl and compile the example.

Step 1
------
In your repl evaluate `(cepl:repl)` You should see a 320x240 pixel window appear. It will almost certainly look like it is not responding, this is normal as we aren't handling events yet. Don't worry!

Step 2
------
Evaluate `(in-package :cepl)`

Step 3
------
Open the `moving-triangles.lisp` file in the examples directory and `C-c C-k` to compile the whole file

Step 4
------
Evaluate `(run-loop)` in the repl

Step 5
------
Enjoy! This is very alpha software but is still fun when you don't crash into bugs! Report the ones you find and please consider making a fix or two yourself.

Thankyou so much for taking an interest.

Baggers

-----------------------------------------------------------------------------------------

**Driving Ideas**

(making (making a game) a game)
