## CEPL (Code Evaluate Play Loop) - [Beta]

CEPL is a lispy and REPL-friendly Common Lisp library for working with OpenGL.

Its definition of success is making the user feel that GPU programming has always been part of the languages standard.

The usual approach to using CEPL is to start it at the beginning of your Lisp session and leave it open for the duration of your work. You can then treat the window it creates as just another output for your graphics, analogous to how `*standard-output*` is treated for text.

CEPL is in beta. The API is close to what it needs to be but there are still many bugs to fix, features to add, and experiences to smooth out.

See the [cepl.examples repository](https://github.com/cbaggers/cepl.examples) for some examples of how CEPL can be used

Videos: http://www.youtube.com/playlist?list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y

### Installing

Run `(ql:quickload :cepl)` at your REPL.

### Cloning

Whilst it is recommended to get CEPL from quicklisp, if you clone please note that `master` is not the stable branch. Please use `release-quicklisp` for the stable code that will be in the next CEPL release.

### Documentation

Currently we have full documentation of every exported symbol in the CEPL package. You can find this [here: CEPL API Docs](http://techsnuffle.com/cepl/api.html)

Guides will be provided in future, however these take much longer to write.

I can also be reached by my email (techsnuffle [at] gmail · com) and sometimes on #lispgames IRC. Come to #lispgames anyway though, there are some lovely folks, all lispy dialects welcome!

### Requirements

All of the following will be downloaded automatically by quicklisp

- cffi
- cl-fad
- cl-opengl
- cl-plus-c
- cl-ppcre
- documentation-utils
- fn
- ieee-floats
- named-readtables
- varjo
- rtg-math
- rtg-math.vari
- closer-mop
- bordeaux-threads

#### C Library dependency

CEPL uses OpenGL ( version >= 3.1 ) so you need to make sure it is available on your machine. Installing your GPU drivers will usually handle this.


#### CEPL's Host

CEPL abstracts working with OpenGL but is not responsible for creating a window or GL context; this is handled by a `Host`. Right now the only supported host is `SDL2`; the system to load is called `cepl.sdl2`, you can find it here: [cepl.sdl2](https://github.com/cbaggers/cepl.sdl2)


### Getting Started

_Note:_ On `OSX`, `slime` users may want to add the code specifed in `docs/single-thread-swank.md` to their Emacs config file, and use the command `slime-style` which will start `slime` in a more OpenGL friendly mode. Then follow the rest of this as usual.

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
 - [dirt](https://github.com/cbaggers/dirt) for loading images
 There are many good event systems & image loaders out there so browse around. But here are two :)

You are now ready to get started. Simply run:
- `(ql:quickload "my-proj")`
- `(in-package :my-proj)`
- and finally (if you havent already) `(cepl:repl)`


#### Windows C Library Hack

If you are having issues getting the C libraries to load and just need to rule out whether Lisp can find them, try putting them in the same folder as the lisp exe. For example `C:\Program Files\sbcl\`.

## CHANGELOG

### 2017-06-04

- pipelines can take `:dynamic` as their draw mode. This means they will take the draw-mode from the `buffer-stream` they are mapped over. This only works for pipelines with `vertex` & `fragment` stages.

- `buffer-stream`s now hold the primitive type of their data. Defaults to `:triangles`

- Fix bug that was stopping g-structs contain matrices

- Cache more values in `buffer-stream` to save time during rendering

- Add `surface-dimensions`, `surface-resolution`, `surface-title` & `surface-fullscreen-p`

- add `adjust-gpu-array` (pretty basic right now)

- Remove `cepl.spaces`, It is now a seperate project (and will be in quicklisp in the next cycle)

- Remove `cepl.misc`. If you were using the `draw-texture` function then please consider `Nineveh` (which will be in quicklisp in the next cycle)

- `make-project` now uses `dirt` instead of `devil`. `dirt` uses `cl-soil` which ships with binaries for multiple paltforms so has a better 'out of the box' experience (plus also supports more formats)

### 2017-05-16

*I missed some logs here so this is a recap of everything since 2017-02-19*

- Geometry & Tessellation fully supported (including for inline glsl stages)

- Draw mode can now be specified for pipelines

- fixes for pull-g with gpu-functions & pipelines

- add `with-gpu-array-range-as-pointer` & `with-gpu-array-range-as-c-array`. These still feel experimental to me.

- add `reallocate-gpu-array` & `reallocate-buffer`

- buffer-streams always hold on to their gpu-arrays by default

- Refactoring based on changes in Varjo

- Added bordeux-threads as a dependency. Will be needed for some context related things

- Very basic support for multiple surfaces (windows)

- New 'host' api. Is versioned so old hosts are still supported

- remove `run-session`. All of these attempts at thread hackery felt bad. I'm sticking with `slime-style` until we have a better fix


### 2017-02-19
- Removed the `continuable` macro. The macro can be found in the `livesupport` project. Simply `(ql:quickload :livesupport)`
