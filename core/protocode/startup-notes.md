# Startup

Ah, so it's time to revisit this madness. Let's follow the 'logic' from `#'repl` and `#'initialize` and see where they go.

## Load time stuff

Ok so all the globals are gonna get initialzied, of those 2 stand out `*contexts*` & `*cepl-context*`.

    context/cepl-context.lisp:7:(defvar *contexts* nil)
    context/cepl-context.lisp:56:(defvar *cepl-context* (make-context))

Let's take a peek at `#'make-context` as there is probably a lot going on there.

So straight away it is grabbing the `current-thread` and asserting that nothing else in on this thread.

It computes the gl version (from user input or existing context's) it makes the `shared` array for this context (this holds the context and any contexts shared with the context).

Then, if a host has been loaded, it calls `on-host-initialized`..which is interesting as Im not stoked on the idea of this context saying it's initialized yet.

It then pushes itself on the `*contexts*` list and returns our new context.

Straight up, capturing the thread at this stage seems wrong. We need to have a seperate `initialize` stage for the context, this will put the context 'in circulation' and make sure everything is set up. Currently our context doesnt even have a GL context (which is fine in that it is a CEPL context, but still..)

### ON-HOST-INITIALIZED

A little detour here, I just needed to see what was going on in here.

It takes a context, checks if it initialized by calling `initialized-p`...which always returns `T`..fucks sake

If it worked it would check for multiple contexts, init-pending-surfaces  and make the first surface current..of course this will never happen :|

## (CEPL:REPL)

Ok so this simply kicks off `initialize-cepl` and then, once that is complete, calls `legacy-add-surface` with a pile of details. I'm guessing this helps in the case on v0 host.

## INITIALIZE-CEPL

First it makes sure that cepl is not already initialized, then it:

- grabs a copy of the contexts
- calls cepl.host::initialize
- loop over the contexts calling:
 - patch-uninitialized-context-with-version (as we know the version now apparently)
 - on-host-initialized..which we already know is shit
- finally change the lifecycle state to active.

Well, this is all slightly fucked then :D

## PATCH-UNINITIALIZED-CONTEXT-WITH-VERSION

This just runs through and, for those contexts that dont have 'requested-gl-version' give it one (in our case the one passed to `initialize-cepl`)

## LEGACY-ADD-SURFACE

after checking for multiple surface support in the host this function makes a pending-surface.

Then if there is already has a host it calls `make-surface-from-pending` which, usually, will create a window. For system like glfw this may also be when the gl-context is created. but for some it wont be GAH hosts.

Finally the surface is made current. At this point we NEED a GL context and so this is where init-gl-context is called if it hasnt already been set. Then the context is made current on the surface.
