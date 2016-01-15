# Memory

### Where's my data?

With Jungl your data can exist in 3 different places:

- lisp memory - The regular lisp data in your program. This get's GC'd (garbage collected)
- c memory - Data stored in block of cffi memory. This does not get GC'd
- gpu memory - Data stored on the GPU. This does not get GC'd

A lot of work done in jungl is about making moving data between these places easy.

### GC

Garbage collection is our friend in lisp. It let's us use our repls for experimental bliss and makes possible the kinds of data structures that would be a knightmare to keep in order without a very special data ownership model.

However in realtime graphics the GC *can* be a problem. One of the things we end up working very hard for is a stable frame rate which makes spikes in GC activity a problem. It clearly is possible to have a GC and make games but the core system have to be written sensibly to ensure they arent adding load.

On top of this, communication with the gpu can be very expensive if done at the wrong time. Deciding to free a bunch of gpu memory without being very aware of the current render state is a recipe for disaster.

To this end, by default, none of the data in c-memory or gpu-memory is GC'd. This obviously means you need to free this data yourself or you end up with memory leaks.

We say by default as there are certain times you don't mind paying the cost. For example when you are playing with ideas in the repl. To this end we are looking into ways of providing this without compromising any performance in the rest of jungl. This is WIP so this doco will be updated when that is in place.

### #'pull-g & #'push-g

Two helpful generic functions that
