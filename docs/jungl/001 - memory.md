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

### #'free and #'free-*

To release memory we have two options, firstly we can use the generic function #'free as in

```
(free some-jungl-data)
```

However you may not want to pay the dispatch cost in performance critical code, so there are a number of `free-*` functions (e.g. #'free-c-array #'free-gpu-array etc). These specific functions will be covered in their respective chapters.

### #'pull-g & #'push-g

These are two very helpful generic functions that let you move data between lisp, c-memory &
gpu-memory with ease!

`push-g` will take a lisp array or lisp list and push the data into a c-array or a gpu-array (dont worry if you dont know what these are yet)

`pull-g` will pull data from c-array ＆ gpu-arrays and bring it back into lisp.

Obviously the generic dispatch and working out how to upload the data to a given target takes time, so these are generally used in non performance critical places, like the repl!

There is also a bit more to this but we will revisit these in a later chapter.
