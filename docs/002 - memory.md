# Memory

### Where's my data?

With CEPL data can exist in three different places:

- lisp memory - The regular lisp data. This get's GC'd (garbage collected)
- c memory - Data stored in block of cffi memory. This does not get GC'd
- gpu memory - Data stored on the GPU. This does not get GC'd

CEPL makes moving data between these places easy.

### GC

Garbage collection is our friend in lisp. GC facilitates the use our REPLs for experimental bliss and allows for data structures that would be a nightmare to maintain otherwise.

However, in realtime graphics, the GC *can* be a problem. Stable frame rate is a major requirement, and spikes in GC activity can cause problems. It is clearly possible to have a GC and make games, but the core systems have to be written sensibly to ensure they aren't adding load.

Communication with the GPU at the wrong time can be very expensive. Freeing GPU memory used by the current render state is a recipe for disaster.  To this end, by default, none of the data in c-memory or gpu-memory is GC'd. This obviously means we need to free this data ourselves, or end up with memory leaks.

We say "by default" as there are certain times GC may be worth the cost. For example, when we are playing with ideas in the REPL: interactive environments tend to accumulate garbage. To this end, we are exploring ways of providing this capability without compromising any performance in the rest of CEPL. This is work in progress, so this document will be updated as changes are made.

### #'free and #'free-*

To release memory we have two options, firstly we can use the generic function #'free as in
```
(free some-cepl-data)
```
However, in critical code, the dispatch cost may be considerable; there are a number of `free-*` functions (e.g. #'free-c-array #'free-gpu-array etc) that operate on specific types directly. These functions will be covered in their respective chapters.

### #'pull-g & #'push-g

These are two very helpful generic functions that move data between Lisp, c-memory & gpu-memory with ease:

`push-g` takes a Lisp array or Lisp list and pushes the data into a c-array or a gpu-array;

`pull-g` pulls data from c-array or gpu-arrays and brings it back into lisp.

Obviously, the generic dispatch and working out how to upload the data to a given target take time, so these are generally used in non performance critical places, like the REPL.

There is more to this, but we will revisit this topic in a later chapter.
