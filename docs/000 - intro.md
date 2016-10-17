# CEPL - (Alpha)

CEPL is a sane interface to the modern OpenGL API.

### OpenGL Background

OpenGL gets a bad rap for its API, and for good reason: it is actually two (pretty much) incompatible API's in one box.

The first API is the old fixed-function pipeline, and the second is the modern shader-based pipeline. The ugly thing is that these APIs often reuse function names, even when the behaviour underneath is very different. This makes for a very fustrating experience.

The belief held by CEPL is that there is a good API in OpenGL, it's just very well hidden :)

### CEPL's Goals

- Make a sane interface to the modern OpenGL API.
- Make working with OpenGL lispy & repl friendly
- Do the above without losing performance (see Performance section below)

### Performance

The third and second goals can often be at odds with each other in a project like this. We want to get the absolute maximum performance out of Common Lisp, *but* we also adore the repl and the power it brings.

The strategy, therefore, is to always have a codepath that is optimized as hard as possible (whilst being as pleasant as possible). In those situations where the abstractions are not repl-friendly, we create a more generic codepath that sacrifices a bit of performance for coder-experience. This should be minimized. Very high levels of abstractions tend to be geared to a certain aproach or technique, and those are best kept in a seperate library that depends on CEPL.

### Bugs and Unsupported Features

Any commonly used features of modern OpenGL that are either impossible or prohibitively slow in CEPL are likely to be considered a bug.

Another goal is to document all unsupported features in [wont-support.md](./wont-support.md) along with the justification for not supporting it. We can't guarentee that the reason will be satisfying, but it should be in keeping with the project.
