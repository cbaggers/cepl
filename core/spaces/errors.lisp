(in-package :cepl.space)

(deferror from-ndc-or-screen-cpu-side () ()
    "Cepl.Spaces: Limitations in cpu-side space transforms from
                   *screen-space* & *ndc-space*

CEPL's `spaces` feature can transform between most spaces in the graph, however
there are limitations for *screen-space* & *ndc-space*

Transforming from *screen-space* or *ndc-space* back to *clip-space* (or earlier)
is not expressible as a matrix. Instead it is a function using data only[0]
available in the fragment shader (the gl-frag-coord).

CEPL therefore requires you to explicitly specify the variable *screen-space*
or *ndc-space* in your shader code and does not allow you to pass in either as a
uniform[1].

By having this limitation we guarentee we can give you the most performance we
can for these cases.

Notes:
[0] technically we could return a function that you then called with the
    screen-space vec4, however this would mean you couldnt be certain if you
    were going to recieve a matrix or a function requiring more conditionals
    and incurring a performance hit.
    CEPL prefers to optimize for the normal case and clarify the details of the
    edge cases in the documentation (and this error message)

[1] One possible way of avoiding this issue would be to upload a struct that
    contained a matrix or a flag specifying that a [ndc/screen]-space->space function
    should be used. However this would require lots of 'if' statements inside your
    glsl code, which is a great way to destroy the performace of your code.")

(deferror to-ndc-or-screen () ()
    "Cepl.Spaces: Un-implemented Transform Feature

Sorry we do not currently support transforming to *ndc-space* or *screen-space*
This is a gap in the api and will be looked at in future versions.")

(deferror from-ndc () ()
    "Cepl.Spaces: Un-implemented Transform Feature

Sorry we do not currently support transforming from *ndc-space*
This is a gap in the api and will be looked at in future versions.")
