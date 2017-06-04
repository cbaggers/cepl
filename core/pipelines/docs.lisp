(in-package :cepl.pipelines)

(docs:define-docs
  (defmacro defun-g
      "
Defun-g let's you define a function which will be run on the gpu.
Commonly refered to in CEPL as a 'gpu function' or 'gfunc'

Gpu functions try to feel similar to regular CL functions however naturally
there are some differences.

The first and most obvious one is that whilst gpu function can be called
from other gpu functions, they cannot be called from lisp functions directly.
They first must be composed into a pipeline using def-g->.

When a gfunc is composed into a pipeline then that function takes on the role of
one of the 'shader stages' of the pipeline. For a proper breakdown of pipelines
see the docstring for def-g->.


Let's see a simple example of a gpu function we can then break down

    ;;       {0}          {3}          {1}         {2}
    (defun-g example ((vert my-struct) &uniform (loop :float))
      (values (v! (+ (my-struct-pos vert) ;; {4}
                     (v! (sin loop) (cos loop) 0))
                  1.0)
              (my-struct-col vert)))


{0} So like the normal defun we specify a name first, and the arguments as a
    list straight afterwards

{1} The &uniform lambda keyword says that arguments following it are 'uniform
    arguments'. A uniform is an argument which has the same value for the entire
    stage.
    &optional and &key are not supported

{2} Here is our definition for the uniform value. If used in a pipeline as a
    vertex shader #'example will be called once for every value in the
    gpu-stream given. That means the 'vert' argument will have a different value
    for each of the potentially millions of invocations in that ONE pipeline
    call, however 'loop' will have the same value for the entire pipeline call.

{2 & 3} All arguments must have their types declared

{4} Here we see we are using CL's values special form. CEPL fully supports
    multiple value return in your shaders. If our function #'example was called
    from another gpu-function then you can use multiple-value-bind to bind the
    returned values. If however our example function were used as a stage in a
    pipeline then the multiple returned values will be mapped to the multiple
    arguments of the next stage in the pipeline.

That's the basics of gpu-functions. For more details on how they can be used
in pipelines please see the documentation for def-g->.
")

  (defmacro defun-g-equiv
      "
Defun-g-equiv let's you define a function which will be run on the gpu.
Commonly refered to in CEPL as a 'gpu function' or 'gfunc'

The difference between defun-g-equiv & defun-g is that defun-g will create
a 'dummy' lisp function so that 'jump to definition' and signature hits work
in your editor, defun-g-equiv does not do this.

The advantage of defun-g-equiv is that you are then free define a lisp
equivalent of your gpu-function. This means you can use the same functions in
cpu or gpu code, which is very compelling.

*- the rest of the doc-string is the same as for defun-g -*

Gpu functions try to feel similar to regular CL functions however naturally
there are some differences.

The first and most obvious one is that whilst gpu function can be called
from other gpu functions, they cannot be called from lisp functions directly.
They first must be composed into a pipeline using def-g->.

When a gfunc is composed into a pipeline then that function takes on the role of
one of the 'shader stages' of the pipeline. For a proper breakdown of pipelines
see the docstring for def-g->.


Let's see a simple example of a gpu function we can then break down

    ;;       {0}          {3}          {1}         {2}
    (defun-g-equiv example ((vert my-struct) &uniform (loop :float))
      (values (v! (+ (my-struct-pos vert) ;; {4}
                     (v! (sin loop) (cos loop) 0))
                  1.0)
              (my-struct-col vert)))


{0} So like the normal defun we specify a name first, and the arguments as a
    list straight afterwards

{1} The &uniform lambda keyword says that arguments following it are 'uniform
    arguments'. A uniform is an argument which has the same value for the entire
    stage.
    &optional and &key are not supported

{2} Here is our definition for the uniform value. If used in a pipeline as a
    vertex shader #'example will be called once for every value in the
    gpu-stream given. That means the 'vert' argument will have a different value
    for each of the potentially millions of invocations in that ONE pipeline
    call, however 'loop' will have the same value for the entire pipeline call.

{2 & 3} All arguments must have their types declared

{4} Here we see we are using CL's values special form. CEPL fully supports
    multiple value return in your shaders. If our function #'example was called
    from another gpu-function then you can use multiple-value-bind to bind the
    returned values. If however our example function were used as a stage in a
    pipeline then the multiple returned values will be mapped to the multiple
    arguments of the next stage in the pipeline.

That's the basics of gpu-functions. For more details on how they can be used
in pipelines please see the documentation for def-g->.
")

  (defmacro def-g->
      "
def-g-> is how we define named rendering pipelines in CEPL.

Rendering pipelines are constructed by composing gpu-functions.

Rendering in OpenGL is descibed as a pipeline where a buffer-stream of data
usually describing geometry) is mapped over whilst a number of uniforms are
available as input and the outputs are written into an FBO.

There are many stages to the pipeline and a full explanation of the GPU pipeline
is beyond the scope of this docstring. However it surfices to say that only
5 stages are fully programmable (and a few more customizable).

def-g-> lets you specify the code (shaders) to run the programmable
parts (stages) of the pipeline.

The available stages kinds are:

- :vertex
- :tessellation-control
- :tessellation-evaluation
- :geometry
- :fragment

To define code that runs on the gpu in CEPL we use gpu functions (gfuncs). Which
are defined with defun-g.

Here is an example pipeline:

    (defun-g vert ((position :vec4) &uniform (i :float))
      (values position (sin i) (cos i)))

    (defun-g frag ((s :float) (c :float))
      (v! s c 0.4 1.0))

    (def-g-> prog-1 ()
      (vert :vec4)
      (frag :float :float))

Here we define a pipeline #'prog-1 which uses the gfunc vert as its vertex
shader and used the gfunc frag as the fragment shader.

It is also possible to specify the name of the stages

    (def-g-> prog-1 ()
      :vertex (vert :vec4)
      :fragment (frag :float :float))

But this is not neccesary unless you need to distinguish between tessellation
or geometry stages.

-- Context --

The second argument to def-g-> is the a list of additional information that is
confusingly called the 'pipeline's context'. We need to change this name.

Valid things that can be in this list are:

- A primitive type: This specifies what primitives can be passed into this pipeline.
  By default all pipelines expect triangles. When you map a buffer-stream over a
  pipeline the primitive kind of the stream must match the pipeline.

  The valid values are:
  :dynamic
  :points
  :lines :line-loop :line-strip
  :lines-adjacency :line-strip-adjacency
  :triangles :triangle-fan :triangle-strip
  :triangles-adjacency :triangle-strip-adjacency
  (:patch <patch-size>)

  :dynamic is special, it means that the pipeline will take the primitive kind
  from the buffer-stream being mapped over. This won't work for with pipelines
  with geometry or tessellation stages, but it otherwise quite useful.

- A version restriction: This tells CEPL to compile the stage for a specific
  version of GLSL. You usually do not want to use this as CEPL will compile for
  the version the user is using.
  The value can be one of:
  :140 :150 :330 :400 :410 :420 :430 :440 :450

-- Stage Names --

Notice that we have to specify the typed signature of the stage. This is because
CEPL allows you to 'overload' gpu functions. The signature for the a
gpu-function is a list which starts with the function name and whose other
elements are the types of the non-uniforms arguments. As an example we can see
above that the signature for vert is (vert :vec4), not (vert :vec4 :float).

-- Passing values from Stage to Stage --

The return values of the gpu functions that are used as stages are passed as the
input arguments of the next. The exception to this rule is that the first return
value from the vertex stage is taken and used by GL, so only the subsequent
values are passed along.

We can see this in the example above: #'vert returns 3 values but #'frag only
receives 2.

The values from the fragment stage are writen into the current FBO. This may be
the default FBO, in which case you will likely see the result on the screen, or
it may be a FBO of your own.

By default GL only writed the fragment return value to the FBO. For handling
multiple return values please see the docstring for with-fbo-bound.

-- Using our pipelines --

To call a pipeline we use the map-g macro (or one of its siblings
map-g-into/map-g-into*). The doc-strings for those macros go into more details
but the basics are that map-g maps a gpu-stream over our pipeline and the
results of the pipeline are fed into the 'current' fbo.

We pass our stream to map-g as the first argument after the pipeline, we then
pass the uniforms in the same style as keyword arguments. For example let's see
our prog-1 pipeline again:

    (defun-g vert ((position :vec4) &uniform (i :float))
      (values position (sin i) (cos i)))

    (defun-g frag ((s :float) (c :float))
      (v! s c 0.4 1.0))

    (def-g-> prog-1 ()
      (vert :vec4)
      (frag :float :float))

We can call this as follows:

    (map-g #'prog-1 v4-stream :i game-time)


")

  (defmacro map-g
      "
The map-g macro maps a gpu-stream over our pipeline and the results of the
pipeline are fed into the 'current' fbo.

This is how we run our pipelines and thus is how we render in CEPL.

The arguments to map-g are going to depend on what gpu-functions were composed
in the pipeline you are calling. However the layout is always as follows.

- the pipeline function: The first argument is always the pipeline you wish to
  map the data over.

- The stream: The next argument will be the gpu-stream which will be used as the
  inputs to the vertex-shader of the pipeline. The type of the gpu-stream  must
  be mappable onto types of the non uniform args of the gpu-function being used
  as the vertex-shader.

- Uniform args: Next you must provide the uniform arguments. These are passed in
  the same fashion as regular &key arguments.

CEPL will then run the pipeline with the given args and the results will be fed
into the current FBO. If no FBO has been bound by the user then the current FBO
will be the default FBO which will most likely mean you are rendering into the
window visable on your screen.

If an FBO has been bound then the value/s from the fragment shader will be
written into the attachments of the FBO. To control this please see the
doc-string for with-fbo-bound. The default behaviour is that each of the
multiple returns values from the gpu-function used as the fragment shader will
be written into the respective attachments of the FBO (first value to first attachment, second value to second attachment, etc)
")

  (defmacro map-g-into
      "
The map-g-into macro maps a gpu-stream over our pipeline and the results of the
pipeline are fed into the supplied fbo.

This is how we run our pipelines and thus is how we render in CEPL.

The arguments to map-g-into are going to depend on what gpu-functions were
composed in the pipeline you are calling. However the layout is always as
follows:

- target fbo: This is where the results of the pipeline will be written.

- the pipeline function: The first argument is always the pipeline you wish to
  map the data over.

- The stream: The next argument will be the gpu-stream which will be used as the
  inputs to the vertex-shader of the pipeline. The type of the gpu-stream  must
  be mappable onto types of the non uniform args of the gpu-function being used
  as the vertex-shader.

- Uniform args: Next you must provide the uniform arguments. These are passed in
  the same fashion as regular &key arguments.

CEPL will then run the pipeline with the given args and the results will be fed
into the specified FBO. The value/s from the fragment shader will be
written into the attachments of the FBO. If you need to  control this in the
fashion usualy provided by with-fbo-bound then please see the doc-string for
 map-g-into*.

The default behaviour is that each of the multiple returns values from the
gpu-function used as the fragment shader will be written into the respective
attachments of the FBO (first value to first attachment, second value to
second attachment, etc)

Internally map-g-into wraps call to map-g in with-fbo-bound. The with-fbo-bound
has its default configuration which means that:

- the viewport being will be the dimensions of the gpu-array in the first fbo attachment
- and blending is enabled

If you want to use map-g-into and have control over these options please use
map-g-into*
")

  (defmacro map-g-into*
      "
The map-g-into* macro is a variant of map-g-into which differs in that you have
more control over how the fbo is bound.

Like map-g-into, map-g-into* maps a gpu-stream over our pipeline and the
results of the pipeline are fed into the supplied fbo.

This is how we run our pipelines and thus is how we render in CEPL.

The arguments to map-g-into* are going to depend on what gpu-functions were
composed in the pipeline you are calling. However the layout is always as
follows.

- fbo: This is where the results of the pipeline will be written.

- with-viewport: If with-viewport is t then with-fbo-bound adds a
                 with-fbo-viewport that uses this fbo to this scope. This means
                 that the current-viewport within this scope will be set to the
                 equivalent of:

                     (make-viewport dimensions-of-fbo '(0 0))

                 See the docstruct with-fbo-viewport for details on this
                 behavior.

                 One last detail is that you may want to take the dimensions of
                 the viewport from an attachment other than attachment-0.
                 To do this use the 'attachment-for-size argument and give the
                 index of the color-attachment to use.

- with-blending: If with-blending is t then with-fbo-bound adds a with-blending
                 that uses this fbo to this scope.
                 This means that the blending parameters from your fbo will be
                 used while rendering. For the details and version specific
                 behaviours check out the docstring for with-blending

- attachment-for-size: see above

- the pipeline function: The first argument is always the pipeline you wish to
  map the data over.

- The stream: The next argument will be the gpu-stream which will be used as the
  inputs to the vertex-shader of the pipeline. The type of the gpu-stream  must
  be mappable onto types of the non uniform args of the gpu-function being used
  as the vertex-shader.

- Uniform args: Next you must provide the uniform arguments. These are passed in
  the same fashion as regular &key arguments.

CEPL will then run the pipeline with the given args and the results will be fed
into the specified FBO. The value/s from the fragment shader will be
written into the attachments of the FBO. If you need to control this in the
fashion usualy provided by with-fbo-bound then please see the doc-string for
 map-g-into*.

The default behaviour is that each of the multiple returns values from the
gpu-function used as the fragment shader will be written into the respective
attachments of the FBO (first value to first attachment, second value to
second attachment, etc)

Internally map-g-into* wraps call to map-g in with-fbo-bound. The with-fbo-bound
has its default configuration which means that:
")

  (defmacro def-glsl-stage
      "
def-glsl-stage is useful when you wish to define a CEPL pipeline stage in glsl
rather than lisp. This is especially useful if you want to use some
pre-exisiting glsl without rewriting it to lisp.

It is used like this:

    (def-glsl-stage frag-glsl ((\"color_in\" :vec4) &context :330 :fragment)
      \"void main() {
           color_out = color_in;
       }\"
      ((\"color_out\" :vec4)))

It differs from a regular defun-g definition in a few ways.

- argument names are specified using strings.

- &context is mandatory. You must specify what shader stage this can be used for
  and also what version/s this stage requires

- You are defining the entire stage, not just a function body. This means you
  can define local shader functions etc

- You have to specify the outputs in lisp aswell as the inputs. This allows CEPL
  to compose this stage in pipelines with regular CEPL gpu functions.

CEPL will write all the in, out and uniform definitions for your shader so do
not specify those yourself.

This stage fully supports livecoding, so feel free to change and recomplile the
text in the stage at runtime.
")

  (defmacro defmacro-g
      "
This lets you a define a macro that only works in gpu code.

The &context lambda list keyword allows you to restrict this macro to only be
valid in gpu functions with compatible contexts.

&whole and &environment are not supported.
")

  (defmacro define-compiler-macro-g
      "
This lets you define a compiler-macro that only works with gpu-functions.

The &context lambda list keyword allows you to restrict this macro to only be
valid in gpu functions with compatible contexts.

&whole and &environment are not supported.
")

  (defmacro with-instances
      "
The with-instances macro is used to enable instancing. You specify number number
of instances with the count argument.

An example of its usage is as follows:

    (with-instances 1000
      (map-g #'draw-grass grass-data :tex *grass-texture*))

This behaves kind of like you had written the following..

    (dotimes (x 1000)
      (map-g #'draw-grass grass-data :tex *grass-texture*))

..except MUCH more efficiently as you did not have to submit 1000 draw calls.

Another difference is that, in the pipeline, the variable gl-instance-id will
contain the index of which of the 1000 instances is currently being drawn.
")

  (defmacro g->
      "
Making anonymous pipelines is not yet supported in CEPL. But it will be!

g-> is used to compose gpu-functions together into anonymous pipelines
")

  (defun gpu-functions (name)
    "
This function returns all the signatures of the gpu-functions named 'name'.

The reason there may be many is that functions can be 'overloaded' so you
can have multiple gpu-functions with the same name as long as they can be
uniquely identified by the combination of their name and argument types.
")

  (defun delete-gpu-function (signature)
    "
This function will delete a gpu-function this will mean it can no longer be used
in new pipelines.

This function will only delete one function at a time, so if your gpu-function
is overloaded then you will want to specify the function signature exactly.

See the documentation for #'gpu-functions which will lists all the signatures
for the gpu-functions with a given name.

")

  (defun gpu-function (signature)
    "
This is CEPL's equivalent of Common Lisp's #'function function.

It returns the object that represents the gpu-function with the specified
signature.

Currently there is no reason to use this function. It is only available for the
sake of completeness and future features.
")

  (defmacro bake-uniforms
      "
__WARNING__ EXPRERIMENTAL FEATURE

This allows you to create a new lambda-pipeline from existing pipeline whilst also
fixing the values for certain uniforms.

These values will be baked into the gpu-code so that they will not need to be uploaded
each time the pipeline is mapped over.

For example:

    (def-g-> draw-cube ()
      :vertex (draw-cube-vert g-pnt)
      :fragment (draw-cube-frag :vec2))

    (defun fix-cube-size (size)
      (bake-uniforms #'draw-cube :edge-length (float size)))
"))
