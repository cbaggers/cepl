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
They first must be composed into a pipeline using defpipeline.

When a gfunc is composed into a pipeline then that function takes on the role of
one of the 'shader stages' of the pipeline. For a proper breakdown of pipelines
see the docstring for defpipeline.


Let's see a simple example of a gpu function we can then break down

    ;;       {0}          {3}          {1}         {2}
    (defun-g vert ((vert my-struct) &uniform (loop :float))
      (values (v! (+ (my-struct-pos vert) ;; {4}
		     (v! (sin loop) (cos loop) 0))
		  1.0)
	      (my-struct-col vert))) ;;{5}


{0} So like the normal defun we specify a name first, and the arguments as a
    list straight afterwards

{1} The &uniform lambda keyword says that arguments following it are 'uniform
    arguments'. A uniform is an argument which has the same value for the entire
    stage



- Type: All the function's aruments must have their types declared

- No &optional or &key: Instead we have &uniform and &context


")

  (defmacro defpipeline
      "

")

  (defmacro map-g
      "

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

An example of it's usage is as follows:

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
g-> is used to compose gpu-functions or pipelines together.

Currently it can only be used with defpipeline but in future using this outside
of a defpipeline will create an anonomous gpu function
"))
