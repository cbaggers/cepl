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
defpipeline is how we define named rendering pipelines in CEPL

                - - - - - - - - - - - - - - - - - - -

-- Defining a Pipeline From GPU Funuctions--

defpipeline can both compose gpu function in rendering pipelines and also
compose multiple pipelines into a multipass pipeline. We will look at both in
turn below.

Rendering in OpenGL is descibed as a pipeline where a buffer-stream of data
(usually describing geometry), and a number of uniforms are used as inputs and
the outputs of the pipelines are written into an FBO.

There are many stages to the pipeline and a full explanation of the GPU pipeline
is beyond the scope of this docstring. However it surfices to say that only
4 stages are fully programmable (and a few more customizable).

defpipeline lets you specify the code (shaders) to run the programmable
parts (stages) of the pipeline.

The available stages are:

- vertex stage
- tessellation - Not yet supported in CEPL
- geometry     - Not yet supported in CEPL
- fragment

To define code that runs on the gpu in CEPL we use gpu functions (gfuncs). Which
are defined with defun-g.

Here is an example pipeline:

    (defun-g vert ((position :vec4) &uniform (i :float))
      (values position (sin i) (cos i)))

    (defun-g frag ((s :float) (c :float))
      (v! s c 0.4 1.0))

    (defpipeline prog-1 ()
	(g-> #'vert #'frag))

Here we define a pipeline #'prog-1 which uses the gfunc #'vert as it's vertex
shader and used the gfunc #'frag as the fragment shader.

It is also possible to specify the name of the stages

    (defpipeline prog-1 ()
	(g-> :vertex #'vert :fragment #'frag))

But this is not neccesary unless you need to distinguish between a tessellation
or geometry stage.


-- Passing values from Stage to Stage --

The return values of the gpu functions that are used as stages are passed as the
input arguments of the next. The exception to this rule is that the first return
value from the vertex stage is taken as used by GL, so only the subsequent
values are passed along.

We can see this in the example above: #'vert returns 3 values but #'frag only
receives 2.

The values from the fragment stage are writen into the current FBO. This may be
the default FBO, in which case you will likely see the result on the screen, or
it may be a FBO of your own.

By default GL only writed the fragment return value to the FBO. For handling
multiple return values please see the docstring for with-fbo-bound.



                - - - - - - - - - - - - - - - - - - -

-- Defining Multipass Pipelines --

Most often we dont want just one pass in our rendering code, we want to take the
output of one pass and feed it straight into another. This can be done perfectly
well with fbos & with-fbo-bound but defpipeline has some syntax that you may
find nicer.

Like before let's take an example


")

  (defmacro map-g
      "

")

;; (defmacro with-fbo-bound
;;       "
;; This is one macro you use when you want to capture the output from a pipeline in
;; an FBO.

;; with-fbo-bound will capture any rendering from any map-g calls inside it body.

;; Also look at the docs for map-g-into and map-g-into* for a full picture of your
;; options

;; -- draw buffers-
;; draw-buffers is an important argument, it allows you to direct the outputs from
;; the fragment-shader of the pipeline into the fbo's color attachments.
;; This means that your pipelines can write into multiple attachments (and thus
;; multiple textures) at once.

;; To use it either pass in:

;;  nil - Which means that the fbo is bound but no attachments will be draw into
;;        (rarely used)

;;  t -  Which means the all attachments will be available to be drawn into
;;       this will happen in order, so the first output from the fragment shader
;;       will draw into the first attachment, the second output to the second
;;       attachment, etc


;; -- with-viewport --
;; If with-viewport is t then with-fbo-bound adds a with-fbo-viewport that uses
;; this fbo to this scope. This means that the current-viewport within this scope
;; will be set to the equivalent of:

;;     (make-viewport dimensions-of-fbo '(0 0))

;; See the docstruct with-fbo-viewport for details on this behavior.

;; One last detail is that you may want to take the dimensions of the viewport from
;; an attachment other than attachment-0. To do this use the 'attachment-for-size
;; argument and give the index of the color-attachment to use.

;; -- with-blending --
;; If with-blending is t then with-fbo-bound adds a with-blending that uses
;; this fbo to this scope.
;; This means that the blending parameters from your fbo will be used while
;; rendering. For the details and version specific behaviours check out
;; the docstring for cepl.blending:with-blending

;; See the with-fbo-viewport for details on the behavior

;; -- target --
;; For target the choices are :framebuffer, :read_framebuffer and
;; :draw_framebuffer.
;; You normally dont need to worry about the target as the last two are only used
;; when you need certain GL read and write operations to happen to different
;; buffers. It remains for those who know they need this but otherwise you can
;; let CEPL handle it.


;; -- unbind --
;; If unbind is set to nil then the fob is not unbound at the end of the scope.
;; Only use this if you know you need it. Most often it is best to let CEPL control
;; that.
;; ")

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
