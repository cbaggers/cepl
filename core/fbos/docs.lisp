(in-package :cepl.fbos)

(docs:define-docs
  (defstruct fbo
    "
A framebuffer object (FBO) is a structure that can be rendered into.

When a FBO is rendered into the data from the pipeline is written into one or
more of the attachments of the FBO

An FBO attachment is a slot where a texture-backed gpu-array can be attached.
When the gpu-array is attached the data from a pipeline rendering into the fbo
is written into that gpu-array.

This is the heart of how multi-pass rendering is done in OpenGL. One pipeline
writes data into textures that are then used as inputs to another pipeline[0].

A FBO can have:
- 0 or more 'color attachments'
- 0 or 1 'depth attachment'
- 0 or 1 'stencil attachment'
- 0 or 1 'depth-stencil attachment'

Let's look at these in more detail:

-- Color Attachments --

Color attachments can only contain gpu-arrays whose element-type can be found in
the *color-renderable-formats* list.

Note that while OpenGL terminology mentions 'color' and 'image' a lot you are
not limitted to only using textures or fbos for pictures. It is perfectly
valid (and incredibly useful) to return data meaning all kinds of things other
than colors.

-- Depth Attachments --

The depth attachment (when used) can only contain a gpu-array whos element-type
can be found in *depth-formats*

Usage Tip:
Even if you don't plan on reading from this depth_attachment, any fbo that will
be rendered to should have a depth attachment.

-- Stencil Attachments (NOT SUPPORTED IN CURRENT CEPL VERSION) --

The stencil attachment (when used) can only contain a gpu-array whos
element-type can be found in *stencil-formats*

-- Depth-Stencil Attachments (NOT SUPPORTED IN CURRENT CEPL VERSION) --

The depth-stencil attachment (when used) can only contain a gpu-array whos
element-type can be found in *depth-stencil-formats*

This attachment is shorthand for 'both depth and stencil'. The gpu-array
attached here becomes both the depth and stencil attachment.


-- Choosing which attachment to render into --

Making these choices is done with the 'with-fbo-bound macro. See it's docstring
for further details

[0] WARNING:

It is possible to bind a texture to an FBO, bind that same texture to a shader,
and then try to sample from it at the same time. You will get undefined results.

This means it may do what you want, the sampler may get old data, the sampler
may get half old and half new data, or it may get garbage data. Any of these are
possible outcomes.

Do Not Do This!
")

  (defun attachment
      "
This function retrieves the attachment named by attachment-name from the
given fbo. The result is a gpu-array

The attachment-name can be one of the following:

 a positive integer - In which case this function returns the
		      nth color-attachments where (= n attachment-name)

 :d - the depth-attachment is returned

 :s - the stencil-attachment is returned [0]

 :ds - the depth-stencil-attachment is returned [0]


You can also setf this function, the value must be a texture-backed gpu-array
with a valid element-type for that attachment.

For color attachments this means the element type must be a member of
the *color-renderable-formats* list

For color attachments this means the element type must be a member of
the *depth-formats* list

For stencil attachments this means the element type must be a member of
the *stencil-formats* list

For depth-stencil attachments this means the element type must be a member of
the *depth-stencil-formats* list

[0] WARNING:
:s & :ds are not supported in the current version of CEPL
")

  (defun fbo-blending-params
      "
This function returns the blending parameters from the given fbo.

Blending parameters tell OpenGL how values written into a gpu-array should be
combined with any values that are already present.

The canonical use for this is implementing transparency.

The details of blending parameters and where they can be used is best covered in
the docstring for the 'blending-params struct.
")

  (defun attachment-blending
      "
This function returns the blending parameters that will be used when rendering
into the specified attachment on the given fbo

Blending parameters tell OpenGL how values written into a gpu-array should be
combined with any values that are already present.

The canonical use for this is implementing transparency.

The details of blending parameters and where they can be used is best covered in
the docstring for the 'blending-params struct.
")

  (defun attachment-viewport
      "
This function takes an fbo and attachment-name and returns a new viewport whos
dimensionsmatch those of the gpu-array connected to the attachment.
")

  (defun check-framebuffer-status
      "
This function asks OpenGL to check the given FBO and ensure that it is
'complete'.

For a full rundown on what it means for a fbo to be complete see:
https://www.opengl.org/wiki/Framebuffer_Object#Framebuffer_Completeness

All fbos made in CEPL using #'make-fbo are checked for completeness before
being returned to the user.
")

  (defun clear
      "
What this function will clear depends on what is passed:

- a fbo - See 'clearing fbos' below
- an attachment - See 'clearing a single attachment' below
- nothing - The 'current fbo' will be cleared

-- Clearing Fbos --

In this case clearing means that all the elements of the attachments will be set
to a certain value. The value that the attachment's gpu-arrays will be set to
varies based on the attachment.

- color attachments: Will be set to the value set in #'gl:clear-color
- depth attachments: Will be set to the value set in #'gl:clear-depth
- stencil attachments: Will be set to the value set in #'gl:clear-stencil

You can also use #'clear-fbo for this task


-- Clearing a single attachment --

Not currently implemented")

  (defun clear-fbo
      "
This function will set the elements of the attachments of the fbo to a
certain value. The value that the attachment's gpu-arrays will be set to
varies based on the attachment.

- color attachments: Will be set to the value set in #'gl:clear-color
- depth attachments: Will be set to the value set in #'gl:clear-depth
- stencil attachments: Will be set to the value set in #'gl:clear-stencil

You can also perform this action by calling #'clear with an fbo")

  (defun clear-attachment
      "
Not currently implemented

This function will clear a single attachment of a fbo.
")

  (defun fbo-p
      "
This function will return t if the given value is an fbo, otherwise it will
return nil
")

  (defun make-fbo
      "
This, like other make-* functions in CEPL has a large variety of valid
arguments. The goal of this apparent complexity is to make exploration from the
repl easy, whilst still allowing absolutely control when it is needed.

Lets look at the behaviour when given different arguments


-- (make-fbo) --
It is not valid is have an fbo with no attachments so this will fail

-- (make-fbo 0)
Make an fbo with one color attachment in attachment slot 0.
CEPL with make a texture with dimensions equal to that of the current viewport
and with the element-type :rgba (which is a sensible default for a color
attachment)

-- (make-fbo 0 1)
Make an fbo with two color attachments 1 in each of attachment slots 0 & 1.
CEPL with make the textures with dimensions equal to that of the current
viewport and with the element-type :rgba (which is a sensible default for a
color attachment)

-- (make-fbo :d)
Make an fbo with one depth attachment.
CEPL with make a texture with dimensions equal to that of the current viewport
and with the element-type :depth-component24 (which is a sensible default for a
depth attachment)

-- (make-fbo 0 1 :d)
Make an fbo with two color attachments and one depth attachment.

-- (make-fbo (list 0 some-gpu-array))
Makes an fbo with one color attachment whos gpu-array is 'some-gpu-array'

--  (make-fbo (list 0 some-texture))
Makes an fbo with one color attachment whos gpu-array is (texref some-texture)

-- (make-fbo '(0 :dimensions (100 100) :element-type :rgba8))
Makes an fbo with one color attachment whos gpu-array is taken from a new
texture created by taking the arguments after 0 and applying them to
#'make-texture

-- Any combination of the above --
")

  (defun make-fbo-from-id
      "
This function will make a CEPL fbo from an existing GL FBO

It does no sanity checking on the inputs and does not check for completeness
used with caution")

  (defun per-attachment-blending-available-p
      "
This function will return t if you are on a version of opengl that supports
setting blending parameters on framebuffer attachments. Otherwise it returns nil

If the result is nil then you will only be able to change blend params on the
first attachment. You can however enable blending on any number of attachments
and they will inherit their params from attachment 0

For more details see cepl.blending
")

  (defmacro with-fbo-bound
      "
This is one macro you use when you want to capture the output from a pipeline in
an FBO.

with-fbo-bound will capture any rendering from any map-g calls inside it body.

Also look at the docs for map-g-into and map-g-into* for a full picture of your
options

-- draw buffers-
draw-buffers is an important argument, it allows you to direct the outputs from
the fragment-shader of the pipeline into the fbo's color attachments.
This means that your pipelines can write into multiple attachments (and thus
multiple textures) at once.

To use it either pass in:

 nil - Which means that the fbo is bound but no attachments will be draw into
       (rarely used)

 t -  Which means the all attachments will be available to be drawn into
      this will happen in order, so the first output from the fragment shader
      will draw into the first attachment, the second output to the second
      attachment, etc


-- with-viewport --
If with-viewport is t then with-fbo-bound adds a with-fbo-viewport that uses
this fbo to this scope. This means that the current-viewport within this scope
will be set to the equivalent of:

    (make-viewport dimensions-of-fbo '(0 0))

See the docstruct with-fbo-viewport for details on this behavior.

One last detail is that you may want to take the dimensions of the viewport from
an attachment other than attachment-0. To do this use the 'attachment-for-size
argument and give the index of the color-attachment to use.

-- with-blending --
If with-blending is t then with-fbo-bound adds a with-blending that uses
this fbo to this scope.
This means that the blending parameters from your fbo will be used while
rendering. For the details and version specific behaviours check out
the docstring for cepl.blending:with-blending

See the with-fbo-viewport for details on the behavior

-- target --
For target the choices are :framebuffer, :read_framebuffer and
:draw_framebuffer.
You normally dont need to worry about the target as the last two are only used
when you need certain GL read and write operations to happen to different
buffers. It remains for those who know they need this but otherwise you can
let CEPL handle it.


-- unbind --
If unbind is set to nil then the fob is not unbound at the end of the scope.
Only use this if you know you need it. Most often it is best to let CEPL control
that.
"))
