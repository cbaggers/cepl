# VAO

## :vertex-array-binding

data returns a single value, the name of the vertex array object
currently bound to the context. If no vertex array object is bound to
the context, 0 is returned. The initial value is 0. See
glBindVertexArray.

--------------------------------------------------------------------
# Buffer Objects

## :array-buffer-binding

data returns a single value, the name of the buffer object currently
bound to the target :array-buffer. If no buffer object is bound to
this target, 0 is returned. The initial value is 0. See glBindBuffer.

## :uniform-buffer-binding

When used with non-indexed variants of glGet (such as glGetIntegerv),
data returns a single value, the name of the buffer object currently
bound to the target :uniform-buffer. If no buffer object is bound to
this target, 0 is returned. When used with indexed variants of glGet
(such as glGetIntegeri-v), data returns a single value, the name of
the buffer object bound to the indexed uniform buffer binding
point. The initial value is 0 for all targets. See glBindBuffer,
glBindBufferBase, and glBindBufferRange.

## :transform-feedback-buffer-binding

When used with non-indexed variants of glGet (such as glGetIntegerv),
data returns a single value, the name of the buffer object currently
bound to the target :transform-feedback-buffer. If no buffer object
is bound to this target, 0 is returned. When used with indexed
variants of glGet (such as glGetIntegeri-v), data returns a single
value, the name of the buffer object bound to the indexed transform
feedback attribute stream. The initial value is 0 for all targets. See
glBindBuffer, glBindBufferBase, and glBindBufferRange.

## :shader-storage-buffer-binding

When used with non-indexed variants of glGet (such as glGetIntegerv),
data returns a single value, the name of the buffer object currently
bound to the target :shader-storage-buffer. If no buffer object is
bound to this target, 0 is returned. When used with indexed variants
of glGet (such as glGetIntegeri-v), data returns a single value, the
name of the buffer object bound to the indexed shader storage buffer
binding points. The initial value is 0 for all targets. See
glBindBuffer, glBindBufferBase, and glBindBufferRange.

## :pixel-unpack-buffer-binding

data returns a single value, the name of the buffer object currently
bound to the target :pixel-unpack-buffer. If no buffer object is
bound to this target, 0 is returned. The initial value is 0. See
glBindBuffer.

## :pixel-pack-buffer-binding

data returns a single value, the name of the buffer object currently
bound to the target :pixel-pack-buffer. If no buffer object is bound
to this target, 0 is returned. The initial value is 0. See
glBindBuffer.


## :element-array-buffer-binding

data returns a single value, the name of the buffer object currently
bound to the target :element-array-buffer. If no buffer object is
bound to this target, 0 is returned. The initial value is 0. See
glBindBuffer.

## :texture-binding-buffer

data returns a single value, the name of the buffer object currently
bound to the :texture-buffer buffer binding point. The initial value
is 0. See glBindBuffer.

## :dispatch-indirect-buffer-binding

data returns a single value, the name of the buffer object currently
bound to the target :dispatch-indirect-buffer. If no buffer object
is bound to this target, 0 is returned. The initial value is 0. See
glBindBuffer.

## :shader-storage-buffer-start

When used with indexed variants of glGet (such as glGetInteger64i-v),
data returns a single value, the start offset of the binding range for
each indexed shader storage buffer binding. The initial value is 0 for
all bindings. See glBindBufferRange.

## :shader-storage-buffer-size

When used with indexed variants of glGet (such as glGetInteger64i-v),
data returns a single value, the size of the binding range for each
indexed shader storage buffer binding. The initial value is 0 for all
bindings. See glBindBufferRange.

## :transform-feedback-buffer-start

When used with indexed variants of glGet (such as glGetInteger64i-v),
data returns a single value, the start offset of the binding range for
each transform feedback attribute stream. The initial value is 0 for
all streams. See glBindBufferRange.

## :transform-feedback-buffer-size

When used with indexed variants of glGet (such as glGetInteger64i-v),
data returns a single value, the size of the binding range for each
transform feedback attribute stream. The initial value is 0 for all
streams. See glBindBufferRange.

## :uniform-buffer-size

When used with indexed variants of glGet (such as glGetInteger64i-v),
data returns a single value, the size of the binding range for each
indexed uniform buffer binding. The initial value is 0 for all
bindings. See glBindBufferRange.

## :uniform-buffer-start

When used with indexed variants of glGet (such as glGetInteger64i-v),
data returns a single value, the start offset of the binding range for
each indexed uniform buffer binding. The initial value is 0 for all
bindings. See glBindBufferRange.

--------------------------------------------------------------------

# Samplers

## :sampler-binding

data returns a single value, the name of the sampler object currently
bound to the active texture unit. The initial value is 0. See
glBindSampler.

--------------------------------------------------------------------

#

## :renderbuffer-binding

data returns a single value, the name of the renderbuffer object
currently bound to the target :renderbuffer. If no renderbuffer
object is bound to this target, 0 is returned. The initial value is
0. See glBindRenderbuffer.

## :program-pipeline-binding

data a single value, the name of the currently bound program pipeline
object, or zero if no program pipeline object is bound. See
glBindProgramPipeline.


## :read-framebuffer-binding

data returns one value, the name of the framebuffer object currently
bound to the :read-framebuffer target. If the default framebuffer is
bound, this value will be zero. The initial value is zero. See
glBindFramebuffer.

## :draw-framebuffer-binding

data returns one value, the name of the framebuffer object currently
bound to the :draw-framebuffer target. If the default framebuffer is
bound, this value will be zero. The initial value is zero. See
glBindFramebuffer.

--------------------------------------------------------------------
# Texture Bindings



## :texture-binding-rectangle

data returns a single value, the name of the texture currently bound
to the target :texture-rectangle. The initial value is 0. See
glBindTexture.

## :texture-binding-cube-map

data returns a single value, the name of the texture currently bound
to the target :texture-cube-map. The initial value is 0. See
glBindTexture.

## :texture-binding-buffer

data returns a single value, the name of the texture currently bound
to the target :texture-buffer. The initial value is 0. See
glBindTexture.

## :texture-binding-3d

data returns a single value, the name of the texture currently bound
to the target :texture-3d. The initial value is 0. See
glBindTexture.

## :texture-binding-2d-multisample-array

data returns a single value, the name of the texture currently bound
to the target :texture-2d-multisample-array. The initial value is
0. See glBindTexture.

## :texture-binding-2d-multisample

data returns a single value, the name of the texture currently bound
to the target :texture-2d-multisample. The initial value is 0. See
glBindTexture.

## :texture-binding-2d-array

data returns a single value, the name of the texture currently bound
to the target :texture-2d-array. The initial value is 0. See
glBindTexture.

## :texture-binding-2d

data returns a single value, the name of the texture currently bound
to the target :texture-2d. The initial value is 0. See
glBindTexture.

## :texture-binding-1d-array

data returns a single value, the name of the texture currently bound
to the target :texture-1d-array. The initial value is 0. See
glBindTexture.

## :texture-binding-1d

data returns a single value, the name of the texture currently bound
to the target :texture-1d. The initial value is 0. See
glBindTexture.

--------------------------------------------------------------------



## :vertex-binding-stride

Accepted by the indexed forms. data returns a single integer value
representing the byte offset between the start of each element in the
bound buffer's data store for vertex attribute bound to index.

## :vertex-binding-offset

Accepted by the indexed forms. data returns a single integer value
representing the byte offset of the first element in the bound
buffer's data store for vertex attribute bound to index.

## :vertex-binding-divisor

Accepted by the indexed forms. data returns a single integer value
representing the instance step divisor of the first element in the
bound buffer's data store for vertex attribute bound to index.




## :active-texture

data returns a single value indicating the active multitexture
unit. The initial value is :texture0. See glActiveTexture.

## :aliased-line-width-range

data returns a pair of values indicating the range of widths supported
for aliased lines. See glLineWidth.

## :blend

data returns a single boolean value indicating whether blending is
enabled. The initial value is :false. See glBlendFunc.

## :blend-color

data returns four values, the red, green, blue, and alpha values which
are the components of the blend color. See glBlendColor.

## :blend-dst-alpha

data returns one value, the symbolic constant identifying the alpha
destination blend function. The initial value is :zero. See
glBlendFunc and glBlendFuncSeparate.

## :blend-dst-rgb

data returns one value, the symbolic constant identifying the RGB
destination blend function. The initial value is :zero. See
glBlendFunc and glBlendFuncSeparate.

## :blend-equation-rgb

data returns one value, a symbolic constant indicating whether the RGB
blend equation is :func-add, :func-subtract,
:func-reverse-subtract, :min or :max. See
glBlendEquationSeparate.

## :blend-equation-alpha

data returns one value, a symbolic constant indicating whether the
Alpha blend equation is :func-add, :func-subtract,
:func-reverse-subtract, :min or :max. See
glBlendEquationSeparate.

## :blend-src-alpha

data returns one value, the symbolic constant identifying the alpha
source blend function. The initial value is :one. See glBlendFunc
and glBlendFuncSeparate.

## :blend-src-rgb

data returns one value, the symbolic constant identifying the RGB
source blend function. The initial value is :one. See glBlendFunc
and glBlendFuncSeparate.

## :color-clear-value

data returns four values: the red, green, blue, and alpha values used
to clear the color buffers. Integer values, if requested, are linearly
mapped from the internal floating-point representation such that 1.0
returns the most positive representable integer value, and -1.0

returns the most negative representable integer value. The initial
value is (0, 0, 0, 0). See glClearColor.

## :color-logic-op

data returns a single boolean value indicating whether a fragment's
RGBA color values are merged into the framebuffer using a logical
operation. The initial value is :false. See glLogicOp.

## :color-writemask

data returns four boolean values: the red, green, blue, and alpha
write enables for the color buffers. The initial value is (:true,
:true, :true, :true). See glColorMask.

## :compressed-texture-formats

data returns a list of symbolic constants of length
:num-compressed-texture-formats indicating which compressed texture
formats are available. See glCompressedTexImage2D.



## :debug-group-stack-depth

data returns a single value, the current depth of the debug message
group stack.

## :context-flags

data returns one value, the flags with which the context was created
(such as debugging functionality).

## :cull-face

data returns a single boolean value indicating whether polygon culling
is enabled. The initial value is :false. See glCullFace.

## :current-program

data returns one value, the name of the program object that is
currently active, or 0 if no program object is active. See
glUseProgram.

## :depth-clear-value

data returns one value, the value that is used to clear the depth
buffer. Integer values, if requested, are linearly mapped from the
internal floating-point representation such that 1.0 returns the most
positive representable integer value, and -1.0

returns the most negative representable integer value. The initial
value is 1. See glClearDepth.

## :depth-func

data returns one value, the symbolic constant that indicates the depth
comparison function. The initial value is :less. See glDepthFunc.

## :depth-range

data returns two values: the near and far mapping limits for the depth
buffer. Integer values, if requested, are linearly mapped from the
internal floating-point representation such that 1.0 returns the most
positive representable integer value, and -1.0

returns the most negative representable integer value. The initial
value is (0, 1). See glDepthRange.

## :depth-test

data returns a single boolean value indicating whether depth testing
of fragments is enabled. The initial value is :false. See
glDepthFunc and glDepthRange.

## :depth-writemask

data returns a single boolean value indicating if the depth buffer is
enabled for writing. The initial value is :true. See glDepthMask.

## :dither

data returns a single boolean value indicating whether dithering of
fragment colors and indices is enabled. The initial value is :true.

## :doublebuffer

data returns a single boolean value indicating whether double
buffering is supported.

## :draw-buffer

data returns one value, a symbolic constant indicating which buffers
are being drawn to. See glDrawBuffer. The initial value is :back if
there are back buffers, otherwise it is :front.

## :draw-buffer i

data returns one value, a symbolic constant indicating which buffers
are being drawn to by the corresponding output color. See
glDrawBuffers. The initial value of :draw-buffer0 is :back if
there are back buffers, otherwise it is :front. The initial values
of draw buffers for all other output colors is :none.

## :fragment-shader-derivative-hint

data returns one value, a symbolic constant indicating the mode of the
derivative accuracy hint for fragment shaders. The initial value is
:dont-care. See glHint.

## :implementation-color-read-format

data returns a single GLenum value indicating the implementation's
preferred pixel data format. See glReadPixels.

## :implementation-color-read-type

data returns a single GLenum value indicating the implementation's
preferred pixel data type. See glReadPixels.

## :line-smooth

data returns a single boolean value indicating whether antialiasing of
lines is enabled. The initial value is :false. See glLineWidth.

## :line-smooth-hint

data returns one value, a symbolic constant indicating the mode of the
line antialiasing hint. The initial value is :dont-care. See glHint.

## :line-width

data returns one value, the line width as specified with
glLineWidth. The initial value is 1.

## :layer-provoking-vertex

data returns one value, the implementation dependent specifc vertex of
a primitive that is used to select the rendering layer. If the value
returned is equivalent to :provoking-vertex, then the vertex
selection follows the convention specified by glProvokingVertex. If
the value returned is equivalent to :first-vertex-convention, then
the selection is always taken from the first vertex in the
primitive. If the value returned is equivalent to
:last-vertex-convention, then the selection is always taken from the
last vertex in the primitive. If the value returned is equivalent to
:undefined-vertex, then the selection is not guaranteed to be taken
from any specific vertex in the primitive.

## :logic-op-mode

data returns one value, a symbolic constant indicating the selected
logic operation mode. The initial value is :copy. See glLogicOp.

## :major-version

data returns one value, the major version number of the OpenGL API
supported by the current context.

## :max-vertex-attrib-bindings

data returns a single integer value containing the maximum number of
vertex buffers that may be bound.

## :max-uniform-buffer-bindings

data returns one value, the maximum number of uniform buffer binding
points on the context, which must be at least 36.

## :max-shader-storage-buffer-bindings

data returns one value, the maximum number of shader storage buffer
binding points on the context, which must be at least 8.

## :max-compute-shader-storage-blocks

data returns one value, the maximum number of active shader storage
blocks that may be accessed by a compute shader.

## :max-combined-shader-storage-blocks

data returns one value, the maximum total number of active shader
storage blocks that may be accessed by all active shaders.

## :max-compute-uniform-blocks

data returns one value, the maximum number of uniform blocks per
compute shader. The value must be at least 14. See
glUniformBlockBinding.

## :max-compute-texture-image-units

data returns one value, the maximum supported texture image units that
can be used to access texture maps from the compute shader. The value
may be at least 16. See glActiveTexture.

## :max-compute-uniform-components

data returns one value, the maximum number of individual
floating-point, integer, or boolean values that can be held in uniform
variable storage for a compute shader. The value must be at least
1024. See glUniform.

## :max-compute-atomic-counters

data returns a single value, the maximum number of atomic counters
available to compute shaders.

## :max-compute-atomic-counter-buffers

data returns a single value, the maximum number of atomic counter
buffers that may be accessed by a compute shader.

## :max-combined-compute-uniform-components

data returns one value, the number of words for compute shader uniform
variables in all uniform blocks (including default). The value must be
at least 1. See glUniform.

## :max-compute-work-group-invocations

data returns one value, the number of invocations in a single local
work group (i.e., the product of the three dimensions) that may be
dispatched to a compute shader.

## :max-compute-work-group-count

Accepted by the indexed versions of glGet. data the maximum number of
work groups that may be dispatched to a compute shader. Indices 0, 1,
and 2 correspond to the X, Y and Z dimensions, respectively.

## :max-compute-work-group-size

Accepted by the indexed versions of glGet. data the maximum size of a
work groups that may be used during compilation of a compute
shader. Indices 0, 1, and 2 correspond to the X, Y and Z dimensions,
respectively.

## :max-debug-group-stack-depth

data returns a single value, the maximum depth of the debug message
group stack.

## :max-3d-texture-size

data returns one value, a rough estimate of the largest 3D texture
that the GL can handle. The value must be at least 64. Use
:proxy-texture-3d to determine if a texture is too large. See
glTexImage3D.

## :max-array-texture-layers

data returns one value. The value indicates the maximum number of
layers allowed in an array texture, and must be at least 256. See
glTexImage2D.

## :max-clip-distances

data returns one value, the maximum number of application-defined
clipping distances. The value must be at least 8.

## :max-color-texture-samples

data returns one value, the maximum number of samples in a color
multisample texture.

## :max-combined-atomic-counters

data returns a single value, the maximum number of atomic counters
available to all active shaders.

## :max-combined-fragment-uniform-components

data returns one value, the number of words for fragment shader
uniform variables in all uniform blocks (including default). The value
must be at least 1. See glUniform.

## :max-combined-geometry-uniform-components

data returns one value, the number of words for geometry shader
uniform variables in all uniform blocks (including default). The value
must be at least 1. See glUniform.

## :max-combined-texture-image-units

data returns one value, the maximum supported texture image units that
can be used to access texture maps from the vertex shader and the
fragment processor combined. If both the vertex shader and the
fragment processing stage access the same texture image unit, then
that counts as using two texture image units against this limit. The
value must be at least 48. See glActiveTexture.

## :max-combined-uniform-blocks

data returns one value, the maximum number of uniform blocks per
program. The value must be at least 70. See glUniformBlockBinding.

## :max-combined-vertex-uniform-components

data returns one value, the number of words for vertex shader uniform
variables in all uniform blocks (including default). The value must be
at least 1. See glUniform.

## :max-cube-map-texture-size

data returns one value. The value gives a rough estimate of the
largest cube-map texture that the GL can handle. The value must be at
least 1024. Use :proxy-texture-cube-map to determine if a texture is
too large. See glTexImage2D.

## :max-depth-texture-samples

data returns one value, the maximum number of samples in a multisample
depth or depth-stencil texture.

## :max-draw-buffers

data returns one value, the maximum number of simultaneous outputs
that may be written in a fragment shader. The value must be at least
8. See glDrawBuffers.

## :max-dual-source-draw-buffers

data returns one value, the maximum number of active draw buffers when
using dual-source blending. The value must be at least 1. See
glBlendFunc and glBlendFuncSeparate.

## :max-elements-indices

data returns one value, the recommended maximum number of vertex array
indices. See glDrawRangeElements.

## :max-elements-vertices

data returns one value, the recommended maximum number of vertex array
vertices. See glDrawRangeElements.

## :max-fragment-atomic-counters

data returns a single value, the maximum number of atomic counters
available to fragment shaders.

## :max-fragment-shader-storage-blocks

data returns one value, the maximum number of active shader storage
blocks that may be accessed by a fragment shader.

## :max-fragment-input-components

data returns one value, the maximum number of components of the inputs
read by the fragment shader, which must be at least 128.

## :max-fragment-uniform-components

data returns one value, the maximum number of individual
floating-point, integer, or boolean values that can be held in uniform
variable storage for a fragment shader. The value must be at least
1024. See glUniform.

## :max-fragment-uniform-vectors

data returns one value, the maximum number of individual 4-vectors of
floating-point, integer, or boolean values that can be held in uniform
variable storage for a fragment shader. The value is equal to the
value of :max-fragment-uniform-components divided by 4 and must be
at least 256. See glUniform.

## :max-fragment-uniform-blocks

data returns one value, the maximum number of uniform blocks per
fragment shader. The value must be at least 12. See
glUniformBlockBinding.

## :max-framebuffer-width

data returns one value, the maximum width for a framebuffer that has
no attachments, which must be at least 16384. See
glFramebufferParameter.

## :max-framebuffer-height

data returns one value, the maximum height for a framebuffer that has
no attachments, which must be at least 16384. See
glFramebufferParameter.

## :max-framebuffer-layers

data returns one value, the maximum number of layers for a framebuffer
that has no attachments, which must be at least 2048. See
glFramebufferParameter.

## :max-framebuffer-samples

data returns one value, the maximum samples in a framebuffer that has
no attachments, which must be at least 4. See glFramebufferParameter.

## :max-geometry-atomic-counters

data returns a single value, the maximum number of atomic counters
available to geometry shaders.

## :max-geometry-shader-storage-blocks

data returns one value, the maximum number of active shader storage
blocks that may be accessed by a geometry shader.

## :max-geometry-input-components

data returns one value, the maximum number of components of inputs
read by a geometry shader, which must be at least 64.

## :max-geometry-output-components

data returns one value, the maximum number of components of outputs
written by a geometry shader, which must be at least 128.

## :max-geometry-texture-image-units

data returns one value, the maximum supported texture image units that
can be used to access texture maps from the geometry shader. The value
must be at least 16. See glActiveTexture.

## :max-geometry-uniform-blocks

data returns one value, the maximum number of uniform blocks per
geometry shader. The value must be at least 12. See
glUniformBlockBinding.

## :max-geometry-uniform-components

data returns one value, the maximum number of individual
floating-point, integer, or boolean values that can be held in uniform
variable storage for a geometry shader. The value must be at least
1024. See glUniform.

## :max-integer-samples

data returns one value, the maximum number of samples supported in
integer format multisample buffers.

## :min-map-buffer-alignment

data returns one value, the minimum alignment in basic machine units
of pointers returned fromglMapBuffer and glMapBufferRange. This value
must be a power of two and must be at least 64.

## :max-label-length

data returns one value, the maximum length of a label that may be
assigned to an object. See glObjectLabel and glObjectPtrLabel.

## :max-program-texel-offset

data returns one value, the maximum texel offset allowed in a texture
lookup, which must be at least 7.

## :min-program-texel-offset

data returns one value, the minimum texel offset allowed in a texture
lookup, which must be at most -8.

## :max-rectangle-texture-size

data returns one value. The value gives a rough estimate of the
largest rectangular texture that the GL can handle. The value must be
at least 1024. Use :proxy-texture-rectangle to determine if a
texture is too large. See glTexImage2D.

## :max-renderbuffer-size

data returns one value. The value indicates the maximum supported size
for renderbuffers. See glFramebufferRenderbuffer.

## :max-sample-mask-words

data returns one value, the maximum number of sample mask words.

## :max-server-wait-timeout

data returns one value, the maximum glWaitSync timeout interval.

## :max-tess-control-atomic-counters

data returns a single value, the maximum number of atomic counters
available to tessellation control shaders.

## :max-tess-evaluation-atomic-counters

data returns a single value, the maximum number of atomic counters
available to tessellation evaluation shaders.

## :max-tess-control-shader-storage-blocks

data returns one value, the maximum number of active shader storage
blocks that may be accessed by a tessellation control shader.

## :max-tess-evaluation-shader-storage-blocks

data returns one value, the maximum number of active shader storage
blocks that may be accessed by a tessellation evaluation shader.

## :max-texture-buffer-size

data returns one value. The value gives the maximum number of texels
allowed in the texel array of a texture buffer object. Value must be
at least 65536.

## :max-texture-image-units

data returns one value, the maximum supported texture image units that
can be used to access texture maps from the fragment shader. The value
must be at least 16. See glActiveTexture.

## :max-texture-lod-bias

data returns one value, the maximum, absolute value of the texture
level-of-detail bias. The value must be at least 2.0.

## :max-texture-size

data returns one value. The value gives a rough estimate of the
largest texture that the GL can handle. The value must be at least
1024. Use a proxy texture target such as :proxy-texture-1d or
:proxy-texture-2d to determine if a texture is too large. See
glTexImage1D and glTexImage2D.

## :max-uniform-block-size

data returns one value, the maximum size in basic machine units of a
uniform block, which must be at least 16384.

## :max-uniform-locations

data returns one value, the maximum number of explicitly assignable
uniform locations, which must be at least 1024.

## :max-varying-components

data returns one value, the number components for varying variables,
which must be at least 60.

## :max-varying-vectors

data returns one value, the number 4-vectors for varying variables,
which is equal to the value of :max-varying-components and must be
at least 15.

## :max-varying-floats

data returns one value, the maximum number of interpolators available
for processing varying variables used by vertex and fragment
shaders. This value represents the number of individual floating-point
values that can be interpolated; varying variables declared as
vectors, matrices, and arrays will all consume multiple
interpolators. The value must be at least 32.

## :max-vertex-atomic-counters

data returns a single value, the maximum number of atomic counters
available to vertex shaders.

## :max-vertex-attribs

data returns one value, the maximum number of 4-component generic
vertex attributes accessible to a vertex shader. The value must be at
least 16. See glVertexAttrib.

## :max-vertex-shader-storage-blocks

data returns one value, the maximum number of active shader storage
blocks that may be accessed by a vertex shader.

## :max-vertex-texture-image-units

data returns one value, the maximum supported texture image units that
can be used to access texture maps from the vertex shader. The value
may be at least 16. See glActiveTexture.

## :max-vertex-uniform-components

data returns one value, the maximum number of individual
floating-point, integer, or boolean values that can be held in uniform
variable storage for a vertex shader. The value must be at least
1024. See glUniform.

## :max-vertex-uniform-vectors

data returns one value, the maximum number of 4-vectors that may be
held in uniform variable storage for the vertex shader. The value of
:max-vertex-uniform-vectors is equal to the value of
:max-vertex-uniform-components and must be at least 256.

## :max-vertex-output-components

data returns one value, the maximum number of components of output
written by a vertex shader, which must be at least 64.

## :max-vertex-uniform-blocks

data returns one value, the maximum number of uniform blocks per
vertex shader. The value must be at least 12. See
glUniformBlockBinding.

## :max-viewport-dims

data returns two values: the maximum supported width and height of the
viewport. These must be at least as large as the visible dimensions of
the display being rendered to. See glViewport.

## :max-viewports

data returns one value, the maximum number of simultaneous viewports
that are supported. The value must be at least 16. See
glViewportIndexed.

## :minor-version

data returns one value, the minor version number of the OpenGL API
supported by the current context.

## :num-compressed-texture-formats

data returns a single integer value indicating the number of available
compressed texture formats. The minimum value is 4. See
glCompressedTexImage2D.

## :num-extensions

data returns one value, the number of extensions supported by the GL
implementation for the current context. See glGetString.

## :num-program-binary-formats

data returns one value, the number of program binary formats supported
by the implementation.

## :num-shader-binary-formats

data returns one value, the number of binary shader formats supported
by the implementation. If this value is greater than zero, then the
implementation supports loading binary shaders. If it is zero, then
the loading of binary shaders by the implementation is not supported.

## :pack-alignment

data returns one value, the byte alignment used for writing pixel data
to memory. The initial value is 4. See glPixelStore.

## :pack-image-height

data returns one value, the image height used for writing pixel data
to memory. The initial value is 0. See glPixelStore.

## :pack-lsb-first

data returns a single boolean value indicating whether single-bit
pixels being written to memory are written first to the least
significant bit of each unsigned byte. The initial value is
:false. See glPixelStore.

## :pack-row-length

data returns one value, the row length used for writing pixel data to
memory. The initial value is 0. See glPixelStore.

## :pack-skip-images

data returns one value, the number of pixel images skipped before the
first pixel is written into memory. The initial value is 0. See
glPixelStore.

## :pack-skip-pixels

data returns one value, the number of pixel locations skipped before
the first pixel is written into memory. The initial value is 0. See
glPixelStore.

## :pack-skip-rows

data returns one value, the number of rows of pixel locations skipped
before the first pixel is written into memory. The initial value is
0. See glPixelStore.

## :pack-swap-bytes

data returns a single boolean value indicating whether the bytes of
two-byte and four-byte pixel indices and components are swapped before
being written to memory. The initial value is :false. See
glPixelStore.

## :point-fade-threshold-size

data returns one value, the point size threshold for determining the
point size. See glPointParameter.

## :primitive-restart-index

data returns one value, the current primitive restart index. The
initial value is 0. See glPrimitiveRestartIndex.

## :program-binary-formats

data an array of :num-program-binary-formats values, indicating the
proram binary formats supported by the implementation.

## :program-point-size

data returns a single boolean value indicating whether vertex program
point size mode is enabled. If enabled, then the point size is taken
from the shader built-in gl-PointSize. If disabled, then the point
size is taken from the point state as specified by glPointSize. The
initial value is :false.

## :provoking-vertex

data returns one value, the currently selected provoking vertex
convention. The initial value is :last-vertex-convention. See
glProvokingVertex.

## :point-size

data returns one value, the point size as specified by
glPointSize. The initial value is 1.

## :point-size-granularity

data returns one value, the size difference between adjacent supported
sizes for antialiased points. See glPointSize.

## :point-size-range

data returns two values: the smallest and largest supported sizes for
antialiased points. The smallest size must be at most 1, and the
largest size must be at least 1. See glPointSize.

## :polygon-offset-factor

data returns one value, the scaling factor used to determine the
variable offset that is added to the depth value of each fragment
generated when a polygon is rasterized. The initial value is 0. See
glPolygonOffset.

## :polygon-offset-units

data returns one value. This value is multiplied by an
implementation-specific value and then added to the depth value of
each fragment generated when a polygon is rasterized. The initial
value is 0. See glPolygonOffset.

## :polygon-offset-fill

data returns a single boolean value indicating whether polygon offset
is enabled for polygons in fill mode. The initial value is
:false. See glPolygonOffset.

## :polygon-offset-line

data returns a single boolean value indicating whether polygon offset
is enabled for polygons in line mode. The initial value is
:false. See glPolygonOffset.

## :polygon-offset-point

data returns a single boolean value indicating whether polygon offset
is enabled for polygons in point mode. The initial value is
:false. See glPolygonOffset.

## :polygon-smooth

data returns a single boolean value indicating whether antialiasing of
polygons is enabled. The initial value is :false. See glPolygonMode.

## :polygon-smooth-hint

data returns one value, a symbolic constant indicating the mode of the
polygon antialiasing hint. The initial value is :dont-care. See
glHint.

## :read-buffer

data returns one value, a symbolic constant indicating which color
buffer is selected for reading. The initial value is :back if there
is a back buffer, otherwise it is :front. See glReadPixels.

## :sample-buffers

data returns a single integer value indicating the number of sample
buffers associated with the framebuffer. See glSampleCoverage.

## :sample-coverage-value

data returns a single positive floating-point value indicating the
current sample coverage value. See glSampleCoverage.

## :sample-coverage-invert

data returns a single boolean value indicating if the temporary
coverage value should be inverted. See glSampleCoverage.

## :samples

data returns a single integer value indicating the coverage mask
size. See glSampleCoverage.

## :scissor-box

data returns four values: the x and y window coordinates of the
scissor box, followed by its width and height. Initially the x and y

window coordinates are both 0 and the width and height are set to the
size of the window. See glScissor.

## :scissor-test

data returns a single boolean value indicating whether scissoring is
enabled. The initial value is :false. See glScissor.

## :shader-compiler

data returns a single boolean value indicating whether an online
shader compiler is present in the implementation. All desktop OpenGL
implementations must support online shader compilations, and therefore
the value of :shader-compiler will always be :true.

## :shader-storage-buffer-offset-alignment

data returns a single value, the minimum required alignment for shader
storage buffer sizes and offset. The initial value is 1. See
glShaderStorageBlockBinding.

## :smooth-line-width-range

data returns a pair of values indicating the range of widths supported
for smooth (antialiased) lines. See glLineWidth.

## :smooth-line-width-granularity

data returns a single value indicating the level of quantization
applied to smooth line width parameters.

## :stencil-back-fail

data returns one value, a symbolic constant indicating what action is
taken for back-facing polygons when the stencil test fails. The
initial value is :keep. See glStencilOpSeparate.

## :stencil-back-func

data returns one value, a symbolic constant indicating what function
is used for back-facing polygons to compare the stencil reference
value with the stencil buffer value. The initial value is
:always. See glStencilFuncSeparate.

## :stencil-back-pass-depth-fail

data returns one value, a symbolic constant indicating what action is
taken for back-facing polygons when the stencil test passes, but the
depth test fails. The initial value is :keep. See
glStencilOpSeparate.

## :stencil-back-pass-depth-pass

data returns one value, a symbolic constant indicating what action is
taken for back-facing polygons when the stencil test passes and the
depth test passes. The initial value is :keep. See
glStencilOpSeparate.

## :stencil-back-ref

data returns one value, the reference value that is compared with the
contents of the stencil buffer for back-facing polygons. The initial
value is 0. See glStencilFuncSeparate.

## :stencil-back-value-mask

data returns one value, the mask that is used for back-facing polygons
to mask both the stencil reference value and the stencil buffer value
before they are compared. The initial value is all 1's. See
glStencilFuncSeparate.

## :stencil-back-writemask

data returns one value, the mask that controls writing of the stencil
bitplanes for back-facing polygons. The initial value is all 1's. See
glStencilMaskSeparate.

## :stencil-clear-value

data returns one value, the index to which the stencil bitplanes are
cleared. The initial value is 0. See glClearStencil.

## :stencil-fail

data returns one value, a symbolic constant indicating what action is
taken when the stencil test fails. The initial value is :keep. See
glStencilOp. This stencil state only affects non-polygons and
front-facing polygons. Back-facing polygons use separate stencil
state. See glStencilOpSeparate.

## :stencil-func

data returns one value, a symbolic constant indicating what function
is used to compare the stencil reference value with the stencil buffer
value. The initial value is :always. See glStencilFunc. This stencil
state only affects non-polygons and front-facing polygons. Back-facing
polygons use separate stencil state. See glStencilFuncSeparate.

## :stencil-pass-depth-fail

data returns one value, a symbolic constant indicating what action is
taken when the stencil test passes, but the depth test fails. The
initial value is :keep. See glStencilOp. This stencil state only
affects non-polygons and front-facing polygons. Back-facing polygons
use separate stencil state. See glStencilOpSeparate.

## :stencil-pass-depth-pass

data returns one value, a symbolic constant indicating what action is
taken when the stencil test passes and the depth test passes. The
initial value is :keep. See glStencilOp. This stencil state only
affects non-polygons and front-facing polygons. Back-facing polygons
use separate stencil state. See glStencilOpSeparate.

## :stencil-ref

data returns one value, the reference value that is compared with the
contents of the stencil buffer. The initial value is 0. See
glStencilFunc. This stencil state only affects non-polygons and
front-facing polygons. Back-facing polygons use separate stencil
state. See glStencilFuncSeparate.

## :stencil-test

data returns a single boolean value indicating whether stencil testing
of fragments is enabled. The initial value is :false. See
glStencilFunc and glStencilOp.

## :stencil-value-mask

data returns one value, the mask that is used to mask both the stencil
reference value and the stencil buffer value before they are
compared. The initial value is all 1's. See glStencilFunc. This
stencil state only affects non-polygons and front-facing
polygons. Back-facing polygons use separate stencil state. See
glStencilFuncSeparate.

## :stencil-writemask

data returns one value, the mask that controls writing of the stencil
bitplanes. The initial value is all 1's. See glStencilMask. This
stencil state only affects non-polygons and front-facing
polygons. Back-facing polygons use separate stencil state. See
glStencilMaskSeparate.

## :stereo

data returns a single boolean value indicating whether stereo buffers
(left and right) are supported.

## :subpixel-bits

data returns one value, an estimate of the number of bits of subpixel
resolution that are used to position rasterized geometry in window
coordinates. The value must be at least 4.

## :texture-compression-hint

data returns a single value indicating the mode of the texture
compression hint. The initial value is :dont-care.

## :texture-buffer-offset-alignment

data returns a single value, the minimum required alignment for
texture buffer sizes and offset. The initial value is 1. See
glUniformBlockBinding.

## :timestamp

data returns a single value, the 64-bit value of the current GL
time. See glQueryCounter.



## :uniform-buffer-offset-alignment

data returns a single value, the minimum required alignment for
uniform buffer sizes and offset. The initial value is 1. See
glUniformBlockBinding.



## :unpack-alignment

data returns one value, the byte alignment used for reading pixel data
from memory. The initial value is 4. See glPixelStore.

## :unpack-image-height

data returns one value, the image height used for reading pixel data
from memory. The initial is 0. See glPixelStore.

## :unpack-lsb-first

data returns a single boolean value indicating whether single-bit
pixels being read from memory are read first from the least
significant bit of each unsigned byte. The initial value is
:false. See glPixelStore.

## :unpack-row-length

data returns one value, the row length used for reading pixel data
from memory. The initial value is 0. See glPixelStore.

## :unpack-skip-images

data returns one value, the number of pixel images skipped before the
first pixel is read from memory. The initial value is 0. See
glPixelStore.

## :unpack-skip-pixels

data returns one value, the number of pixel locations skipped before
the first pixel is read from memory. The initial value is 0. See
glPixelStore.

## :unpack-skip-rows

data returns one value, the number of rows of pixel locations skipped
before the first pixel is read from memory. The initial value is
0. See glPixelStore.

## :unpack-swap-bytes

data returns a single boolean value indicating whether the bytes of
two-byte and four-byte pixel indices and components are swapped after
being read from memory. The initial value is :false. See
glPixelStore.

## :max-vertex-attrib-relative-offset

data returns a single integer value containing the maximum offset that
may be added to a vertex binding offset.

## :viewport

When used with non-indexed variants of glGet (such as glGetIntegerv),
data returns four values: the x and y window coordinates of the
viewport, followed by its width and height. Initially the x and y

window coordinates are both set to 0, and the width and height are set
to the width and height of the window into which the GL will do its
rendering. See glViewport.

When used with indexed variants of glGet (such as glGetIntegeri-v),
data returns four values: the x and y window coordinates of the
indexed viewport, followed by its width and height. Initially the x
and y

window coordinates are both set to 0, and the width and height are set
to the width and height of the window into which the GL will do its
rendering. See glViewportIndexedf.

## :viewport-bounds-range

data returns two values, the minimum and maximum viewport bounds
range. The minimum range should be at least [-32768, 32767].

## :viewport-index-provoking-vertex

data returns one value, the implementation dependent specifc vertex of
a primitive that is used to select the viewport index. If the value
returned is equivalent to :provoking-vertex, then the vertex
selection follows the convention specified by glProvokingVertex. If
the value returned is equivalent to :first-vertex-convention, then
the selection is always taken from the first vertex in the
primitive. If the value returned is equivalent to
:last-vertex-convention, then the selection is always taken from the
last vertex in the primitive. If the value returned is equivalent to
:undefined-vertex, then the selection is not guaranteed to be taken
from any specific vertex in the primitive.

## :viewport-subpixel-bits

data returns a single value, the number of bits of sub-pixel precision
which the GL uses to interpret the floating point viewport bounds. The
minimum value is 0.

## :max-element-index

data returns a single value, the maximum index that may be specified
during the transfer of generic vertex attributes to the GL.

Many of the boolean parameters can also be queried more easily using
glIsEnabled.
