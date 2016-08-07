(in-package :cepl.textures)

(docs:define-docs
  (defvar *immutable-available*
    "
After CEPL has been initialized this variable will hold t if immutable texture
storage is available and nil if not.

Immutable texture storage does not mean that the texture's gpu-array data is
immutable. It means that the underlying format of the texture data cannot be
changed. It is rare that you would want mutable storage and when you do, it is
very hard to not create 'incomplete-textures'.
https://www.opengl.org/wiki/Immutable_Storage_Texture#Texture_completeness

CEPL tries to make only complete textures so this is not a concern. If you do
need this level of control, please raise a github issue as I would love to
understand your usecase.
")

  (defstruct buffer-texture
    "
Buffer texture are a special kind of texture where the data resides, not in
texture memory, but in buffer memory.

This means that when you call texref on this texture you will recieve a
buffer-backed texture instead of the usual texture-backed ones.

buffer-textures are created by calling #'make-texture with the :buffer key
argument set to t.

Buffer textures have limitations over regular textures:
 - cannot be mipmapped
 - can only only have one gpu-array
 - that gpu-array must be one dimensional
 - can only be accessed in shaders with #'texel-fetch
 - can have a more limited number of valid element-types[0] to choose from
   see *valid-image-formats-for-buffer-backed-texture* for details

[0] OpenGL calls the element-types of textures 'image-formats' or sometimes
    'internal-formats'. Even though the name implies they are only useful
    for image data we keep the name as the subject of image-formats is
    complicated enough without us adding more things to think about")

  (defstruct texture
    "
Textures are structures that hold a number of gpu-arrays that all have the same
element-type.

Textures can be used as a source of data or can be attached to an fbo, which
means you can render into them instead of to the screen[0].

These texture-backed arrays cannot contain arbitrary data, they are specific
formats that are allowed in specific circumstances. CEPL makes working with
these as smooth as possible, but the details are confusing and the best sources
of information for the nitty-gritty are the GL-Wiki and the GL-Specification.


-- Texture Types --
The textures themselves have a number of configurations (known from here on as
'texture-types') that are allowed:

  :texture-1d
  Gpu-Arrays in this texture all are 1-dimensional.

  :texture-2d
  Gpu-Arrays in this texture all are 2-dimensional.

  :texture-3d
  Gpu-Arrays in this texture all are 3-dimensional.

  :texture-rectangle
  This texture only has one 2-dimensional gpu-array. The texture cannot have
  mipmapping. Texture coordinates used for these textures are not normalized.

  :texture-buffer
  See the documentation for buffer-texture for details

  :texture-cube-map
  There are exactly 6 distinct sets of 2D gpu-arrays, all of the same size.
  They act as 6 faces of a cube.

  :texture-1d-array
  Gpu-Arrays in this texture all are 1-dimensional. However, it contains
  multiple sets of 1-dimensional gpu-arrays, all within one texture.

  :texture-2d-array
  Gpu-Arrays in this texture all are 2-dimensional. However, it contains
  multiple sets of 2-dimensional gpu-arrays, all within one texture.

  :texture-cube-map-array
  Gpu-Arrays in this texture are all cube maps. It contains multiple sets of
  cube maps, all within one texture.

  :texture-2d-multisample & :texture-2d-multisample-array
  Not currently supported in CEPL


-- Element Types --

Unlike c-array and gpu-arrays (which use foreign types for their elements)
textures have a different set of acceptable format. These are called
'image formats'.

The name may imply that these formats are only usable for image data but this
is not the case. Most of the types we are used to are there, but under unsual
names.

This would make this a ripe candidate for CEPL to clean up the naming.. but it
doesnt, why?

Well the answer to that is image-formats are damn confusing and if we change
too much of the naming it will be even more confusing when trying to google
information. Instead CEPL provides a number of functions for converting
between (foreign) lisp types and their image-format counterparts.


-- Mip Maps --
When a texture is applied to a surface, the number of the texture's
pixels (commonly called 'texels') that are used depends on the angle at which
that surface is rendered. For example if we were rendering a TV then when it is
almost side on we will use much less pixels that if we were rendering it from
the front. At that point the gpu has to pick which texels from the texture to
use.
When an object is moving (or the camera is) the texels that are chosen to be
drawn each frame will not be the same, this causes nasty visual artifacts.

To get around this, gpus employ mip maps. These are pre-shrunk versions of the
full-sized gpu-array. Each mipmap is half the size of the previous one in the
chain. So a 64x16 2D texture can have 6 mip-maps: 32x8, 16x4, 8x2, 4x1,
2x1, and 1x1.

OpenGL does not require that the entire mipmap chain is complete; you can
specify what range of mipmaps in a texture are available.

Some texture types have multiple independent sets of mipmaps. Each face of a
cubemap has its own set of mipmaps, as does each entry in an array texture.

When sampling a texture (see below), the implementation will automatically
select which mipmap to use based on the viewing angle, size of texture,
and various other factors.


-- Mutable or Immutable Storage --
First we will explain what this is not, this is not about whether you can change
the contents of the gpu-arrays after the texture is created. It is about whether
you can redefine the 'nature' of the data stored in the gpu-arrays after they
are created, by that we mean things like the type of the data, the resolution of
the gpu-arrays.

It is hard to do without creating an 'incomplete' texture [1] and so CEPL opts
to treat all textures as immutable textures. When the OpenGL version doesnt
support immutable texture storage CEPL makes uses immutable storage,
allocates all the memory at construction, and doesnt provide abstractions over
the opengl features that would modify the storage.


-- Sampling --
Sampling is the process of fetching a value from a texture at a given position.
It is interesting as, because of things like mipmaping, the usual case is that
reading a single value from a texture involves reading from more than one of
its gpu-arrays and combining the resultson some fashion.

To control this process GL (and thus CEPL) have a special object called a
sampler. Samplers have a number of values that can be tweaked that affect how
the sampling is done.

To find out more see the Cepl.Samplers section.

[0] - more correctly the default-fbo
[1] - https://www.opengl.org/wiki/Immutable_Storage_Texture#Texture_completeness
")

  (defun texture-base-dimensions ()
    "
Returns the resolution of the gpu-array at the 'base-level' of the mipmap chain.
The base level is the largest one.
")

  (defun texture-cubes-p
      "
This function returns t if the texture is a cube texture and nil otherwise
")

  (defun texture-id
      "
This function returns the id of the GL Texture Object from the CEPL texture.

Don't use this unless you know what you are doing on the GL side.
")

  (defun texture-layer-count
      "
When called with a texture with one of the following texture-types:
:texture-1d-array
:texture-2d-array
:texture-cube-map-array

then this function returns the number of 'layers' in the texture.

For all other texture types this will return 0
")

  (defun texture-mipmap-levels
      "
When called with a texture with mipmaps enabled this function returns the number
of 'mipmap levels' in the texture.

For all other texture types this will return 0
")

  (defun texture-mutable-p
      "
When called with a texture this function returns t if the texture was made with
mutable texture storage and nil otherwise.

Using immutable texture storage does not mean that the data inside the texture's
gpu-arrays cannot be mutated.

Mutable texture storage allows you to redefine the 'nature' of the data stored
in the gpu-arrays after they are created, by that we mean things like the type
of the data, the resolution of the gpu-arrays.

As it is hard to change such things without creating an 'incomplete' texture[0]
CEPL opts to treat all textures as immutable textures.
When the OpenGL version doesnt support immutable texture storage CEPL makes uses
immutable storage, allocates all the memory at construction, and doesnt provide
abstractions over the opengl features that would modify the storage.

[0] - https://www.opengl.org/wiki/Immutable_Storage_Texture#Texture_completeness
")

  (defun texture-type
      "
This function will return the type of the given texture.

The possible values are:

:texture-1d
:texture-2d
:texture-3d
:texture-rectangle
:texture-buffer
:texture-cube-map
:texture-1d-array
:texture-2d-array
:texture-cube-map-array
:texture-2d-multisample
:texture-2d-multisample-array
")

  (defmacro with-texture-bound
      "
Binds the given texture to the gl-context for the duration of the body and then
ensures it is unbound.

You do not need to interact with this directly as it will be handled
by map-g and the pipeline itself.
")

  (defun buffer-texture-p
      "
Return t if the given value is a texture who data is stored in a gpu-buffer as
opposed to the usual texture memory. Returns nil otherwise
")

  (defun generate-mipmaps
      "
Calling this with a texture asks OpenGL to generate mipmaps for the texture.

You do not normally need to use this function as you can use the :mipmaps
argument on #'make-texture. See #'make-texture for more details.
")

  (defun make-texture
      "
This function allows you to make any kind of texture based on the arguments
provided. Whilst the signature is quite intimidating your rarely need to use
more than a few of the arguments to make a given texture.

For example: (make-texture '(1 2 3 4)) will give you a valid texture.

We will now go through the arguments and their behaviours:

           - - - - - - - - - - - - - - - - - - - - - - - -

:initial-contents :dimensions & :element-type

- with :initial-contents to nil:
  In this case you need to provide dimensions and an element-type.

- with :initial-contents populated.
  The initial-contents can be a (potentially nested) list, array or c-array.

  When the :initial-contents are a c-array then the dimensions and element-type
  are taken from the c-array. As the data is already in foreign memory the
  upload will be notable faster that from lisp-data->gpu as no type conversions
  are needed

  When the :initial-contents are an array then the dimension of the texture
  will be the same as the array passed in. Remember OpenGL only allows up to
  3 dimensions for the textures

  When the :initial-contents is a flat list then each element is used as one
  element in the textures gpu-array
  If the :initial-contents is a nested list then you must either:
  - specify multiple dimensions and an element-type
  - specify an element-type to be some struct type, then nested lists are then
    used to populate the fields of the foreign structs. For an example of this
    please see this example: https://github.com/cbaggers/cepl.examples/blob/master/examples/triangle.lisp#L30.

  If the :element-type is not provided then CEPL will look at every element in
  the initial-contents and try and find the smallest (in bytes) foreign type
  which works for every element. This mean if the array is full of single-floats
  then CEPL will choose :float, not :double.
  Naturally this behaviour is too slow for use in performance critical
  applications however it is nice for experimentation and working from the repl.

Extra element-type details:
Unlike c-array and gpu-arrays (which use foreign types for their elements)
textures have a different set of acceptable element-types. These are called
'image formats'.

If you provide a GL image-format as the :element-type CEPL will use it directly

If you provide a (foreign) lisp type then CEPL will try to find the appropriate
image-format for that type.

          - - - - - - - - - - - - - - - - - - - - - - - -

:pixel-format

If this optional argument is not nil, then CEPL will use the pixel format
provided when uploading the pixel data.

Usually this is not provided as CEPL can calculate a valid pixel-format to
use from the element-type.

          - - - - - - - - - - - - - - - - - - - - - - - -

:mipmap & :generate-mipmaps

If this is set to t then CEPL will make mipmaps for the texture. The texture
will have (floor (log (apply #'max dimensions) 2)) levels of mipmaps

If this is set to a positive integer CEPL will check that the number of levels
is allowed for the dimensions given and make that many levels of mipmaps.

If generate-mipmaps is t CEPL will ask OpenGL to generate content for all the
mipmaps levels.

          - - - - - - - - - - - - - - - - - - - - - - - -

:layer-count

If this is set to a value greater than 1 then you are trying to create one of
the following:

:texture-1d-array
:texture-2d-array
:texture-cube-map-array

Which one is created depends on the dimensions provided and whether cubes is t.

          - - - - - - - - - - - - - - - - - - - - - - - -

:cubes
If this is t then you are trying to make a cube-map texture

          - - - - - - - - - - - - - - - - - - - - - - - -

:rectangle
If this is t then you are trying to make a rectangle texture.

This is different from a regular 2d texture (which can also be rectangular)

This texture only has one 2-dimensional gpu-array. The texture cannot have
mipmapping. Texture coordinates used for these textures are not normalized.

          - - - - - - - - - - - - - - - - - - - - - - - -

:multisample
This is not currently supported in CEPL

          - - - - - - - - - - - - - - - - - - - - - - - -

:immutable
Set this to nil if you definitely dont want to use immutable texture storage.
See the docstring for 'texture for more details

          - - - - - - - - - - - - - - - - - - - - - - - -

:buffer-storage
If this is set to t you are trying to make a buffer-texture.

If you set this to t then the element-type should NOT be an image-format. Use
a foreign lisp type instead. Or leave it nil if you want CEPL to take the type
from the initial-contents

See the docstring for 'texture for more details.
")

  (defun make-texture-from-id
      "
Wrap and existing GL Texture Object in a CEPL texture struct.

This function does not do ANY sanity checking on the values provided, use only
if you are 100% sure of what you are setting.

For details on the meaning of the arguments see the docstring for #'make-texture
")

  (defun texref
      "
This function allows you to access a specific gpu-array from the texture.

:mipmap-level is invalid if the texture doesnt have mipmaps

:layer is invalid if the texture-type is not a 1d,2d or cube-map array-texture.

:cube-face is invalid if the texture is not a cube-map texture
")

  (defun texture-element-type
      "
This function returns the element-type of the given texture.

If the texture is a buffer-texture then the element-type will be a foreign lisp
type.

Otherwise it will be one of the image-formats

-- Element Types Details --

Unlike c-array and gpu-arrays (which use foreign types for their elements)
textures have a different set of acceptable format. These are called
'image formats'.

The name may imply that these formats are only usable for image data but this
is not the case. Most of the types we are used to are there, but under unsual
names.

This would make this a ripe candidate for CEPL to clean up the naming.. but it
doesnt, why?

Well the answer to that is image-formats are damn confusing and if we change
too much of the naming it will be even more confusing when trying to google
information. Instead CEPL provides a number of functions for converting
between (foreign) lisp types and their image-format counterparts.

")

  (defun texture-p
      "
This function returns t if the given value is a texture, otherwise nil is
returned.
")

  (defun free-texture
      "
This function will free the texture and all texture-memory behind the texture's
gpu-arrays.

Be sure not to use those arrays after this function has been called.
"))
