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
- can have a more limited number of valid internal-formats to choose from
  see *valid-internal-formats-for-buffer-backed-texture* for details
")

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

-- Mip Maps --
When a texture is applied to a surface, the number of the texture's pixels
(commonly called 'texels') that are used depends on the angle at which that
surface is rendered. For example if we were rendering a TV then when it is
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

It is hard to do without creating and 'incomplete' texture [1]



[0] - more correctly the default-fbo
[1] - https://www.opengl.org/wiki/Immutable_Storage_Texture#Texture_completeness
")

  (defun make-texture
      "
Textures have a number of configurations (known from here on as 'texture-types')
that are allowed, they are enumerated in #'make-texture.


"))
