
# Textures

Ah textures! The area of opengl with probably the most misleading names in all of graphics.

#### Texture

Lets start with textures themselves. I cant speak for everyone but when I started this Ι thought a texture was a kind of image that you wrapped onto the 3d model in your game/application/whatever. It turns out that I was very wrong.

The open gl spec says:

> A texture is an OpenGL Object that contains one or more images that all have the same image format.

So a texture is a datastructure that contains a number of 'images'

#### Image

But image is well named right? NO! An 'image' in opengl can be 1, 2 or 3 dimensional. It can contain bytes, floats, doubles, vectors..these are starting to sounds just like arrays again! Actually thats really what they are, arrays with some extra features of course, but arrays none the less. In fact the GL wiki makes this very clear:

> an image is defined as a single array of pixels of a certain dimensionality (1D, 2D, or 3D), with a particular size, and a specific format.

Reading the data out of a gpu-array (image) in a shader is called `sampling`

#### Return of Gpu-Array

Given that we already have an abstraction for arrays on the gpu it made sense to use it here.

If you have read chapter `005` you will have seen that gpu-arrays tell you what kind of memory they are `backed by`. So, for example, the following code:

```
     (make-gpu-array '(1 2 3))
```

gives us this:

```
     #<GPU-ARRAY :element-type :UBYTE :dimensions (3) :backed-by :BUFFER>
```

Which is a gpu-array *backed by* an opengl buffer, which meant the data for the array was stored in an opengl buffer.

When we access a gpu-array from inside a texture we will see it is *backed-by* `:texture`..But we are getting ahead of ourselves, let's recap.

#### Where were we?

Ok so in cepl terminology a texture is a datastructure that contains 1 or more gpu-arrays. Opengl textures can a have a bewildering number of options applied to them and while cepl can make the experience easier, some things are just gonna be confusing at first. Rest assured though that you can get useful stuff done without knowing all of it. If you want to survey your options check out this pages:

https://www.opengl.org/wiki/Texture
https://www.opengl.org/wiki/Texture_Storage

With that said let's begin. I'm going to start with a bunch of simple examples to show things informally, and then we can do the rigourous version later.

#### 1D Texture

It's fairly common to talk about a texture with a single 1 dimensional array as a 1D texture. To make one of these is very similar to making a c-array or gpu-array

```
CEPL> (defvar x (make-texture '(1 2 3 4)))
#<GL-TEXTURE-1D (4)>
```

This is the simplest possible texture, notice how it uses the same technique as before if you dont provide the `:element-type`.

How about we take a look at the gpu-array inside this texture?

Like we have `aref` for lisp arrays and `aref-c` for `c-arrays`, we have `texref` for looking inside textures. `texref` lets you index into the texture by the `mipmap-level` `layer` and `cube-face`, these are opengl terms we havent covered yet so dont worry if they are foreign. They are all optional so if our texture only has one image then you only need to do this:

```
CEPL> (defvar a (texref x))
#<GPU-ARRAY :element-type :R8 :dimensions (4) :backed-by :TEXTURE>
```

Well that looks familiar!

`pull-g` still works of course:

```
CEPL> (pull-g a)
(1 2 3 4)
```

As does `push-g`

```
(push-g '(5 6 7 8) a)
#<GPU-ARRAY :element-type :R8 :dimensions (4) :backed-by :TEXTURE>
```

As before we can make an array without initializing the contents

```
CEPL> (make-texture nil :dimensions 10 :element-type :ubyte)
#<GL-TEXTURE-1D (10)>
CEPL> (texref *)
#<GPU-ARRAY :element-type :R8 :dimensions (10) :backed-by :TEXTURE>
```

Eagle-eyed readers will notice that the `:element-type` we provided was `:ubyte` yet the `:element-type` of the gpu-array is `:r8`. Textures have special type names for their data and Cepl is just picking the matching type. OpenGL's textures types are a topic of their own so I'll cover it a bit later. For now just know that Cepl lets you use regular types when it can work out the equivalent **or** the official OpenGL names.

#### More dimensions

We can make 2D and 3D textures as we would expect
```
CEPL> (make-texture #2A ((1 2 3) (4 5 6)))
#<GL-TEXTURE-2D (2x3)>

CEPL> (make-texture nil :dimensions '(10 10 10) :element-type :ubyte)
#<GL-TEXTURE-3D (10x10x10)>
```

#### Subseq-g for Texture backed gpu-arrays?

Having this would mean we could `pull-g` from, and `push-g` to, a portion of the texture. This is on the roadmap but not currently implemented.

#### MipMaps

I'll just paste chunks of the GL wiki's description of mipmaps as it is very good:

> When a texture is directly applied to a surface, how many pixels of that texture (commonly called "texels") are used depends on the angle at which that surface is rendered. A texture mapped to a plane that is almost edge-on with the camera will only use a fraction of the pixels of the texture. Similarly, looking directly down on the texture from far away will show fewer texels than an up-close version.
> The problem is with animation. When you slowly zoom out on a texture, you start to see aliasing artifacts appear. These are caused by sampling fewer than all of the texels; the choice of which texels are sampled changes between different frames of the animation. Even with linear filtering, artifacts will appear as the camera zooms out.

> To solve this problem, we employ mip maps. These are pre-shrunk versions of the full-sized image. Each mipmap is half the size of the previous one in the chain, using the largest dimension of the image . So a 64x16 2D texture can have 6 mip-maps: 32x8, 16x4, 8x2, 4x1, 2x1, and 1x1. OpenGL does not require that the entire mipmap chain is complete; you can specify what range of mipmaps in a texture are available.

> Some texture types have multiple independent sets of mipmaps. Each face of a cubemap has its own set of mipmaps, as does each entry in an array texture `(more on these later)`. However, the texture as a whole only has one setting for which mipmaps are present. So if the texture is set up such that only the top 4 levels of mipmaps present, you must have them for all mipmap chains in the texture.

> When sampling a texture (see below), the implementation will automatically select which mipmap to use based on the viewing angle, size of texture, and various other factors.

> When using texture sizes that are not powers of two, the half-size of lower mipmaps is rounded down. So a 63x63 texture has as its next lowest mipmap level 31x31. And so on.

> The base level of a mipmap chain is the largest one. It is also the one that defines the full size of the texture. OpenGL numbers this mipmap level as 0; the next largest mipmap level is 1, and so on.

> The base level of a texture does not have to be loaded. As long as you specify the range of mipmaps correctly, you can leave out any mipmap levels you want.

There are two mipmap related `&keys` in `#'make-texture`, `:mipmap` and `:generate-mipmaps`.

- `:mipmap` let's you pick the number of mipmap levels there will be
- `:generate-mipmaps` is either `t` or `nil` and specifies whether GL will generate the mipmap images for you or not.

```
CEPL> (defvar x2d (make-texture nil :dimensions '(512 512) :element-type :ubyte-vec4  :mipmap 4 :generate-mipmaps t))
#<GL-TEXTURE-2D (512x512) mip-levels:4>
```

To get those different gpu-arrays we can still use `texref` but we can now use the mipmap-level `&key`

```
CEPL> (texref x2d :mipmap-level 0)
#<GPU-ARRAY :element-type :RGBA8 :dimensions (512 512) :backed-by :TEXTURE>
CEPL> (texref x2d :mipmap-level 1)
#<GPU-ARRAY :element-type :RGBA8 :dimensions (128 128) :backed-by :TEXTURE>
CEPL> (texref x2d :mipmap-level 2)
#<GPU-ARRAY :element-type :RGBA8 :dimensions (85 85) :backed-by :TEXTURE>
```

Remember each level is just a gpu-array so `pull-g`ing and `push-g`ing to them work just fine.

If you want to generate mipmaps for an existing texture, simply call #'generate-mipmaps on it.

#### Cube Textures

Cube textures are pretty cool, as the GL wiki puts it:

> A Cubemap Texture is a texture, where each mipmap level consists of six 2D images. The images are arranged in a cube-shape, hence the name. Cubemaps can have multiple mipmap levels.

So in our parlance a cubemap texture has 6 2d gpu-arrays at each mipmap level.

The way we sample the data from cube textures is cool, you can read all the gory details here: https://www.opengl.org/wiki/Cubemap_Texture#Samplers

To make a cube-texture in Cepl we can write the following:

```
CEPL> (defvar c (make-texture nil :dimensions '(10 10) :element-type :ubyte-vec4 :cubes t))
#<GL-TEXTURE-CUBE-MAP (10x10)>
```

And to access the gpu-arrays

```
CEPL> (texref c :cube-face 0)
#<GPU-ARRAY :element-type :RGBA8 :dimensions (10 10) :backed-by :TEXTURE>
CEPL> (texref c :cube-face 3)
#<GPU-ARRAY :element-type :RGBA8 :dimensions (10 10) :backed-by :TEXTURE>
```

#### Array-Textures

Textures can hold also arrays of gpu-arrays. The description from the wiki is as follows:

> An Array Texture is a Texture where each mipmap level contains an array of images of the same size. Array textures may have Mipmaps, but each mipmap in the texture has the same number of levels.

Notice they use the term **layers** when talking about the elements of the array, and number of layers when talking about the length. This is a bit odd but it is OpenGL parlance so (for now at least) Cepl is using it.

As always, the source of truth on these matters in the GL spec and https://www.opengl.org/wiki/Array_Texture

Array-Textures can be arrays of 1D, 2D, 3D or Cube texture. Cepl tries to get this right but, to be honest, it gets rather confusing. If you find a combination that should work (according to the GL spec) and doesnt, please let us know on github.

To make an array texture we do this using the `:layer-count` `&key` argument

```
(make-texture nil :dimensions 10 :element-type :ubyte-vec4 :layer-count 8)
ERROR
```
Woops, seems Cepl has a bug. You can track this here: https://github.com/cbaggers/cepl/issues/48


#### Buffer Textures

Buffer textures are interesting:

> A Buffer Texture is a one-dimensional Texture whose storage comes from a Buffer Object. They are used to allow a shader to access a large table of memory that is managed by a buffer object.

There are a bunch of limitations to buffer textures (other than being 1D)

- limited size: See `GL_MAX_TEXTURE_BUFFER_SIZE` for details
- cannot use filtering (which we havent covered yet)
- cannot have mipmaps

Making them is super easy though

```
CEPL> (defvar b (make-texture nil :dimensions 10 :element-type :ubyte-vec4 :buffer-storage t))
#<GL-TEXTURE-BUFFER (10)>
```
And the fun bit is, we have the perfect type for the gpu-array :)

```
CEPL> (texref b)
#<GPU-ARRAY :element-type :UBYTE-VEC4 :dimensions (10) :backed-by :BUFFER>
```
Yup, a **buffer-backed** gpu-array!


#### Rectangle Textures

> A Rectangle Texture is a Texture that contains a single 2D image with no mipmaps. It has no power-of-two restrictions on its size. Texture coordinates for accessing this texture must be texel values (floating-point), representing texels within the texture, rather than normalized texture coordinates.

Interesting restrictions but it can be useful.

```
CEPL> (make-texture nil :dimensions '(10 20) :element-type :ubyte-vec4 :rectangle t)
#<GL-TEXTURE-RECTANGLE (10x20)>
```

#### Mutisample Textures

Cepl does not support these yet.


#### Mutable and Immutable Texture Storage

I was confused by this at first as I thought it was talking about the data inside the texture's gpu arrays, but no. All texture data is mutable, but OpenGL traditionally allowed you to redefine the nature of the storage on an existing texture. It also required you to do a lot more work setting up.

Cepl will use immutable texture storage if your GL supports it and mutable if not. Cepl will (read should) give you a texture that is ready to use (the GL term is *complete*) for details see this page: https://www.opengl.org/wiki/Texture_Storage and prepare for your brain to melt (well mine did anyway)


#### Image Formats

Ok, brace yourselves...this gets hairy.

Image formats dictate what dat can be stores in a texture's gpu-arrays and also how they are stored (and potentially accessed)

The main wiki page of Image Formats is here, https://www.opengl.org/wiki/Image_Format but I actually find that page to be missing A LOT of data, so also see this page：https://www.opengl.org/wiki/GLAPI/glTexStorage2D

Yay choices. Ok so Cepl does not support all this yet. It's a shame but it doesnt. Ιt could though, so at some point I need to get back to it.

Things that are defintely unsupported are:

- Compressed formats
- S3TC/DXT

Things that are in a questionable state:

- Stencil format: I havent tested this yet.


Auto conversion:
Cepl can help with types by providing conversions from lisp types to their equivalent image formats.

The convertable types are: `:ubyte :byte :ushort :short :uint :int :float`

Of course opengl has many more potential format that this so feel free to use any of the following formats.

```
	:r8 :r8-snorm :r16 :r16-snorm :rg8 :rg8-snorm :rg16 :rg16-snorm :rgb8
    :rgb8-snorm :rgb16-snorm :rgba8 :rgba8-snorm :rgba16 :r32f :rg32f :rgb32f
    :rgba32f :r8i :r8ui :r16i :r16ui :r32i :r32ui :rg8i :rg8ui :rg16i :rg16ui
    :rg32i :rg32ui :rgb8i :rgb8ui :rgb16i :rgb16ui :rgb32i :rgb32ui :rgba8i
    :rgba8ui :rgba16i :rgba16ui :rgba32i :rgba32ui :srgb8 :srgb8-alpha8 :rgba2
    :rgba4 :r3-g3-b2 :rgb5-a1 :rgb10-a2 :rgb10-a2ui
	:depth-component16 :depth-component24 :depth-component32 :depth-component32f
	:stencil-index8))
```
If you are missing a format, or you are having issues with pulling or pushing data, as usual, please file an issue report on github.


#### Pixel Formats

Arghh too many format. The short version is that `pixel formats` are not equal to `image formats`. `Pixel formats` define the format of the data on the cpu side (client side in opengl terminology) and `image formats` define the gpu side.

See here for details: https://www.opengl.org/wiki/Pixel_Transfer#Format_conversion

There are only certain combinations that make sense and so Cepl should just **do the right thing**. Any cases where it doesnt are considered a bug.


#### Freeing

You can use either `#'free` or `#'free-texture` to free textures (and all their contained gpu-arrays)

You can't free a texture backed gpu-array on it's own.
