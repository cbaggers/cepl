(in-package :cepl.pixel-formats)

(docs:define-docs
  (defstruct pixel-format
    "
`pixel-format`s exist to aid the handling and conversion of types in CEPL.

OpenGL does not make it easy to understand the types involved with textures.

First there is the name of the type you would use in lisp (or C)

Then there is the image-format (sometimes called internal-format) which
specifies the format on the GPU

Then there are the types and formats that must be specified when uploading or
downloading data to/from a texture (or rather a texture-backed gpu-array).

CEPL's pixel-fromat pulls together enough metadata that conversions to and from
these various representations is easy.

Technically you shouldnt need to use this directly as it will be handled by CEPL
however if you ever have to deal with the formats directly, this type can be a
boon.

It doesnt cover every combination yet, but will in time. Anything missing is
considered a bug, so please report it on Github so it can be added to the todo
list.


")

  (defun pixel-format-p
      "
This function returns t if the the given value is a `pixel-format`. Otherwise it
returns nil
")

  (defun pixel-format-components
      "
This function returns the components of the given `pixel-format` as a keyword

possible values are:
:r
:rg
:rgb
:rgba
:stencil-only
:depth
")

  (defun pixel-format-type
      "
This function returns the lisp type of a single 'element' of the given
`pixel-format`.

For example:
- the element type of a :vec3 is a :float.
- the element type of a :vec2 is a :float.
- the element type of a :int8 is a :int8
")

  (defun pixel-format-normalize
      "
This function returns whether values of the components belonging to the
`pixel-format` are normalized.

If t then sampling the values on the gpu will gives values in the
range 0s0 → 1s0
")

  (defun pixel-format-sizes
      "
Some `pixel-format`s and image-formats are 'special' and have specifically sized
components.

This function will return a list of sizes in bits of the components.
")

  (defun pixel-format-reversed
      "
Some `pixel-format`s are 'reversed' meaning the component order in the data is
reversed.

This function will return t if the pixel format provided is a reversed-type
")

  (defun pixel-format-comp-length
      "
This function when passed a `pixel-format` will return the number of components
it has. The values will be from 1 upto 4
"))
