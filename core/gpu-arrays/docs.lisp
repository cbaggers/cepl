(in-package :cepl.gpu-arrays)

(docs:define-docs
  (defstruct gpu-array
    "
GPU-ARRAY is a structure that respresents an array in gpu-memory.

Depending on how the array was created it is said that the gpu-array is either
'buffer-backed' or 'texture-backed'.

- buffer-backed: Means it was created with #'make-gpu-array or #'make-gpu-arrays
                 The data is stored in a gpu-buffer.
- texture-backed: Means it was created along with a texture. Textures are
                  structures that contain some number of gpu-arrays.
                  The data is stored in texture memory.
                  See 'texture' for more details.

Both have different performance characteristics, use cases and allow different
element types, however they are both an ordered block of typed data.
They can have multiple dimensions and you can pull-g and push-g data to and from
them.

Note for folks who are used to OpenGL:
texture-backed gpu-arrays are what opengl would normally call 'images'. This is
a pretty terrible name for them as it implices they only can hold image data, or
that they are 2d only. In fact 'images' can have 1 to 3 dimensions, can have
elements that are single bytes, floats, vectors of either or a large number of
other types.
'image' belies this nature and so CEPL chooses 'array'

This also matches how the GLWiki chooses to explain them:
> an image is defined as a single array of pixels of a certain
> dimensionality (1D, 2D, or 3D), with a particular size, and a specific format.
")

  (defun gpu-array-dimensions
      "
Return a list whose elements are the dimensions of the array.

You can also use the generic function #'dimensions to get this info.
")

  (defun gpu-array-access-style
      "
When passed a buffer-backed gpu-array this function will return the access-style
of the underlying gpu-buffer.

The access-style of a gpu-array (or gpu-buffer) is a hint to OpenGL on how you
intend to use the array. It is optional whether your gpu manufacturer's
implementation of GL takes any notice of this option. When they do take notice
of it, it will to optimize access to the underlying data.

There are also no repercussions for accessing the data in a way contrary to the
declared access-style (other than potential performance costs).
")

  (defun gpu-array-buffer
      "
When passed a buffer-backed gpu-array this function will return the gpu-buffer
backing this array.

Note that if you made the array with #'make-gpu-arrays then there will be
multiple gpu-arrays sharing this buffer, so care should be taken when modifying
or freeing data.
")

  (defun gpu-array-face-num
      "
When passed a texture-backend gpu-array this function will return the index of
the texture's cubeface that contains this gpu-array. This will only be
")

  (defun gpu-array-layer-num
      "
When passed a texture-backend gpu-array this function will return the index of
the texture's cubeface that contains this gpu-array.
"))
