(in-package :cepl.gpu-arrays)

(docs:define-docs
  (defstruct gpu-array
    "
GPU-ARRAY is a structure that represents an array in gpu-memory.

Depending on how the array was created it is said that the gpu-array is either
'buffer-backed' or 'texture-backed'.

- buffer-backed: Means it was created with #'make-gpu-array or #'make-gpu-arrays
                 The data is stored in a gpu-buffer.
- texture-backed: Means it was created along with a texture. Textures are
                  structures that contain some number of gpu-arrays.
                  The data is stored in texture memory.
                  See 'texture' for more details.

Both have different use cases and allow different element types, however they
are both an ordered block of typed data. They can have multiple dimensions and
you can push-g and pull-g data to and from them.

Note for folks who are used to OpenGL:
texture-backed gpu-arrays are what opengl would normally call 'images'. This is
a pretty terrible name for them as it implies they only can hold image data, or
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
When passed a texture-backed gpu-array this function will return the index of
the texture's cubeface that contains this gpu-array. This only truly applies to
gpu-arrays belonging to cubemap textures, for all gpu-arrays this number will
always be 0.
")

  (defun gpu-array-layer-num
      "
When passed a texture-backed gpu-array this function will return the index of
the gpu-array within the array-textures. This only truly applies to gpu-arrays
 belonging to array-textures, for all gpu-arrays this number will always be 0.
")

  (defun gpu-array-element-type
      "
Will return the type of the elements in the gpu-array given.

If this is a texture-backed gpu-array then the element-type will be the same as
the 'image-format' of the texture.
")

  (defun gpu-array-level-num
      "
When passed a texture-backed gpu-array this function will return the
mipmap level the gpu-array resides on within the texture.

This only truly applies to gpu-arrays with mipmaps, for all gpu-arrays this
number will always be 0.
")

  (defun gpu-array-p
      "
Will return t if the value given is a gpu-array.

This will return t for both texture-backed and buffer-backed gpu-arrays
")

  (defun gpu-array-texture-type
      "
When passed a texture-backed gpu-array this function will return the
type of the texture containing this gpu-array.

The result will be one of the following:
:texture-1d
:texture-2d
:texture-3d
:texture-1d-array
:texture-2d-array
:texture-cube-map
:texture-cube-map-array
:texture-rectangle
:texture-buffer
:texture-buffer
:texture-buffer
:texture-2d-multisample
:texture-2d-multisample-array
")

  (defun make-gpu-array
      "
This function creates a buffer-backed gpu-array.
Texture-backed gpu-array can only be created via #'make-texture

make-gpu-array is very similar to make-c-array.

It can be used in a few different ways:

- with :initial-contents to nil:
  In this case you need to provide dimensions and an element-type.

- with :initial-contents populated.
  The initial-contents can be a (potentially nested) list, array or c-array.

  When the :initial-contents are a c-array then the dimensions and element-type
  are taken from the c-array. As the data is already in foreign memory the
  upload will be notable faster that from lisp-data->gpu as no type conversions
  are needed

  When the :initial-contents are an array then the dimension of the gpu-array
  will be the same as the array passed in. CEPL currently only supports up
  to 4D gpu-arrays.

  When the :initial-contents is a flat list then each element is used as one
  element in the gpu-array.
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

  If you need what would be called a displaced array in lisp then please see the
  subseq-g function.


Access style is optional but if you are comfortable with
opengl, and know what type of usage pattern this array will
have, you can set this to any of the following:

The access-style argument is a hint to OpenGL on how you intend to use
the array. It is optional whether your gpu manufacturer's implementation of GL
takes any notice of this option. When they do take notice of it, it will to
optimize access to the underlying data.

There are also no repercussions for accessing the data in a way contrary to the
declared access-style (other than potential performance costs).

:stream-draw  :static-draw  :dynamic-draw
:stream-read  :static-read  :dynamic-read
:stream-copy  :static-copy  :dynamic-copy
")

  (defun make-gpu-arrays
      "
This function takes a list of x c-arrays and returns a list of x buffer-backed
gpu-arrays.
The reason to use this rather than (mapcar #'make-gpu-array c-arrays-list) is
that all of the gpu-arrays created will share the same gpu-buffer.

Usually you will know if you need this instead of #'make-gpu-array as you will
have some behaviour or performance characteristic in mind.
")

  (defun subseq-g
      "
This function returns a gpu-array which contains a subset of the gpu-array
passed into this function.

It does not copy the foreign data, instead this array points to within the data
of the original array. This means these arrays now share data (like a displaced
array in standard CL.

Due to this you have to be very careful when freeing the underlying array as
this will affect any other array sharing that data.

The reason that this arguably more dangerous behaviour is default is efficiency.
Mofidying gpu memory in performance critical applications should be done at
specific times so as not to get blocked by rendering commands. As such CEPL
tries not to allocate new memory when the function is not explicitly about that.
")

    (defun subseq-g-raw
      "
This function returns a gpu-array which contains a subset of the gpu-array
passed into this function optionally allowing you to change the element-type
of the resulting gpu-array.

It does not copy the foreign data, instead this array points to within the data
of the original array. This means these arrays now share data (like a displaced
array in standard CL.

Due to this you have to be very careful when freeing the underlying array as
this will affect any other array sharing that data.

Also you have to be careful that the new element-type you choose makes sense
given the data already in the arrays. For example taking an gpu-array of :vec4
and making an gpu-array of :float will give you sensible values, however making
a gpu-array of :int will give you garbage.

Unless you have a very specific use-case then it is best to use #'subseq-g

The reason that this arguably more dangerous behaviour is default is efficiency.
Mofidying gpu memory in performance critical applications should be done at
specific times so as not to get blocked by rendering commands. As such CEPL
tries not to allocate new memory when the function is not explicitly about that.
")

  (defun backed-by
      "
This function takes a gpu-array and returns either :texture or :buffer depending
on whether it is a texture-backed gpu-array or a buffer-backed gpu-array.
")

  (defun free-gpu-array
      "
When given a buffer-backed gpu-array this will do one of three things:

If the gpu-array is the only gpu-array in the gpu-buffer then the gpu-buffer is
freed and the gpu-array is blanked.

If the gpu-array shares a gpu-buffer with other gpu-arrays and they are still
live then blank this gpu-array.

If the gpu-array shared a gpu-buffer with other gpu-buffer but they have all
been freed then free the gpu-buffer and blank the gpu-array.

Blanking the gpu-array means its fields will be set to default values,
for example dimensions will be nil, the texture will be null etc.

The generic function #'free will call #'free-gpu-array when passed a gpu-array.
")

  (defun with-gpu-array-as-c-array
      "
This macro takes a gpu-array and asks OpenGL to temporarily 'map' it to
a c-array. Within the scope of the body you can run any of the c-array commands
on it.

This macro is really helpful if you need to have random access to the data in
the gpu-array.

A simple example would be if we wanted to set the 3rd element in a gpu array to
5.0 we could do the following:

    (with-gpu-array-as-c-array (my-gpu-array)
      (setf (aref-c my-gpu-array 2) 5.0))

The reason we provide this and not a function like #'aref-c for gpu-arrays is
that it would give the impression that this kind of operation is cheap, which it
is not. There are cases where using with-gpu-array-as-c-array will perform
better than #'push-g and there cases where the opposite is true.
Generally this will be to do with how much of the block of memory is being
updated, but it is best to consult current graphics programming texts to find
out the details.

The valid values for the :access argument are :read-only :write-only or
:read-write.
")

  (defun with-gpu-array-as-pointer
      "
This macro takes a gpu-array and asks OpenGL to temporarily 'map' it to
a pointer. Within the scope of the body you can run any cffi command on it.

This macro is really helpful if you need to have random access to the data in
the gpu-array.

The reason we provide this and not a function like #'aref-c for gpu-arrays is
that it would give the impression that this kind of operation is cheap, which it
is not. There are cases where using with-gpu-array-as-c-array will perform
better than #'push-g and there cases where the opposite is true.
Generally this will be to do with how much of the block of memory is being
updated, but it is best to consult current graphics programming texts to find
out the details.

The valid values for the :access argument are :read-only :write-only or
:read-write.
"))
