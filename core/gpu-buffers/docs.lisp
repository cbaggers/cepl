(in-package :cepl.gpu-buffers)

(docs:define-docs

  (defstruct gpu-buffer
    "
gpu-buffer is a struct that abstracts a OpenGL 'Buffer Object'

Along with the the ID of the GL Object itself it stores the layout of the data
in the buffer.

This layout is as follows:
`((data-type data-index-length offset-in-bytes-into-buffer))

for example:

`((:float 3 0) ('vert-data 140 12))

It is not expected that users will be using gpu-buffer's directly. Instead they
are ususal interacted with via CEPL's gpu-array and ubo features.
")


  (defun gpu-buffer-p
      "
This function returns t if the given value is a gpu-buffer. Otherwise it
returns nil.
")

  (defun gpu-buffer-id
      "
This function, when passed a gpu-buffer will return the OpenGL buffer object
from the gpu-buffer.

It is not recommended to modify this directly as the changes from doing so won't
be reflected in the layout of the gpu-buffer, which may potentially put the
gpu-buffer (and and gpu-array or ubo using it) into an invalid state.
")

  (defun gpu-buffer-format
      "
This function, when passed a gpu-buffer will return the format list used
by the gpu-buffer to keep track of the types and layout of the data in the
OpenGL Buffer Object
")


  (defun make-gpu-buffer
      "
This function creates and returns a new gpu-buffer.


If you wish to populate the buffer during construction you can pass a c-array as
the :initial-contents.


The :usage argument is a hint to OpenGL on how you intend to use the gpu-buffer.
It is optional whether your gpu manufacturer's implementation of GL takes any
notice of this option. When they do take notice of it, it will to optimize
access to the underlying data.


The :target argument can take any of the following binding targets:

Buffer Binding Target      Purpose
:array-buffer              Vertex attributes
:atomic-counter-buffer     Atomic counter storage
:copy-read-buffer          Buffer copy source
:copy-write-buffer         Buffer copy destination
:dispatch-indirect-buffer  Indirect compute dispatch commands
:draw-indirect-buffer      Indirect command arguments
:element-array-buffer      Vertex array indices
:pixel-pack-buffer         Pixel read target
:pixel-unpack-buffer       Texture data source
:query-buffer              Query result buffer
:shader-storage-buffer     Read-write storage for shaders
:texture-buffer            Texture data buffer
:transform-feedback-buffer Transform feedback buffer
:uniform-buffer            Uniform block storage

Do note that the default of :array-buffer is perfectly fine for creating the
gpu-buffer and uploading the data. It does not limit how the buffer can be used
in future parts of your program.

")

  (defun make-gpu-buffer-from-id
      "
This function takes an existing GL Buffer Object and wraps it in a new
gpu-buffer.


If you wish to populate the buffer during construction you can pass a c-array as
the :initial-contents.


The :usage argument is a hint to OpenGL on how you intend to use the gpu-buffer.
It is optional whether your gpu manufacturer's implementation of GL takes any
notice of this option. When they do take notice of it, it will to optimize
access to the underlying data.


The :target argument can take any of the following binding targets:

Buffer Binding Target      Purpose
:array-buffer              Vertex attributes
:atomic-counter-buffer     Atomic counter storage
:copy-read-buffer          Buffer copy source
:copy-write-buffer         Buffer copy destination
:dispatch-indirect-buffer  Indirect compute dispatch commands
:draw-indirect-buffer      Indirect command arguments
:element-array-buffer      Vertex array indices
:pixel-pack-buffer         Pixel read target
:pixel-unpack-buffer       Texture data source
:query-buffer              Query result buffer
:shader-storage-buffer     Read-write storage for shaders
:texture-buffer            Texture data buffer
:transform-feedback-buffer Transform feedback buffer
:uniform-buffer            Uniform block storage

Do note that the default of :array-buffer is perfectly fine for creating the
gpu-buffer and uploading the data. It does not limit how the buffer can be used
in future parts of your program.

Managed is a piece of metadata used by CEPL for knowing when to release the
gpu memory. This should be removed from the public API")


  (defun free-buffer
      "
This function, when passed a gpu-buffer will free the memory on the gpu and
'blank' the gpu-buffer.

Blanking in this case means that the slots of the gpu-buffer will all be set to
default values.
")

  (defun free-buffers
      "
This function, when passed a list of  gpus-buffer will free the gpu memory for
all the gpus-buffers and 'blank' them.

Blanking in this case means that the slots of each gpu-buffer will all be set to
default values.
")


  (defun buffer-reserve-block
      "
This function creates an empty block of data in the opengl buffer equal in size
to (* length size-in-bytes-of-type).

It will remove ALL data currently in the buffer")

  (defun buffer-reserve-block-raw
      "
This function creates an empty block of data in the opengl buffer.
It will remove ALL data currently in the buffer.

It also will not update the format of the buffer so you must be sure to handle
this yourself. It is much safer to use this as an assistant function to one
which takes care of these issues")

  (defun buffer-reserve-blocks
      "
This function creates an empty block of data in the opengl buffer equal in size
to the sum of all of the (* length size-in-bytes-of-type) in types-and-lengths.

The types-and-lengths list should be of the format:

    `((type length) (type length) ...etc)

It will remove ALL data currently in the buffer")


  (defun buffer-data
  "
This function populates the gpu-buffer with the contents of the c-array.

You also pass in the buffer type and the draw type this buffer is to be used
for.

The function returns a buffer object with its format slot populated with the
details of the data stored within the buffer")

  (defun buffer-sub-data
  "
This function replaces a subsection of the data in the specified buffer with
the data in the c-array.

The byte offset specifies where you wish to start overwriting data from.

When the :safe option is t, the function checks to see if the data you are about
to write into the buffer will cross the boundaries between data already in the
buffer and will throw an error if you are.")

  (defun multi-buffer-data
      "
This function takes a list of c-arrays and uploads all of the data to the
gpu-buffer, it then updates the gpu-buffer-format with the new layout.
")

  (defun buffer-data-raw
      "
This function populates an opengl buffer with the 'data-byte-size' bytes of data
from the given point, optionally at the offset provided.

You also pass in the buffer type and the draw type this buffer is to be
used for.

The function returns a buffer object with its format slot populated with the
details of the data stored within the buffer")


  (defun bind-buffer
    "
Binds the given gpu-buffer to the target on the GL Context
")

  (defun unbind-buffer
      "
Unbinds any gpu-buffer currently bound to the GL Context
")

  (defmacro with-buffer
      "
This macro binds the given gpu-buffer to the target on the GL Context for the
duration of the body.

The gpu-buffer is then unbound at the end.
"))
