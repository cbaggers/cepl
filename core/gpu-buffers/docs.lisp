(in-package :cepl.gpu-buffers)

(docs:define-docs

  (defstruct gpu-buffer
    "
gpu-buffer is a struct that abstracts a OpenGL 'Buffer Object'

Along with the the ID of the GL Object itself it stores the unformatted data
as an array of gpu-arrays.

Every gpu-array in the buffer will have an element-type of :uint8, even if this
buffer was created for a gpu-array with a different element-type.

For example (make-gpu-array '(.1 .2 .3 .4)) will make a gpu-array of 4 float.
However the buffer backing this gpu-array will contain a single array with
element-type :uint8 and a length of 16.

It is not expected that users will be using gpu-buffer's directly. Instead they
are ususal interacted with via CEPL's gpu-array and ubo features.
")

  (defun gpu-buffer-p
      "
This function returns t if the given value is a gpu-buffer. Otherwise it
returns nil.
")

  (defun gpu-buffer-arrays
      "
This function returns an array of the raw :uint8 gpu-arrays that make up the
data in this gpu-buffer.
")

  (defun gpu-buffer-id
      "
This function, when passed a gpu-buffer will return the OpenGL buffer object
from the gpu-buffer.

It is not recommended to modify this directly as the changes from doing so won't
be reflected in the layout of the gpu-buffer, which may potentially put the
gpu-buffer (and and gpu-array or ubo using it) into an invalid state.
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

  (defun buffer-data
  "
This function populates the gpu-buffer with the contents of the c-array.

You also pass in the buffer type and the draw type this buffer is to be used
for.

The function returns a buffer object with its format slot populated with the
details of the data stored within the buffer")

  (defun multi-buffer-data
      "
This function takes a list of c-arrays and uploads all of the data to the
gpu-buffer.
")

  (defun buffer-data-raw
      "
This function populates an opengl buffer with the 'data-byte-size' bytes of data
from the given point, optionally at the offset provided.

You also pass in the buffer type and the draw type this buffer is to be
used for.

The function returns a buffer object with its format slot populated with the
details of the data stored within the buffer")



  (defmacro with-buffer
      "
This macro binds the given gpu-buffer to the target on the GL Context for the
duration of the body.

The gpu-buffer is then unbound at the end.
"))
