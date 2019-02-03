#||

Once one of these objects has at least one handle associated with it,
the object's state immediately becomes immutable. No functions that
modify anything about the texture will work. This includes the Sampler
Object used in texture+sampler handles.

Furthermore, there is no way to undo this immutability. Once you get a
handle that is associated with that object, it is permanently frozen.

Note that you can still create View Textures from textures with
handles. Similarly, views of a texture that uses a handle are still
mutable (or at least, as mutable as an Immutable Storage Texture can
be). Also, you can update the contents of the storage for such
textures; just not their state.

||#

#|| Things to keep in mind:

- When converting a handle into a sampler/image variable, the type of
  sampler/image must match with the handle.

- The integer values used with handles must be actual handles returned
  by the handle APIs, and those handles must be resident. So you can't
  perform "pointer arithmetic" or anything of the like on them; treat
  them as opaque values that happen to be 64-bit unsigned integers.
||#
