(in-package :cepl.gpu-arrays.buffer-backed)

(docs:define-docs
  (defstruct gpu-array
    "
GPU-ARRAY is a structure that respresents an array in gpu-memory.

OpenGL has 2 places it lets you store data on the gpu. Textures, which are covered in cepl.textures, and gpu-buffers

GPU Buffers are blocks of unformatted gpu memory. Most commonly filled ordered
data that is streamed into the rendering pipeline via the vertex shader, when
used in this way you will often hear it be called a vertex buffer object (vbo)
in opengl texts.

Given that

")

  (defun c-array-pointer
      "
C-ARRAY-POINTER takes a c-array as an argument and returns the
pointer to the foreign data.

You can also use the generic function #'POINTER to get the same result.
"))
