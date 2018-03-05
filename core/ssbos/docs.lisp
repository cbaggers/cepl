(in-package :cepl.ssbos)

(docs:define-docs
  (defstruct ssbo
    "
`SSBO` is a structure that abstracts an OpenGL Shader Storage Buffer Object

An SSBO is chunk of data stored in a `gpu-buffer`, which can be used in shader
pipelines. They are very similar to UBOs and are used in pretty much the same
way. The big differences between them are:


- SSBOs can be much larger. The OpenGL spec guarantees that UBOs can
  be up to 16KB in size (implementations can allow them to be
  bigger). The spec guarantees that SSBOs can be up to 128MB. Most
  implementations will let you allocate a size up to the limit of GPU
  memory.

- SSBOs are writable (even atomically) whereas UBOs are not.
  Be aware that SSBOs reads and writes use incoherent memory accesses,
  so they need the appropriate barriers.

- SSBO access, all things being equal, will likely be slower than UBO
  access. SSBOs generally are accesses like buffer textures, while UBO
  data is accessed through internal shader-accessible memory reads. At
  the very least, UBOs will be no slower than SSBOs.

- *Not yet supported in CEPL*
  SSBOs can have variable storage, up to whatever buffer range was
  bound for that particular buffer; UBOs must have a specific, fixed
  storage size. This means that you can have an array of arbitrary
  length in an SSBO (at the end, rather). The actual size of the
  array, based on the range of the buffer bound, can be queried at
  runtime in the shader using the length function on the unbounded
  array variable.
")

  (defun make-ssbo
      "
This function will make and return a new `ssbo`

You need only provide an element type and the lisp data to populate that type.

For example, given the following type:

   (defstruct-g test
     (scale :float :accessor scale)
     (age :int))

You create a ssbo using this type as follows:

    (make-ssbo '(1.2 10) 'test)

You can also pass in a c-array or gpu-array and omit the optional type
specifier.

If you wish to make a ssbo from a specific element of a `c-array` or
`gpu-array` please see the #'make-ssbo-from-array function
")

  (defun make-ssbo-from-array
      "
This function takes a lisp-array, `c-array` or `gpu-array` and an index.
It returns a new `ssbo` with the specified element as the data of the ssbo.

If made from a c-array or lisp-array, a fresh gpu memory is allocated to hold
the data

If made from a gpu-array the ssbo simply holds a reference to the gpu-array
and the index. No new memory is allocated. This means that if that
gpu-array is destroyed then this ssbo is in an invalid state.

This also means it is possible to do invalid things when rendering. For example
say you used the ssbo as an input whilst streaming vertex data from the same
array. Consult GL documentation for details on such exceptional cases.
")

  (defun ssbo-id
      "
This function return the id of the GL SSBO abstracted by this CEPL `SSBO`.
")

  (defun ssbo-data
      "
Returns the `gpu-array` that contains the data presented by this `SSBO`
")

  (defun ssbo-data-type
      "
This function returns the type of the data that is contained in the `SSBO`
")

  (defun ssbo-index
      "
This function returns the index into the ssbo-data where the element defined by
this `ssbo` is located
")

  (defun ssbo-owns-gpu-array
      "
This function returns t if the `gpu-array` was created by `make-ssbo` or nil if it is
using data from a gpu-array passed to make-ssbo
"))
