(in-package :cepl.ubos)

(docs:define-docs
  (defstruct ubo
    "
A UBO is a structure that abstracts an OpenGL Uniform Buffer Object

A Ubo is chunk of data stored in a gpu-buffer, which can be used as a uniform
in shader pipelines.

The advantage of using a UBO over just passing the data as regular unforms is
that UBOs can be used to share uniforms between different programs, as well as
quickly change between sets of uniforms for the same pipeline.

Let's expand on that a little:

Switching between uniform buffer bindings is typically faster than switching
dozens of uniforms in a pipeline.

Also, uniform buffer objects can typically store more data than non-buffered
uniforms. So they can be used to store and access larger blocks of data than
unbuffered uniform values.

Lastly, they can be used to share information between different programs.
So modifying a single buffer can effectively allow uniforms in multiple programs
to be updated.
")

  (defun make-ubo
      "
This function will make and return a new ubo

You need only provide an element type and the lisp data to populate that type.

For example, given the following type:

   (defstruct-g test
     (scale :float :accessor scale)
     (age :int))

You create a ubo using this type as follows:

    (make-ubo '(1.2 10) 'test)

If you wish to make a ubo from an element of a c-array or gpu-array please
see the #'make-ubo-from-array function
")

  (defun make-ubo-from-array
      "
This function takes a lisp-array, c-array or gpu-array and an index.
It returns a new ubo with the specified element as the data of the ubo.

If made from a c-array or lisp-array, a fresh gpu memory is allocated to hold
the data

If made from a gpu-array the ubo simply holds a reference to the gpu-array
and the index. No new memory is allocated. This means that if that
gpu-array is destroyed then this ubo is in an invalid state.
This also means it is possible to do invalid things when rendering. For example
say you used the ubo as an input whilst streaming vertex data from the same
array. Consult GL documentation for details on such exceptional cases.
")

  (defun ubo-id
      "
This function return the id of the GL UBO abstracted by this CEPL UBO.
")

  (defun ubo-data
      "
Returns the gpu-array that contains the data presented by this UBO
")

  (defun ubo-data-type
      "
This function returns the type of the data that is contained in the UBO
")

  (defun ubo-index
      "
This function returns the index into the ubo-data where the element defined by
this ubo is located
")

  (defun ubo-owns-gpu-array
      "
This function returns t if the gpu-array was created by make-ubo or nil if it is
using data from a gpu-array passed to make-ubo
"))
