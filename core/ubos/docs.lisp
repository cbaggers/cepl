(in-package :cepl.ubos)

(docs:define-docs
  (defstruct ubo
    "
A UBO is a structure that abstracts an OpenGL Uniform Buffer Object

A Ubo is chunk of data stored in a gpu-buffer, which can be used as a uniform
in shader pipelines.

The advantage of using a UBO over just passing the data as regular unforms is
that UBOs can be used to share uniforms between different programs, as well as
quickly change between sets of uniforms for the same program object.

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
This
")

  (defun make-ubo-from-array
      "

")

  (defun ubo-id
      "

")

  (defun ubo-data
      "

")

  (defun ubo-data-type
      "

")

  (defun ubo-index
      "

")

  (defun ubo-owns-gpu-array
      "

"))
