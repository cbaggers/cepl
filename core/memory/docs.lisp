(in-package :cepl.memory)


(docs:define-docs
  (defgeneric free
      "
The function takes a CEPL object of any one of the following types.

- `c-array`
- `gpu-buffer`
- `gpu-array` (texture backed or buffer backed)
- `buffer-stream`
- `texture`
- `ubo`
- `fbo`

And deletes the internal GL object, frees any associated memory and blanks the
CEPL object.

'Blanking', in this case, means the fields of the object will be set to some
default.
")

  (defgeneric push-g
      "
This function takes a CEPL object as a desination and some data to push to the
desination.

It will then upload that data to the destination doing any data conversions
that are required on the way.

CEPL types supported as destinations are:
- `c-array`
- `gpu-array` (both texture-backed and buffer-backed)
- `texture`
- `ubo`
- Any instance of a type defined using `defstruct-g`
")

  (defgeneric pull-g
      "
This function takes a CEPL object containing foreign or gpu data as pulls the
data into lisp performing any data conversions that are required on the way.

CEPL types that can be pulled from are:
- `c-array`
- `gpu-array` (both texture-backed and buffer-backed)
- `texture`
- `ubo`
- Any instance of a type defined using `defstruct-g`
")

  (defgeneric pull1-g
      "
This function is a varient of `pull-g` which, rather than pulling the gpu or
foreign data to lisp, will instead pull it 1 level closer to lisp.

What is meant by that is that, if the data is in foreign memory then it will be
pulled to lisp data (just like `pull-g`). However if you call `pull1-g` on data
stored on the gpu, then the data will be pulled to foreign memory instead.

To clarify:

    (pull1-g c-array) -> lisp list
    (pull1-g gpu-array) -> c-array

You can use pull1-g on the following CEPL types.
- `c-array`
- `gpu-array` (both texture-backed and buffer-backed)
- `texture`
- `ubo`
- Any instance of a type defined using `defstruct-g`

")

  (defun initialized-p
      "
When given an object holding a gpu resource this function will return whether
that object has been initialized.

This will be t unless the GL context has not yet been created.
"))
