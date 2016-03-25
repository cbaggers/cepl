(in-package :cepl.gpu-arrays.buffer-backed)

(docs:define-docs
    (defstruct gpu-array-bb
    "
")

  (defun c-array-pointer
      "
C-ARRAY-POINTER takes a c-array as an argument and returns the
pointer to the foreign data.

You can also use the generic function #'POINTER to get the same result.
"))
