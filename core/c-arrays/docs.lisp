(in-package :cepl)

(docs:define-docs
  (defstruct c-array
    "
C-ARRAY is a structure that respresents an array in foreign memory.

cepl keeps not only the pointer to the foreign data in this structure
but also metadata that makes moving this data to (and from) the gpu or
lisp much easier.
")

  (defun c-array-pointer
      "
C-ARRAY-POINTER takes a c-array as an argument and returns the
pointer to the foreign data.

You can also use the generic function #'POINTER to get the same result.
")

  (defun c-array-dimensions
      "
C-ARRAY-DIMENSIONS takes a c-array as an argument and returns the
dimensions of the c-array

You can also use the generic function #'DIMENSIONS to get the same
result.
")

  (defun c-array-element-type
      "
C-ARRAY-POINTER takes a c-array as an argument and returns the
type of the elements of the array.

You can also use the generic function #'ELEMENT-TYPE to get the same
result.
")

  (defun aref-c
      "
Accesses the c-array element specified by the subscripts
")

  (defun aref-c*
      "
Accesses the c-array element specified by the subscripts.

The difference between this and #'aref-c is that this this function takes the
subscripts as a list.
"))
