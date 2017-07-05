(in-package :cepl.c-arrays)

(docs:define-docs
  (defstruct c-array
    "
`C-ARRAY` is a structure that represents an array in foreign memory.

CEPL keeps not only the pointer to the foreign data in this structure
but also metadata that makes moving this data to (and from) the gpu or
lisp much easier.
")

  (defun c-array-pointer
      "
`C-ARRAY-POINTER` takes a `c-array` as an argument and returns the
pointer to the foreign data.

You can also use the generic function `POINTER` to get the same result.
")

  (defun c-array-dimensions
      "
`C-ARRAY-DIMENSIONS` takes a `c-array` as an argument and returns the
dimensions of the c-array

You can also use the generic function `DIMENSIONS` to get the same
result.
")

  (defun c-array-element-type
      "
`C-ARRAY-POINTER` takes a `c-array` as an argument and returns the
type of the elements of the array.

You can also use the generic function `ELEMENT-TYPE` to get the same
result.
")

  (defun aref-c
      "
Accesses the `c-array` element specified by the subscripts
")

  (defun aref-c*
      "
Accesses the `c-array` element specified by the subscripts.

The difference between this and `aref-c` is that this this function takes the
subscripts as a list.
")

  (defun c-array-p
      "
Return t if the argument is a `c-array`. Returns nil otherwise.
")

  (defun clone-c-array
      "
Takes a `c-array` and makes a new c-array with the same contents as the the
original. The contents in foreign memory are copied.
")

  (defun free-c-array
      "
Frees the foreign memory allocated with the `c-array` and 'blanks' the c-array.

Blanking the c-array means its fields will be set to default values,
for example dimensions will be 0, the pointer will be null etc.

The generic function `free` will call `free-c-array` when passed a c-array.
")

  (defun make-c-array
      "
This function will make and return a new `c-array`.

It can be used in a few different ways:

- with :initial-contents to nil:
  In this case you need to provide dimensions and an element-type.

- with :initial-contents populated.
  The initial-contents can be a (potentially nested) list or array.

When :initial-contents is an array then the dimension of the c-array
will be the same as the array passed in. CEPL currently only supports up
to 4D c-arrays.

When the :initial-contents is a flat list then each element is used as one
element in the c-array.
If the :initial-contents is a nested list then you must either:

- specify multiple dimensions and an element-type
- specify an element-type to be some struct type, then nested lists are then
  used to populate the fields of the foreign structs. For an example of this
  please see [this example](https://github.com/cbaggers/cepl.examples/blob/master/examples/triangle.lisp#L30).

If the :element-type is not provided then CEPL will look at every element in
the initial-contents and try and find the smallest (in bytes) foreign type
which works for every element. This mean if the array is full of single-floats
then CEPL will choose :float, not :double.
Naturally this behaviour is too slow for use in performance critical
applications however it is nice for experimentation and working from the repl.

If you need what would be called a displaced array in lisp then please see the
`subseq-c` function.
")

  (defun make-c-array-from-pointer
      "
Will create a CEPL `c-array` with the element-type and dimensions specified,
and will store the pointer as where the data is expected to be.

This function does allocate the memory or validate the type or dimensions so be
very careful when using this function.
")

  (defun subseq-c
      "
This function returns a `c-array` which contains a subset of the array passed into
this function.

It does not copy the foreign data, instead this array points to within the data
of the original array. This means these arrays now share data (like a displaced
array in standard CL.

Due to this you have to be very careful when freeing the underlying array as
this will affect any other array sharing that data.

If you want a copy of a subseq of a c-array then use something like:

    (clone-c-array (subseq-c arr 3 10))

The reason that this arguably more dangerous behaviour is default is efficiency.
CEPL tries not to allocate new memory when the function is not explicitly about
that.")

  (defgeneric pointer
      "
Returns the pointer to the start of the foreign data that makes up the array
")

  (defun with-c-array-freed
      "
Binds the `c-array` to the variable named by the var-name argument.
Frees the c-array at the end of the scope.
")

  (defun with-c-arrays-freed
      "
Binds a list of `c-array`s to the variable named by the var-name argument.
Frees all of the c-arrays at the end of the scope.
")

  (defun element-byte-size
      "
Returns the size in bytes taken up by a single element of the `c-array`.
")

  (defun element-type
      "
Returns the type of the elements in the `c-array`.
")

  (defun across-c-ptr
      "
This function takes two arguments:

- A function that takes a pointer and a (unsigned-byte 32)
- A `c-array`

`across-c-ptr` will then call the given function once for every element in the
c-array passing in the pointer to an element of the array and the index to that
element.
")

  (defun map-c-into
      "
When given a destination `c-array`, a function and a source c-array this function
will map the function across every element of the source c-array and write the
results of the function destrucively into the destination c-array.
")

  (defun map-c
      "
When given a function and a `c-array` this function will map the function across
every element of the c-array and write the results of the function into a new
c-array with the same element-type and dimensions as the original c-array.

You may also pass in an optional foreign type that will be used as the element
type of the new array.
")

  (defun ptr-index
      "
This function takes a `c-array` and some subscripts and will return the ptr to the
specified element of the c-array
"))
