# Buffers

Most people can safely skip this chapter. It is only relevant if you really want to understand how CEPL allocates `buffer objects`.

This chapter will be more of an api-reference than a guide; if you want to use this you already know what you are doing.

`buffers.lisp` contains all the code for making `OpenGL`'s `buffer object`s and uploading data to those buffers.


### Data Layout

A buffer object is just a chunk of the GPU's `buffer memory`, with a particular memory layout.

Buffer layouts are defined as lists of the format:
```lisp
(data-type data-index-length offset-in-bytes-into-buffer)
```

So a layout for a buffer containing 3 `:floats` and 140 instances of our `our-data` struct type looks like this:
```lisp
`((:float 3 0) ('vert-data 140 12))
```

With this knowledge we can now start working with buffers:

### Making Buffers
```lisp
(make-gpu-buffer (&key initial-contents
                   (buffer-target :array-buffer)
                   (usage :static-draw)
                   (managed nil)))
```
Creates a new OpenGL `buffer object`.

You can optionally provide a `c-array` as the `:initial-contents` to have the buffer populated with the contents of the array.

Usage in this case is the same as in `gpu-arrays`

`Managed` is a flag used by CEPL to indicate that a gpu-array owns the buffer. When true, freeing the `gpu-array` that uses this buffer, frees the buffer the same time.

### Reserve Space
```lisp
(buffer-reserve-block (buffer type dimensions buffer-target usage))
```
This function creates an empty block of data in the OpenGL buffer, large enough to contain `(apply #'* dimensions)` elements of the specified type.

It will remove ALL data currently in the buffer

### Uploading Data
```lisp
(buffer-data (buffer c-array buffer-target usage
               &key (offset 0) (size (c-array-byte-size c-array))))
```
This function populates an OpenGL buffer with the contents of the array. It requires the buffer target and the usage indicating what this buffer is to be used for.

The function returns a buffer object with its format slot populated with the details of the data stored within the buffer.
```lisp
(multi-buffer-data (buffer c-arrays buffer-target usage))
```
This beast will take a list of c-arrays and auto-magically push them into a buffer, taking care of both interleaving and sequencial data, and handling all the offsets.
```lisp
(buffer-sub-data (buffer c-array byte-offset buffer-target
                   &key (safe t)))
```
This function replaces a subsection of the data in the specified buffer with the data in the `c-array`.

The `byte-offset` parameter specifies where you wish to start overwriting data from.

When the `:safe` option is `t`, the function checks to see if the data you are about to write into the buffer will cross the boundaries between data already in the buffer, and will emit an error if necessary.

### Free buffer

Buffers can be freed with `#'free` or `#'free-buffer`
