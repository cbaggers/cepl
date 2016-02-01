# Buffers

Most people can safely skip this chapter. It is only relevent if you really want to understand how jungl allocates `buffer objects`.

This chapter will be more of an api-reference than a guide, if you want to use this then you already know what you are doing

`buffers.lisp` is where we have all the code for making `OpenGL`'s `buffer object`s and for uploading data to those buffers.


### Data Layout

A buffer object is just a chunk of the GPU's `buffer memory` but it's not like you are going to just be storing random data up there, so naturally you are going to have some kind of layout.

Buffer layouts are defined as lists of the format:

```
     (data-type data-index-length offset-in-bytes-into-buffer)
```

So a layout for a buffer containing 3 `:floats` and 140 instances of our `our-data` struct type looks like this:

```
     `((:float 3 0) ('vert-data 140 12))
```

With this knowledge we can now start doing things with buffers

### Making Buffers

`#'make-buffer`:

```
     Signature: (&key initial-contents
                      (buffer-target :array-buffer)
                      (usage :static-draw)
                      (managed nil))
```

Creates a new opengl `buffer object`.

You can optionally provide a `c-array` as the `:initial-contents` to have the buffer populated with the contents of the array.

Usage in this case is the same as in `gpu-arrays`

`Managed` is a flag used by jungl to indicate that a gpu-array owns the buffer. When this is true, and you free the `gpu-array` that uses this buffer, then the buffer is freed at the same time.

### Reserve Space

`#'buffer-reserve-block`

```
     Signature: (buffer type dimensions buffer-target usage)
```

This function creates an empty block of data in the opengl buffer large enough to contain `(apply #'* dimensions)` elements of the specified type

It will remove ALL data currently in the buffer

`#'buffer-reserve-blocks`

```
     Signature: (buffer types-and-dimensions buffer-target usage)
```
This function creates an empty block of data in the opengl buffer large enoug to contain all the elements of types defines in types-and-dimensions.

types-and-dimensions should be of the format: `((type length) (type length) ...etc)`

It will remove ALL data currently in the buffer"


### Uploading Data

`#'buffer-data`

```
     Signature: (buffer c-array buffer-target usage
                 &key (offset 0) (size (c-array-byte-size c-array))
```

This function populates an opengl buffer with the contents of the array. You also pass in the buffer target and the draw type this buffer is to be used for.

The function returns a buffer object with its format slot populated with the details of the data stored within the buffer.


`#'multi-buffer-data`

```
     Signature: (buffer c-arrays buffer-target usage)
```

This beast will take a list of c-arrays and auto-magically push them into a buffer taking care of both interleaving and sequencial data and handling all the offsets.


`#'buffer-sub-data`
```
     Signature: (buffer c-array byte-offset buffer-target
                 &key (safe t))
```

This function replaces a subsection of the data in the specified buffer with the data in the c-array.

The byte offset specifies where you wish to start overwriting data from.

When the :safe option is t, the function checks to see if the data you are about to write into the buffer will cross the boundaries between data already in the buffer and will emit an error if you are.

### Bind a buffer

`with-buffer macro`

```
     Signature: ((var-name buffer &optional (buffer-target :array-buffer)) &body body
```

Example

```
	 (with-buffer (x some-buffer)
	   (buffer-sub-data x ...other args...))
```

### Free buffer

Buffers can be freed with `#'free` or `#'free-buffer`
