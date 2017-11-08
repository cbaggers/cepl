(in-package :cepl.streams)

(docs:define-docs
  (defstruct buffer-stream
    "
A buffer-stream is a structure that represents stream of gpu-data composed from
gpu-array and/or gpu-buffers.

To render in CEPL we map a buffer-stream over a gpu-pipeline. The buffer-stream
contains data (usually geometry) that is passed to the vertex-shader.

A buffer-stream composes various sources of gpu-data together. So if, for example,
the vertex shader took 3 arguments of types :vec2 :vec3 :float. Then you could
make a stream that composes 3 gpu-arrays with element-types :vec2 :vec3 :float
and map this over the pipeline.

Naturally, as we are talking about buffer-stream, only buffer-backed gpu-arrays
can be composed with buffer-streams.

Info for people used to OpenGL:
A buffer-stream is basically a VAO with some extra metadata such as the 'range'
of data to draw the 'style' of drawing & the 'primitive' the data represents .
")

  (defun buffer-stream-gpu-arrays
      "
When you construct a buffer-stream it will (by default) hold onto the
gpu-arrays that were composed.

This function when given a buffer-stream will return those stored gpu-arrays
")

  (defun buffer-stream-index-type
      "
When passed a buffer-stream with an index this function will return the
element-type of the index-data.
")

  (defun buffer-stream-length
      "
Returns the number of elements in the buffer-stream
")

  (defun buffer-stream-vao
      "
Returns the OpenGL Vertex Array Object (VAO) owned by this stream.

Do not modify this unless you are sure what you are doing.
In most cases you do not need to interact with the VAO directly
")

  (defun buffer-stream-p
      "
Returns t if the value passed is a buffer-stream
")

  (defun free-buffer-stream
      "
Calling this with a buffer-stream will free the VAO owned by this buffer-stream
and blank the buffer-stream object.

It will not free any of the gpu-data that was composed to make this stream.

Calling the generic function `free` with a buffer-stream will call this function")

  (defun make-buffer-stream
      "
This function composes a number of gpu-arrays into a buffer-stream.
No data is copied, the buffer-stream simply refers to the gpu-arrays so the
gpu knows where to pull data from.

You can optionally pass a gpu-array to act as an index into the other arrays.

Usually when you map over a buffer-stream it will call the pipeline pulling 1
value from each of the composed gpu-arrays on each 'iteration'[0].
When you have an index then map-g will pull 1 value from index each 'iteration'
and use that to pick which value to use from the other arrays. This approach
gives bigs boosts in performance and memory usage when rendering.

The element-type of the index-array must be of the following:
:uint8 :ushort :uint :unsigned-short :unsigned-int

As well as an element type you also can specify the primitive-type. This says
what the gpu will draw this data as. It can be any one of:

:points
:lines
:line-loop
:line-strip
:lines-adjacency
:line-strip-adjacency
:triangles
:triangle-fan
:triangle-strip
:triangles-adjacency
:triangle-strip-adjacency
(:patch <patch-size>)

By default the primitive-type is :triangles

It is also worth noting  that you can also use gpu-sub-arrays in here if you
want to limit the data you are using, for example the following is perfectly
legal code:

    (make-buffer-stream
      :gpu-arrays `(,(gpu-sub-array monster-pos-data 1000 2000)
                   ,(gpu-sub-array monster-col-data 1000 2000))
      :index-array monster-index-array
      :length 1000)

[0] The use of the term 'iteration' here is quoted as the gpu is going to be
    doing this work in parallel, however it makes the explanation clearer so
    that is why it is there. If you have a clearer way of explaining the
    behaviour please file it as an issue on github.
"))
