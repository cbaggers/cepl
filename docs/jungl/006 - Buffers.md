# Buffers

Most people can safely skip this chapter. It is only relevent if you really want to understand how jungl allocates `buffer objects`.

`buffers.lisp` is where we have all the code for making `OpenGL`'s `buffer object`s and for uploading data to those buffers.

A buffer object is just a chunk of the GPU's `buffer memory` but it's not like you are going to just be storing random data up there, so naturally you are going to have some kind of layout.
