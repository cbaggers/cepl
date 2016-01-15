# Types in Jungl

### Back to the Bytes

Types in Jungl are a lot more akin to the primitive types in languages like C, in that they have a one to one mapping to the byte layout in memory.

This is unlike the usual state of affairs in lisp where we let the implementation decide how memory is laid out.

### Where we use these

We use these types whenever we want to work with data-structures in C or GPU memory.

### How does these map to lisp types?

All the 'C' memory is being handled through cffi so all of the mappings are defined through that system.

### What's with the naming?

If you have already had a peek below you will have seen many keywords being used as names. This is a pattern used by cffi for it's core types and when we added the additional core graphics types (vec3, mat4 etc) it felt bad when they were plain symbols (as very quickly you get used to keyword meaning core type).

So the rule has become that that core types have optional keyword names and all users defined types have regular symbols names (just like cffi)

### Jungl's basic types

So without further ado, lets look at each type, the size in bytes and the lisp types it maps to.

name         size in bytes     component-type     lisp-type
:vec2        8                 :float             (simple-array single-float (2))
:vec3        12                :float             (simple-array single-float (2))
:vec4        16                :float             (simple-array single-float (2))
:ivec2       8                 :int               (simple-array  (2))
:ivec3       12                :int
:ivec4       16                :int
:uvec2       8                 :uint
:uvec3       12                :uint
:uvec4       16                :uint

:ubyte-vec2  2                 :ubyte
:ubyte-vec3  3                 :ubyte
:ubyte-vec4  4                 :ubyte
:byte-vec2   2                 :byte
:byte-vec3   3                 :byte
:byte-vec4   4                 :byte

:mat2        16                :float             (simple-array single-float (4))
:mat3        36                :float             (simple-array single-float (9))
:mat4        64                :float             (simple-array single-float (16))
