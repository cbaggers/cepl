# Types in Jungl

### Back to the Bytes

Types in Jungl are a lot more akin to the primitive types in languages like C, in that they have a one to one mapping to the byte layout in memory.

This is unlike the usual state of affairs in lisp where we let the implementation decide how memory is laid out.

### Where we use these

We use these types whenever we want to work with data-structures in C or GPU memory.

### How does these map to lisp types?

All the 'C' memory is being handled through cffi so all of the mappings are defined through that system.

### What's with the naming?

If you have already had a peek below you will have seen many keywords being used as names. This is a pattern used by cffi for it's in-built types and when we added the additional core graphics types (vec3, mat4 etc) it felt bad when they were plain symbols (as very quickly you get used to keyword meaning in-built type).

So the rule has become that that core types have optional keyword names and all users defined types have regular symbols names (just like cffi)

### Jungl's basic types

So without further ado, lets look at each type, the size in bytes and the lisp types it maps to.

name         size in bytes     component-type     lisp-type
:vec2        8                 :float             (simple-array single-float (2))
:vec3        12                :float             (simple-array single-float (2))
:vec4        16                :float             (simple-array single-float (2))
:ivec2       8                 :int               (simple-array integer (2)) *
:ivec3       12                :int               (simple-array integer (2)) *
:ivec4       16                :int               (simple-array integer (2)) *
:uvec2       8                 :uint              (simple-array unsigned-integer (2)) *
:uvec3       12                :uint              (simple-array unsigned-integer (2)) *
:uvec4       16                :uint              (simple-array unsigned-integer (2)) *

:ubyte-vec2  2                 :ubyte             (simple-array (integer 0 255) (2)) *
:ubyte-vec3  3                 :ubyte             (simple-array (integer 0 255) (2)) *
:ubyte-vec4  4                 :ubyte             (simple-array (integer 0 255) (2)) *
:byte-vec2   2                 :byte              (simple-array (integer -127 128) (2)) *
:byte-vec3   3                 :byte              (simple-array (integer -127 128) (2)) *
:byte-vec4   4                 :byte              (simple-array (integer -127 128) (2)) *

:mat2        16                :float             (simple-array single-float (4))
:mat3        36                :float             (simple-array single-float (9))
:mat4        64                :float             (simple-array single-float (16))

To be 100% accurate I have to say, that for all the types with `*` at the end, the type returned is actually implementation specific. But practically you get the idea :)

See the [cffi documentation for details](https://common-lisp.net/project/cffi/manual/cffi-manual.html#Built_002dIn-Types)

### Supported cffi types
Of course we also support some of the cffi types. For completeness I'll list those here:

:short
:ushort (also called :unsigned-short )
:int
:uint  (also called :unsigned-int)
:float
:double

We will soon also support `:int8` & `:uint8`

### No no limits

Of course if this was all we could use it would be a bit limiting. Luckily we can have arrays of this data and also user defined structs, which is what we will look at in the next chapter.

### p.s

For the sake of brevity we will refer to *all the above types* PLUS *all jungl structs* PLUS *all jungl arrays of these types* using the phrase 'jungl compatible types'
