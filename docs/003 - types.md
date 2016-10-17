# Types in CEPL

### Back to the Bytes

Types in CEPL have a one to one mapping to the byte layout in memory, much like the primitive types in languages such as C.  This is unlike the usual state of affairs in Lisp where we let the implementation decide how memory is laid out.

### Where we use these

We use these types to work with data-structures in C or GPU memory.

### How do these map to lisp types?

All the C memory is being handled through cffi, so all of the mappings are defined through that system.

### What's with the naming?

If you have already had a peek below, you will have seen many keywords being used as names. This is a pattern used by cffi for its in-built types, and when we added the additional core graphics types (vec3, mat4 etc), we continued using keywords for consistency.  You will quickly get used to keywords repersenting in-built types.

The rule is that core types have optional keyword names, and all user-defined types have regular symbols names, just like cffi.

### CEPL's basic types

So without further ado, lets look at each type, the size in bytes, and the lisp types it maps to.

| name        | bytes | component-type |    lisp-type |
|:------------|------:|:---------------|:-------------|
| :vec2       |  8    | :float         | (simple-array single-float (2)) |
| :vec3       |  12   | :float         | (simple-array single-float (2)) |
| :vec4       |  16   | :float         | (simple-array single-float (2)) |
| :ivec2      |  8    | :int           | (simple-array integer (2)) * |
| :ivec3      |  12   | :int           | (simple-array integer (2)) * |
| :ivec4      |  16   | :int           | (simple-array integer (2)) * |
| :uvec2      |  8    | :uint          | (simple-array unsigned-integer (2)) * |
| :uvec3      |  12   | :uint          | (simple-array unsigned-integer (2)) * |
| :uvec4      |  16   | :uint          | (simple-array unsigned-integer (2)) * |
| :uint8-vec2 |  2    | :uint8         | (simple-array (integer 0 255) (2)) * |
| :uint8-vec3 |  3    | :uint8         | (simple-array (integer 0 255) (2)) * |
| :uint8-vec4 |  4    | :uint8         | (simple-array (integer 0 255) (2)) * |
| :int8-vec2  |  2    | :int8          | (simple-array (integer -127 128) (2)) * |
| :int8-vec3  |  3    | :int8          | (simple-array (integer -127 128) (2)) * |
| :int8-vec4  |  4    | :int8          | (simple-array (integer -127 128) (2)) * |
| :mat2       | 16    | :float         | (simple-array single-float (4))  |
| :mat3       | 36    | :float         | (simple-array single-float (9)) |
| :mat4       | 64    | :float         | (simple-array single-float (16)) |

To be 100% accurate I have to say, that for all the types with `*` at the end, the type returned is actually implementation specific. But you get the idea :)

See the [cffi documentation for details](https://common-lisp.net/project/cffi/manual/cffi-manual.html#Built_002dIn-Types)

### Supported cffi types

Of course we also support some of the cffi types. For completeness here is a list:

| name |
|:-----|
| :short |
| :ushort (also called :unsigned-short ) |
| :int |
| :uint  (also called :unsigned-int) |
| :float |
| :double |

Soon we will also support `:int8` & `:uint8`

### No limits

Of course, if this was all we could use, it would be a bit limiting. Luckily, we can have arrays of this data as well as user defined structs (which is what we will look at in the next chapter).

### p.s

For the sake of brevity we will refer to *all the above types* PLUS *all cepl structs* PLUS *all cepl arrays of these types* using the phrase 'CEPL-compatible types'
