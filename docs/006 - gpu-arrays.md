# GPU Arrays

Finally, we are making something on the GPU... Surely this is a monster of a task... Well, no.  Actually the basics work exactly the same as in `c-array`.

Let's do some comparisons of the usage of `#'make-c-array` and `#'make-gpu-array`
```lisp
;; 0.
(make-c-array nil :dimensions 100 :element-type :float)
(make-gpu-array nil :dimensions 100 :element-type :float)

;; 1.
(make-c-array nil :dimensions 100 :element-type 'our-data)
(make-gpu-array nil :dimensions 100 :element-type 'our-data)

;; 2.
(make-c-array '(1.0 2.0 3.0 4.0) :element-type :float)
(make-gpu-array '(1.0 2.0 3.0 4.0) :element-type :float)

;; 3.
(make-c-array `((,(v! 1 2 3) 10) (,(v! 4 5 6) 20))
              :element-type 'our-data)
(make-gpu-array `((,(v! 1 2 3) 10) (,(v! 4 5 6) 20))
                 :element-type 'our-data)

;; 4.
(make-c-array '(1 2 3.0 4))
(make-gpu-array '(1 2 3.0 4))

;; 5.
(make-c-array #2A((1 2) (3 4)))
(make-gpu-array #2A((1 2) (3 4)))
```

Pretty cool!

Just like with `c-array` there are two extra arguments:

- `alignment`, which works the same way as in `c-array` (which means right now it doesnt :p);
- `access-style` is discussed in the next section.

But wait, `#'make-gpu-array` also has another trick. It can take a `c-array` as the first argument, and upload that data to the gpu-array. This is really fast as the is no type conversion to be done before the upload. Also it's cool as you dont need to provide any extra information -- `c-array`s contain all the metadata that `make-gpu-array` needs.

### Access-Style

This argument is a optimization hint that is given to OpenGL to specify how you expect to access data. This bit it pretty technical; if you dont know what you need, just let CEPL use the default.

The reason to use `access-style` hint is that it enables the OpenGL implementation to make more intelligent decisions and possibly  significantly improve GPU performance. It does not, however, constrain the actual usage of the array. 

`Access-Style` is a compound of two words joined with an underscore, :

- prefix, the frequency of access (modification and usage)
- suffix the nature of that access.

The frequency of access may be one of these:

| prefix | Expected datastore access pattern |
|:--------|:----------------------------------|
| STREAM  | Modified once and read occasionally |
| STATIC  | Modified once and read many times |
| DYNAMIC | Modified repeatedly and read many times |

The nature of access may be one of these:

| suffix | Expected datastore access nature |
|:--------|:----------------------------------|
| DRAW | Modified by the application, used as the source for GL drawing and image specification commands |
| READ | Modified by reading data from the GL, used to return that data when queried by the application |
| COPY | Modified by reading data from the GL, used as the source for GL drawing and image specification commands |

So, when combined, we get these nine possible keywords:

|   |   |   |
|---|---|---|
| :STREAM_DRAW | :STREAM_READ | :STREAM_COPY |
| :STATIC_DRAW | :STATIC_READ | :STATIC_COPY |
| :DYNAMIC_DRAW | :DYNAMIC_READ | :DYNAMIC_COPY |

### So we have #'aref-g, right?

Well no actually. While we could implement it in 2 minutes it, would be a *bad idea*. The reason for this is that communicating with the gpu is costly, especially in the middle of rendering. Providing `aref-g` might imply that the access costs are similar to `aref` or `aref-c`, which would be **very misleading**.

Instead, we have ways to explicitly move large chunks of data to and from the array (which is how things are usually done). For the times you absolutely need random access to a `gpu-array`, we have the `with-gpu-array-as-c-array` macro. It maps a `gpu-array` to a `c-array`, which allows you to perform any of the `c-array` operations on it.

A simple example is setting the 3rd element in a `gpu-array` to 5.0:
```lisp
(with-gpu-array-as-c-array (mygpuarray)
  (setf (aref-c mygpuarray 2) 5.0))
```

Very cool! OpenGL does some magic to map the memory containing the `gpu-array` to the local address space. CEPL gets a pointer to it and thus is able to present a `c-array` to you.

Of course, don't try to be sneaky and use this c-array outside the scope of `with-gpu-array-as-c-array`.

The valid values for access are `:read-only`, `:write-only` and `:read-write`

*Performance Note:*

I've taken this from the OpenGL wiki and modified it to use CEPL's terminology:

> Mappings to the data stores of `gpu-arrays` may have nonstandard performance characteristics. For example, such mappings may be marked as uncacheable regions of memory, and in such cases reading from them may be very slow. To ensure optimal performance, the client should use the mapping in a fashion consistent with the values of `:access-style` for the `gpu-array` and of access. Using a mapping in a fashion inconsistent with these values is liable to be **multiple orders of magnitude slower** than using normal memory.

So again, don't assume you will get good performance; this is not how you should be interacting with your `gpu-array`s normally.

### Structs and gpu-arrays

One nice benefit of using CEPL structs is the *interleaving* of your struct data.

Imagine you have a model of a goat with 3000 vertices. Each vertex has a position, uv coordinates, and a normal vector. One way to store it would be:

`[ the 1000 positions | the 1000 uv coordinates | the 1000 normal vectors ]`

This would work, but it means that when the GPU renders the first vertex, it has to jump to 3 places in memory to get the data. That's gonna hurt performance.

Instead, if we make an struct like this:
```lisp
 (defstruct-g goat-vertex
   (position :vec3)
   (uv :vec2)
   (normal :vec3))
```
and make the array like this:
```lisp
     (make-gpu-array nil :dimensions 1000 :element-type 'goat-vertex)
```
The the data is automatically laid out like this
```
     [first pos | first uv | first normal | second pos | second uv | second normal | .. etc .. ]
```

This really helps the gpu, and CEPL makes it easy; do it :)


### subseq-g

`#'subseq-g` is a just like `#'subseq-c`: it takes a `gpu-array` and gives you a new `gpu-array` which shares a subset of the GPU memory of the original. Like with `#'subseq-c` **the data is shared** so you have to be super careful not to mangle or free the data.


### What is this 'Backed-By' stuff?

If you are playing along at home you may have tried something like this:
```lisp
	 CEPL> (make-gpu-array '(1 2 3))
```
and got back an object like this
```
	 #<GPU-ARRAY :element-type :UBYTE :dimensions (3) :backed-by :BUFFER>
```
..which seems fine, except for that `:backed-by` thing. What is going on there?

Well, what you are seeing is information about what kind of GPU memory your data is being stored in.  GPUs are made for games; the whole game industry drives the GPU industry, and GPU engineers work damn hard to be able to support the kinds of techniques game developers are using. However much you optimize your code, you just can't beat custom hardware, so when there are obvious patterns of how data on the gpu is used, the hardware is customized to support it.

> Note: This is simplified, of course. The game/gpu industries are in a mad cycle which feeds one another. This kind of resonant systems (think of an echo chamber) can end up with some odd results, such as hardware being optimized for AAA games like `Crysis`. It's all a bit weird and not a topic I'm going to get into.

So two big patterns in GPU data usage are how we read vertex data and how we read texture data. Having two different pools with different access styles allows GPU designers to optimize the hell out of it. Ι don't want to get too deep so let's get back to `:backed-by`.

OpenGL provides textures (which we will get into later) and 'Buffer Objects' which we will refer to as `buffers` from now on. 
`buffers` allow you to allocate a block of `buffer memory` and then upload data there. There is no real limit of what you can stick up there, but there are *effective limits* as there only certain things you can *do* with the data once it's there.

So while we could just expose these buffers (and we do, see chapter [007]("./007 - Buffers.md")) in CEPL, we choose to also expose objects that map more directly to what you do with this data.  `gpu-arrays` are one case of this. All 3d models have their vertex data stored sequentially in a buffer object; this data has a length and a definite layout of the 'elements'. This is pretty much the definition for a kind of array.

As we will soon see, the `gpu-array` abstraction is also useful with textures, so we need a way to show you whether this `gpu-array` is `backed-by` a `buffer memory` or `texture memory`.

### Multiple gpu-arrays in the same buffer

There are valid reasons to place certain `gpu-array`s in the same `buffer object`. To do this use the `make-gpu-arrays` function:
```lisp
(make-gpu-arrays (c-arrays &key (access-style :static-draw)))`
```
This function creates a list of gpu-arrays residing in a single buffer in opengl. It create one gpu-array for each c-array in the list passed in.

The `:access-style` is shared by all gpu-arrays in the `buffer object`, but otherwise is the same as defined earlier.

### Freeing gpu-arrays

You can use either `#'free` or `#'free-gpu-array` to free GPU arrays.
