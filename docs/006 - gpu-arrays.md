# GPU Arrays

Finally, we are making something on the gpu, surely this is a monster of a task...

well no.

Actually the basics work exactly the same and in c-array

Let's do some comparisons of the usage of `#'make-c-array` and `#'make-gpu-array`

```
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

Just like with `c-array` there are two extra arguments, one of which is alignment which works the same way as in `c-array` (which means right now it doesnt :p), and then there is `access-style`

But wait, `#'make-gpu-array` also has another trick. It can take a `c-array` as the first argument and upload that data to the gpu-array. This is really fast as the is no type conversion to be done before upload. Also it's cool as you dont need to provide any extra information as `c-array`s contain all the metadata `make-gpu-array` needs to work.


### Access-Style

This argument is a optimization hint that is given to opengl to say how you expect to be accessing this data.

This bit it pretty technical so if you dont know what you need, just let cepl use the default.

This enables the OpenGL implementation to make more intelligent decisions that may significantly impact gpu-array performance. It does not, however, constrain the actual usage of the array. `Access-Style` can be broken down into two parts: first, the frequency of access (modification and usage), and second, the nature of that access.

The frequency of access may be one of these:

> :STREAM - The data store contents will be modified once and used at most a few times.
> :STATIC - The data store contents will be modified once and used many times.
> :DYNAMIC - The data store contents will be modified repeatedly and used many times.

The nature of access may be one of these:

> :DRAW - The data store contents are modified by the application, and used as the source for GL drawing and image specification commands.
> :READ - The data store contents are modified by reading data from the GL, and used to return that data when queried by the application.
> :COPY - The data store contents are modified by reading data from the GL, and used as the source for GL drawing and image specification commands.

So combined we get these choices:

> :STREAM_DRAW :STREAM_READ :STREAM_COPY
> :STATIC_DRAW :STATIC_READ :STATIC_COPY
> :DYNAMIC_DRAW :DYNAMIC_READ :DYNAMIC_COPY.

### So we have #'aref-g, right?

Well no actually. Whilst we could implement it in 2 minutes it would be a bad idea. The reason for this is that communicating with the gpu is costly, especially if you are in the middle of rendering something. By giving a complimentary usage to `aref-c` it may feel like we are saying that the access costs are similar, and that would be **very** misleading.

Instead we have ways to push large chunks of data to the array (which is how things are usually done) and for the times you absolutely need random access to a gpu-array we have the `with-gpu-array-as-c-array` macro.

It takes a gpu-array and maps it as a c-array which allows you to run any of the c-array commands on it.

A simple example would be if we wanted to set the 3rd element in a gpu array to 5.0 we could do the following:

```
	(with-gpu-array-as-c-array (mygpuarray)
	  (setf (aref-c mygpuarray 2) 5.0))
```

Very cool!

When you do this OpenGL does some magic to map the memory containing the gpu-array to the local address space. This let's cepl get a pointer to it and thus be able to present a c-array to you.

Of course don't try and be sneaky and copy this c-array somewhere, at the end of the `with-gpu-array-as-c-array`'s scope the gpu memory is

The valid values for access are `:read-only` `:write-only` & `:read-write`

*Performance Note:*

I've taken this from the OpenGL wiki and modified it to use cepl's terminology:

> Mappings to the data stores of `gpu-arrays` may have nonstandard performance characteristics. For example, such mappings may be marked as uncacheable regions of memory, and in such cases reading from them may be very slow. To ensure optimal performance, the client should use the mapping in a fashion consistent with the values of `:access-style` for the `gpu-array` and of access. Using a mapping in a fashion inconsistent with these values is liable to be multiple orders of magnitude slower than using normal memory.

So again, don't assume you will get good performance, this is not how you should be interacting with your gpu-arrays normally.

### Structs and gpu-arrays

One nice thing that cepl does for you if you use our structs is to *interleave* your struct data.

Imagine you have a model of a goat with has 3000 vertices, and each vertex of the goat has a position, uv coordinates and a normal vector. One way to store it would be:

`[ the 1000 positions | the 1000 uv coordinates | the 1000 normal vectors ]`

And this would work, but it means that when the gpu want to render the first vertex, it has to jump to 3 places in memory to get the data. That's gonna hurt performance.

Instead if we make an struct like this:

```
	 (defstruct-g goat-vertex ()
	   (position :vec3)
	   (uv :vec2)
	   (normal :vec3))
```

and make the array like this:

```
     (make-gpu-array nil :dimensions 1000 :element-type 'goat-vertex)
```

The the data is laid out like this

```
     [first pos | first uv | first normal | second pos | second uv | second normal | .. etc .. ]
```

This really helps the gpu, and cepl makes it easy, do it :)


### subseq-g

`#'subseq-g` is a just like `#'subseq-c` except that it takes a `gpu-array` and give you a new one which shares a subset of the memory of the original. Like with `#'subseq-c` the data is shared so you have to be super careful not to mangle or free the data.


### What is this 'Backed-By' stuff?

If you are playing along at home you may have tried something like this:

```
	 CEPL> (make-gpu-array '(1 2 3))
```

and got back an object like this

```
	 #<GPU-ARRAY :element-type :UBYTE :dimensions (3) :backed-by :BUFFER>
```

..which seems fine, except for that `:backed-by` thing, what is going on there?

Well what you are seeing is information about what kind of gpu memory your data is being stored in.

GPUs are made for games, the whole game industry drives the GPU industry and so GPU engineers work damn hard to be able to support the kinds of techniques game developers are trying to achieve. However much you optimize your code, you can just never beat having custom hardware so when you see big patterns in how data on the gpu is used, you will want to customize the hardware to support it.

> Note: This is simplified of course, the game/gpu industries are a mad cycle which feeds eachother, this kind of system (or echo chamber) can end up with some odd results, like hardware being optimized for AAA games like crysis. It's all a bit weird an not a topic I'm gonna get into.

So two big patterns we notice in data usage are how we read vertex data and how we read texture data. By having two different pools with different access styles they can optimize the hell out of it. Ι don't want to get too deep so let's get back to `:backed-by`.

OpenGL provides textures (which we will get into later) and 'Buffer Objects' which we will refer to as `buffers` from now on.

`buffers` allow you to allocate a block of `buffer memory` and then upload data there. There is no real limit of what you can stick up there, but there are *effective limits* as there only certain things you can *do* with the data once it's there.

So whilst we could just expose these buffers (and we do, see chapter [006]("./006 - Buffers.md")) in cepl we choose to also expose objects that map more directly to what you do with this data.

`Gpu-arrays` are one case of this. All your 3d models you render will have their vertex data stored sequentially in a buffer object, this data has a length and has an definite layour of the 'elements'..this is pretty much the definition for a kind of array.

As we will soon see, the `gpu-array` abstraction is also useful with textures, so we need a way to show you whether this gpu-array is `backed-by` a `buffer memory` or `texture memory`.


### Multiple gpu-arrays in the same buffer

There are valids reason to want certain gpu-arrays to live in the same `buffer object`.

To do this use the `make-gpu-arrays` function

This is the signature: `(c-arrays &key (access-style :static-draw))`

This function creates a list of gpu-arrays residing in a single buffer in opengl. It create one gpu-array for each c-array in the list passed in.

The `:access-style` is shared by all gpu-arrays in the `buffer object` but is otherwise the same as defined earlier.


### Freeing gpu-arrays

You can use either `#'free` or `#'free-gpu-array` to free gpu arrays.
