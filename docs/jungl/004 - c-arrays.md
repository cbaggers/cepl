# C Arrays

Let me take a second before getting into our scheduled programming to praise the cffi:

Common Lisp's cffi is amazing, to be able to get some much done as a newbie really solidified my love for CL and cffi (and libraries using it) have been the corner stone of this entire project. To all the developers involved, You Rock.

### Right, back to the snooker:

So when dealing with graphics we are very often dealing with large arrays of information, information like the vectices of our meshes or positions of lights. Cffi allows us to allocate arrays of different types, but in jungl we also want to attach extra metadata that will be used behind the scenes.

To this end jungl has its own c-array type that uses cffi behind the scenes.

That was all a bit technical so lets get into how to use them.

```
	 ;; 0.
	 (make-c-array nil :dimensions 100 :element-type :float)

	 ;; 1.
	 (make-c-array nil :dimensions 100 :element-type 'our-data)

	 ;; 2.
	 (make-c-array '(1.0 2.0 3.0 4.0) :element-type :float)

	 ;; 3.
	 (make-c-array `((,(v! 1 2 3) 10) (,(v! 4 5 6) 20))
                   :element-type 'our-data)

	 ;; 4.
     (make-c-array '(1 2 3.0 4))

	 ;; 5.
	 (make-c-array #2A((1 2) (3 4)))
```

We will go through these 1 by 1 and talk about what is going on:

*0:*
In example `0` we make a c-array of 100 floats with *no initial-contents*.

As you probably guessed, that first argument is where you can provide data to be used as the initial contents. By not providing it we are leaving those values un-initialized, so the normal rules apply (it may be full of garbage)

*1:*
This is simply to show that we can use our structs from the previous chapter in c-arrays.

*2:*
This little example shows that when we provide the `initial-contents` we can leave out the dimensions. The dimensions will be made the same dimensions as that of the data provided.

*3:*
Number 3 shows that you can even provide lisp data when the `element-type` is a jungl struct. In this case it will use `#'populate` (which we talked about in the last chapter) to fill in the values.

Here is the definition of that struct again:

```
	 (defstruct-g our-data ()
	   (position :vec3)
	   (val :int :accessor val))
```

So in this case we end up with a c-array with two elements of type `our-data`, the first struct has the `position` `(v! 1 2 3)` and the `val` `10`; the second struct has the `position` `(v! 4 5 6)` and the `val` 20.

*4:*
Hey now this is odd, we only provide the lisp data, how does jungl know what to do?

What happens is that `#'make-c-array` scans each element of the lisp data and tries to find the *smallest jungl compatible type* that will hold all the values. In this case because of the number `3.0` in there the array has to have `element-type` `:float`.

Now this feature is very handy (especially in the repl) but there are some caveats.

- It can only infer a few types:
  `:ubyte` `:byte` `:int` `:float` & `:double`

- Scanning for types is not fast:
  This is a great feature to use at the repl, because odds are jungl can work out the type fast enough that you won't notice a delay. **However** this is not a good type to use in performance critical code, so if you need the speed, always specify your `element-type`

*5:*
This shows two things:

First: that c-arrays can take arrays (of multiple dimensions also)
Second: The `element-type` that jungl will give this array is `:ubyte`. The reason is that all the elements are between 0 & 255.

If we were to write:

```
	 (make-c-array #2A((1 -2) (3 4)))
```

The the `element-type` would be `:byte`.


### More `#'make-c-array` args

The signature for `#'make-c-array` is as follows

```
 (initial-contents
  &key dimensions
       element-type
	   displaced-by
	   (alignment 1))
```

we have already seen `initial-contents`, `dimensions` & `element-type` but what of the other two?

**displaced-by:**
This behave similarly to the same argument in CL's `#'make-array`. It lets you make an array sharing data with another array and potentially changing the dimensions at the same time.

Be super careful with this. Both c-arrays share the same memory but there is no GC. So if you free the data behind one, you free the data for the other too.

**alignment:**
Alignment is mean to byte align the data in your array but it is currently buggy. Don't use this yet :)


### Getting and Setting

There's not point having an array we can access so let's do that now.

In CL we normally use `(aref some-array subscripts ..)` to get an element from the array and `(setf (aref some-array subscripts ..) val)` to set an element.

In jungl we use `(aref-c some-array subscripts ..)` to get an element and `(setf (aref-c some-array subscripts ..) val)` to set an element.

Mind bending stuff! :p
