# C Arrays

Let me take a second before getting into our scheduled programming to praise the CFFI.  Common Lisp's CFFI is amazing! To be able to get so much done as a newbie really solidified my love for CL.  CFFI and libraries using it have been the cornerstone of this entire project. To all the developers involved, You Rock.

### Right, back to the snooker:

When dealing with graphics we are very often working with large arrays of information, information like the vertices of our meshes or positions of lights. CFFI allows us to allocate arrays of different types, but in CEPL we also want to attach extra metadata that will be used behind the scenes.  To this end CEPL has its own c-array type that uses CFFI behind the scenes.

In this document, the terms "C arrays" and "c-array" refer to the special CFFI arrays that are managed by CEPL.

That was all a bit technical so lets get into how to use CEPL arrays.
```lisp
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

We will go through these one by one and talk about what is going on:

**0:** In example `0` we make a c-array of 100 floats with *no initial-contents*.

As you probably guessed, that first `nil` argument is where you can provide data to be used as the initial contents. By not providing it we, are leaving those values uninitialized, so the normal rules apply (it may be full of garbage)

**1:**
This is simply to show that we can use our structs from the previous chapter in c-arrays.

**2:**
This little example shows that when we provide the `initial-contents`, we can leave out the dimensions. The dimensions will be made the same as those of the data provided.

**3:**
You can even provide Lisp data when the `element-type` is a CEPL struct. In this case, CEPL will take each element of the list in order to fill in the slots of the struct.  Here is the definition of that struct again:

```
     (defstruct-g our-data ()
       (position :vec3)
       (val :int :accessor val))
```

In this case we end up with a c-array with two elements of type `our-data`. The first struct has the `position` `(v! 1 2 3)` and the `val` `10`; the second struct has the `position` `(v! 4 5 6)` and the `val` 20.

**4:**
Hey now, this is odd; we only provide the lisp data. How does CEPL know what to do?  Here `#'make-c-array` scans each element of the Lisp data and tries to find the *smallest cepl compatible type* that will hold all the values. In this case, because of the number `3.0`, the array has to have `element-type` `:float`.

Now this feature is very handy (especially in the repl) but there are some caveats.

- It can only infer a few types:
  `:uint8` `:int8` `:int` `:float` & `:double`

- Scanning for types is not fast:
  This is a great feature to use at the REPL, because odds are CEPL can work out the type fast enough that you won't notice a delay. **However** -- this is not good in performance-critical code, so if you need the speed, always specify your `element-type`.

**5:**
This demonstrates two points:

 - c-arrays can be initialized with Lisp arrays (of multiple dimensions)
 - The `element-type` that CEPL will give this array is `:uint8`. The reason is that all the elements are between 0 & 255.

If we were to write:
```
     (make-c-array #2A((1 -2) (3 4)))
```
the resulting `element-type` would be `:int8`.


### More `#'make-c-array` args

The signature for `#'make-c-array` is as follows:
```
 (initial-contents
  &key dimensions
       element-type
       displaced-by
       (alignment 1))
```

We have already seen `initial-contents`, `dimensions`, and `element-type`; what of the other two?

**displaced-by:**
Just like in CL's `#'make-array` you can share data with another array and potentially change the dimensions at the same time. Be super careful with this. Both c-arrays share the same memory but there is no GC. So if you free the data behind one, you free the data for the other as well.

**alignment:**
Alignment is meant to byte-align the data in your array but it is currently buggy. Don't use this yet :)


### Getting and Setting

There's not point having an array we can't access so let's do that now.  In CL we normally use `(aref some-array subscripts ..)` to get an element from the array and `(setf (aref some-array subscripts ..) val)` to set an element.

In CEPL we use `(aref-c some-array subscripts ..)` to get an element and `(setf (aref-c some-array subscripts ..) val)` to set an element.

Mind bending stuff! :p

### subseq-c

`#'subseq-c` is a cool function. It gives you a c-array, the contents of which are a subset of another c-array. This is very like CL's regular `subseq` function **except for one very important detail**.

`#'subseq-c` **does not** copy the data, which means changes in the first array affect the second (and vice versa)!

### Freeing

To free a c-array we can call `#'free` or `#'free-c-array`. The latter, of course, is the more optimized code path.
