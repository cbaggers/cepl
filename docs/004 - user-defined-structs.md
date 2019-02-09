# User defined structs

Laying out GPU data in memory is one of the really tricky parts of working with GPU data. CEPL simplifies this by providing a special struct that can be used both on the GPU and CPU.

### Defining

`defstruct-g` is used to create these special structs, so lets look at an example right now:
```lisp
     (defstruct-g our-data
       (position :vec3)
       (val :int :accessor val))
```

This should seem familiar if you have used Common Lisp's structs.  You provide a name, options (if you need them), and the definitions for the slots. The slots are mostly what we are interested in, so let's look at them.

The format for a slot is:
```lisp
     (slot-name slot-type)

     -or-

     (slot-name slot-type :accessor accessor-name)
```

### Gimme one!

The first example above makes a slot of the given type (which must be a CEPL-compatible type). As with regular Lisp structs, to make an instance of `our-data` type use the `make-our-data` function, passing in values with the keyword arguments. For example:
```lisp
     (defvar x (make-our-data :position (v! 1 2 3) :val 5))
```

will return a fully populated struct **in c memory**.

For those who havent seen it yet, the `v!` syntax is used to make vectors. Here we are making a vector3.

Notice that we are passing Lisp data into the `make` function, and CEPL is transparently translating it to *c data*. More on this later.

Remember that, like in C, not providing values for the slots leaves the field undefined. The value in the slot will be garbage, and trying to retrieve it may crash CEPL.

### Accessors

In our `our-data` example, the slot named `position` doesn't have an accesor, but the `val` slot does. What is going on here?

Well, both slots will get lisp-struct-style accessor functions, so you can use `(our-data-position x)` and `(our-data-val x)` to get the slot values.

However, because of the `:accessor` in `val`s slot definition, you can also use the method `(val x)` to get the value of that slot. Cool, huh?

#### Performance niggle

Because the result of using `:accessor` is a method, there will be a performance difference between `#'our-data-val` and `#'val` when called on the cpu.  This may not be a problem for your project, but do it keep it in mind.

Also note that I said "on the cpu". As we shall see, our shaders are statically typed so there is *zero* performace penalty for using `#'val` on the gpu (so do it!).

### Options

Let's look at another example struct:
```lisp
     (defstruct-g (our-data :writers nil
                            ..
                            ..)
       ...
       ...)
```

This time the name is inside a list along with one or more options. This is much like providing options to regular Lisp structs. These options are rather technical, and are not likely to be of interest to most people.  OK with that out of the way, let's have a look at these options:

**:constructor**

 - nil means that you will get *no* `make-` function
 - any other symbol will name the constructor using that symbol
 - by default the constructor will be called `make-<struct-name>`

**:readers**

 - nil means that you will get *no* functions to get the slots data

**:writers**

 - nil means that you will get *no* setf functions to set the slots data

**:accessors**

 - nil means that you will get *neither* of the above.

**:pull-push**

 - nil means that you will get *no* `push-g` or `pull-g` methods defined for your type

**:attribs**

 - nil means that `defstruct-g` will not be able to make `gpu streams` from arrays of this type. This may not mean much until after the `gpu streams` chapter.

**:populate**

- nil means that you will not get a `populate` function for this type.


Some of the above options are redundant in combination with others. For example, the `push-g` method uses `#'populate` behind the scenes, so with `populate` disabled, no `#'push-g` is created for this type. CEPL needs to do a better job at communicating these conflicts to the user.
