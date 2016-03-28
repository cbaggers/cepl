# User defined structs

One of the really tricky parts of working with gpu data is laying it out in memory (and a lot of realted details that make my brain melt). Cepl simplifies this by providing a kind of struct that can be used both on the gpu and cpu.

### Defining

You create these using defstruct-g so lets look at an example right now:

```
     (defstruct-g our-data
       (position :vec3)
       (val :int :accessor val))
```

This should seem familiar if you have used common lisp's structs.

You provide a name, options (if you need them) and the definitions for the slots. The slots are mostly what we are interested in, so let's look at them

The format for a slot is

```
     (slot-name slot-type)

     -or-

     (slot-name slot-type :accessor accessor-name)
```

### Gimme one!

The first example above makes a slot of the given type (which must be a cepl compatible type). Like regular lisp structs, the way you make an instance of this our `our-data` type is using the `make-our-data` function passing in values using the keyword arguments. So for example:

```
     (defvar x (make-our-data :position (v! 1 2 3) :val 5))
```

Will return a fully populated struct **in c memory**.

For those who havent seen it yet, the `v!` is used to make vectors. So here we are making a vector3

Notice that we are passing lisp data into this make function and cepl is transparently translating it to *c data*. More on this later.

Remember that, like in C, not providing values for the slots leaves the field undefined. The value in the slot will be garbage and trying to retrieve it may crash cepl.

### Accessors

Notice that, in our `our-data` example, the slot named `position` doesnt have an accesor but the `val` slot does. What is going on here?

Well both slots will get lisp-struct-style accessor functions, so you can use `(our-data-position x)` and `(our-data-val x)` to get the values in the slots.

However because of the `:accessor` in `val`s slot definition you can also use the method `(val x)` to get the value of that slot, cool huh?

#### Performance niggle

Because the result of using `:accessor` is a method there will be a performance difference between `#'our-data-val` and `#'val` when called on the cpu.

This may not be a problem for your project but do it keep it in mind.

Also note that I said `on the cpu`. As we are going to see, our shaders are statically typed so there is *zero* performace penatly for using `#'val` on the gpu (so do it!).

### Options

Let's look at another example struct:

```
     (defstruct-g (our-data :writers nil
                            ..
                            ..)
       ...
       ...)
```

we see that this time the name is inside a list along with one or more options. This is a lot like how we can give options to regular lisp structs.

These options are rather technical, and are not likely to be of interest to most people.

Ok with that out of the way let's have a look at them.

**:constructor**
Setting this to nil means that you will get *no* `make-` function
Setting this to any other symbol will name the constructor using that symbol
The default will is that the constructor will be called `make-<struct-name>`

**:readers**
Setting this to nil means that you will get *no* functions to get the slots data

**:writers**
Setting this to nil means that you will get *no* setf functions to set the slots data

**:accesors**
Setting this to nil means that you will get *neither* of the above.

**:pull-push**
Setting this to nil means that you will get *no* `push-g` or `pull-g` methods defined for your type

**:attribs**
Setting this to nil means that `defstruct-g` will not be able to make `gpu streams` from arrays of this type. This may not mean much until after the `gpu streams` chapter.

**:populate**
Setting this to nil means that you will not get a `populate` function for this type.


Some of the above options are redundent in combination with others. For example the `push-g` method uses `#'populate` behind the scenes so with `populate` disabled you can have `#'push-g` for this type. Cepl needs to do a better job at communicating these conflicts to the user.
