(in-package :cepl.types)

(docs:define-docs
  (defmacro defstruct-g
      "
defstruct-g defines a struct that can be used both gpu-side (in gpu functions,
gpu-arrays, ubos, etc) and also cpu-side (in c-arrays, other gstructs, etc)

You create these using defstruct-g so lets look at an example right now:

    (defstruct-g our-data
      (position :vec3)
      (val :int :accessor val))

This should seem familiar if you have used common lisp's structs.

You provide a name (with options if you need them) and the definitions for the
slots. The format for a slot is:

    (slot-name slot-type)

    -or-

    (slot-name slot-type :accessor accessor-name)

-- make-* --

Once defined you would create the above struct as follows:

    (defvar x (make-our-data :position (v! 1 2 3) :val 5))

This will return a fully populated struct in foreign memory. if values are not
provided then the slots will be left uninitialized and the contents of the slot
are unknown (and likely unsafe)
It is rather rare to make a one-off struct like this as it is much more common
to use the type in a data-structure like a c-arrays or gpu-array.


-- Accessors --

In the 'our-data' example above, the slot named 'position' doesnt have an
accesor but the 'val' slot does.

Both slots will get a lisp-struct-style accessor functions however because of
the :accessor in val's slot definition a generic function named by the symbol
after :accessor will also be created.

For the example this means the functions #'our-data-position, #'our-data-val and
the generic function #'val are available.

Whilst :accessor results in a generic function on the cpu side (with the
associated overheads) on the gpu side the function to be used is resolved
statically and so there is not performance cost.


-- Populate --

defstruct-g also created an implentation for the generic function #'populate
which takes lisp lists and set the slots in a struct using that data.

For example for our 'our-data' struct we can say:

    (populate x (list (v! 1 2 3) 5))

And it will set 'position' to be the vector (v! 1 2 3) and set 'val' to be 5.


-- Options --

With the exception of :constructor the options for defstruct-g are rarely used
but are documented here for the sake of completeness.

  :constructor
  Setting this to nil means that you will get *no* make- function
  Setting this to any other symbol will name the constructor using that symbol
  The default will is that the constructor will be called make-<struct-name>

  :readers
  Setting this to nil means that you will get *no* functions to get the
  slots' data

  :writers
  Setting this to nil means that you will get *no* setf functions to set the
  slots' data

  :accesors
  Setting this to nil means that you will get *neither* of the above.

  :pull-push
  Setting this to nil means that you will get *no* push-g or pull-g methods
  defined for your type

  :attribs
  Setting this to nil means that defstruct-g will not be able to make
  gpu-streams from arrays of this type.

  :populate
  Setting this to nil means that you will not get a populate function for
  this type.


Some of the above options are redundent in combination with others.
For example the push-g method uses #'populate behind the scenes so with
populate disabled you can have #'push-g for this type.

CEPL currently does a poor job at communicating these conflicts to the user.

")

  (defun lisp-type->pixel-format
      "
This function, when given a lisp type name, will attempt to find and
return an equivalent pixel-format.

If no such type is found then nil is returned
")

  (defun image-format->lisp-type
      "
This function, when given a image-format name, will attempt to find and
return the name of a lisp type that is equivalent.

If no such type is found then nil is returned
")

  (defun image-format->pixel-format
      "
This function, when given an image-format name, will attempt to find and
return equivalent equivalent pixel-format.

If no such type is found then nil is returned
")

  (defun lisp-type->image-format
      "
This function, when given a lisp type name, will attempt to find and
return the name of a GL image-format that is equivalent.

If no such type is found then nil is returned
")

  (defun pixel-format->image-format
      "
This function, when given a pixel-format object, will attempt to find and
return the name of a GL image-format that is equivalent.

If no such type is found then nil is returned
")

  (defun pixel-format->lisp-type
      "
This function, when given a pixel-format object, will attempt to find and
return the name of a lisp type that is equivalent.

If no such type is found then nil is returned
")


  (defstruct g-pn
    "
g-pn is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm")

  (defstruct g-pc
    "
g-pc is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  color: of type vec4 with an accessor method: col")

  (defstruct g-pt
    "
g-pt is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  texture: of type vec2 with an accessor method: tex")

  (defstruct g-pnc
    "
g-pnc is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  color: of type vec4 with an accessor method: col")

  (defstruct g-pnt
    "
g-pnt is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  texture: of type vec2 with an accessor method: tex")

  (defstruct g-pntc
    "
g-pntc is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  texture: of type vec2 with an accessor method: tex
  color: of type vec4 with an accessor method: col")

  (defstruct g-pnb
    "
g-pnb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent")

  (defstruct g-pncb
    "
g-pncb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent
  color: of type vec4 with an accessor method: col")

  (defstruct g-pntb
    "
g-pntb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent
  texture: of type vec2 with an accessor method: tex")

  (defstruct g-pntcb
    "
g-pntcb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent
  texture: of type vec2 with an accessor method: tex
  color: of type vec4 with an accessor method: col"))
