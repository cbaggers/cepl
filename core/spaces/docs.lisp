(in-package :cepl.space)

(docs:define-docs
  (defstruct vec-space
    "
This type is an abstraction over the concept of a vector-space.

In graphics programming we are very used to using matrices to describe
transforms. The matrix describes the transform for any given point in
vector-space to a point in another vector-space.

Some spaces are very commonly mentioned when discuss rendering, spaces like
world-space, camera-space, clip-space, etc. However when it comes to our
programs we have to work with the transform (matrix) rather the space directly.

In CEPL we have a feature to allow working with vector spaces as a first class
object, the vec-space. We uses these vec-spaces in our GPU code and CEPL
statically analyses our code to work out what matrices are needed and uploads
them as uniforms.

By doing this, our GPU ends up running code that only has the 'normal'
types (matrices and vectors) which means we pay no performace cost for the
features

Working with vec-spaces directly means our code more closely reflects how we
talk about the problems we are trying to solve. As we will see below it also
makes it much harder to perform 'invalid' spatial calculations.


-- Lexical Space --

In many graphics related texts we talk about performing a calculation in a
certain space.

In CEPL we express this with the 'in' macro

    (in *world-space*
      (some-calculation)
      (more-calculations))

Everything that is happening in this scope is happening 'in *world-space*'

the 'in' macro can be nested:

    (in *clip-space*
      (let ((z (in *world-space*
		 (let ((x (sv! 0 1 2 3)))
		   ...
		   x))))
	z))

Here z is in *clip-space* and x is in *world-space*


-- Spatial Vectors --

In the above example we also introduce another type: spatial-vectors

sv! behave exactly the same as v! in that it produces a vector, however
instead of making a regular vector the code above will make a spatial-vector.

Spatial vectors are vectors which store which space they were created in.

The advantage of this is that CEPL can detect if they leave that space and enter
a different space. When that happens CEPL will automatically transform the
vector to the new space.

We can see this in the example above. The value in 'x' is a spatial vector that
was created in world space, however as the value moves up the stack to be
stored in 'z' it leaves the scope of *world-space* and enters *clip-space*.
At this point CEPL will multiply the vector with a
'world-space-to-clip-space matrix'.

This is awesome as it means that if you are working with spatial vectors in the
same scope, you are guarenteed that they are in the same space.

Another advantage is that in most case CEPL can calculate these transform
matrices on the CPU and upload them as uniforms, which makes this code very
fast.


-- Defining Spaces --

Currently vec-spaces can only be defined in cpu code. They must be passed to
gpu functions as uniforms.

All spaces are defined relative to another space. A vec-space is either a
hierarchical-space, relational-space or a model-space.

A hierarchical-space is a space which is a child of another space. When the
parent space is transformed, the child space is also transformed. You can
see this relationship as how your hand has a position and orientation but will
be transformed by the parent, the lower-arm. The lower arm in turn is a child
of the upper arm, and so on. Hierarchical spaces are WIP and not currently
exposed in CEPL

A relational-space x is a space whose relationship to another space can be
defined with a matrix, but which is not affected by changes to that other space.

A relational-space can have any number of spaces as neighbours, however
in the case where there is only one neighbour space, the space is known as a
model-space.

Making a model space is very easy.

    (make-space neighbour-space
		transform-to-neighbour-matrix4
		transform-from-neighbour-matrix4)

the transform-from-neighbour-matrix4 transform is optional where the inverse can
be calculated.

To make a relational space you use #'make-space* and pass in a list where the
sublists are the same as the arguments to make-space


-- Spaces & Implicit-Uniforms

If the vec-space is stored in a variable with global scope then CEPL's
implicit-uniforms feature means you can use the space without explicitly adding
the uniform argument yourself.

See the docstring for defun-g for more details on implicit-uniforms.


-- Immutable Relationships --

Whilst the transforms between neighbouring spaces are mutable.
It is not possible to add new neighbours to an existing vec-space x, except by
making a new vec-space as specifying x as its neighbour.


-- Predefined Spaces --

CEPL defines a few commonly used spaces for you. Other than *world-space* they
are all spaces used in the GLSL pipeline:

 *world-space*
 *clip-space*
 *ndc-space*
 *screen-space*

")

  (defstruct svec4
    "
This is a 4 component spatial vector type. A spatial vector is a vector that
also stores which vector-space it is in.
")

  (defun make-space
      "
This function returns a new vec-space when passed:

- The space which this new space will be defined in relation to

- A mat4 which describes the transform from this new space to the target-space

- (optional) A mat4 which describes the transform from the target-space to this
             new space.

As the new vec-space will have only 1 neighbour it will be a model-space
")

  (defun make-space*
      "
This function returns a new vec-space when passed as in the following format:

    (list (neighbour-space1 mat4-to-neighbour-space1 mat4-from-neighbour-space1)
          (neighbour-space2 mat4-to-neighbour-space2 mat4-from-neighbour-space2)
          ... etc ...)

Just like in #'make-space, the mat4-from-neighbour-space transform is optional.

As this function produces a vec-space with multiple neighbour spaces, the
resulting vec-space is known as a relational-space.
")

  (defun parent-space
      "
When given a hierarchical vec-space, this function will return the space that
has this vec-space as a child
")

  (defun model-space-p
      "
When passed a vec-space this function will return t if the vec-space is a
'model-space'

In CEPL this means that this vec-space on has one non-hierarchical relationship
with another space.
")

  (defun relational-space-p
      "
When passed a vec-space this function will return t if the vec-space is a
'relational-space'

In CEPL this means that this vec-space is a child of another vec-space.
")

  (defun get-transform
      "
This function will return the mat4 that describes the transform between
from-space and to-space.

If you have an existing matrix you wish to use as the base of the transform then
you case pass it as the initial-m4 argument
")

  (defun get-transform-via
      "
This function will return the mat4 that describes the transform between
from-space and to-space, via the specified via-space.

This is very useful in cases where there are multiple possible routes to the
to-space and you wish to restrict which route is taken.
")

  (defun with-space-routing-via
      "
This macro ensures that all routes calculated between spaces go via the specifed
vec-space.

This behaves as if all the calls to #'get-transform were actually called to
#'get-transform-via with the given vec-space used as the via-space.
")

  (defmacro in
      "
This macro is only valid inside gpu functions.

It defines a scope which is said to be 'in the vec-space' given as the first
argument. This means that any spatial-vectors created in this scope will be
declared in this space.

If the value that is returned from the scope of the 'in' is a spatial-vector
then the vector is transformed from the space of the inner scope to the space of
the outer scope.

For example:

    (in *clip-space*
      (let ((z (in *world-space*
		   (let ((x (sv! 0 1 2 3)))
		     ...
		     x))))
	z))

In the code above the spatial-vector x is defined in *world-space*. However
as it is used in the tail position of the innermost let, it will leave the
*world-space* scope and enter the *clip-space* scope. At that point the
spatial-vector will be multiplied with the mat4 that describes the
*world-space* → *clip-space* transform.

All of this information is resolved statically at compile-time by CEPL. The 'in'
forms are removed and the spatial-vectors are replaced with regular ones. CEPL
will calculate the mat4 transforms between the used spaces on the CPU and upload
them as uniforms.

This means that the final glsl will only contain standard glsl vectors and
matrices and will perform as well as any other handwritten glsl code.
")

  (defvar *screen-space*
      "
This is the vec-space that describes screen-space in CEPL.

It could also be thought of as FBO space when you are not rendering into the
default framebuffer object.
")

  (defvar *ndc-space*
      "
This is the vec-space that describes ndc-space in CEPL.
")

  (defvar *clip-space*
      "
This is the vec-space that describes clip-space in CEPL.
")

  (defvar *world-space*
      "
This is the vec-space that describes world-space in CEPL.

Whilst it is perfectly possible to use CEPL's vec-space feature to define your
own world-space, we strongly suggest you do not as CEPL has some specific
optimizations it can do you you use *world-space* instead of your own.
")

  (defun sv!
      "
This function is only useful inside gpu functions.

It returns a new spatial vector. A spatial vector is much like a regular vector
except that the vector-space in which it resides is stored along with the
vector's components.

If a spatial vector leaves the lexical scope of the vec-space it was defined in,
then it is automatically transformed into the vec-space of the surrounding
scope.

See the docstring for the 'in' macro for more details.

For more info on the behaviour of CEPL's space feature as a whole please see the
docstring for the vec-space struct.
"))
