#||

One things I really dislike with 'free' is that it's perfectly reasonable to
encode a number of things as functions. However due to it being a built-in type
we can only differentiate of what it is and not what it represents.

It may be a depper issue, dont specialize on any public, general purpose type.
or that may be bullshit

This is all essentially another flavor of global pollution.

It would be nice to make a subclass of function with not additional data or
functionality but with a mixin/interface/typeclass/whatever that indicates
it's meaning within a given context. Free would then be defined over the
mixins instead.

||#
