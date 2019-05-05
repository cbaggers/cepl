
#||

The cepl-context caches what it thinks the state currently is.
We have to restore state after calls to 3rd party gl code as we
make be in a block whos semantics are that the state is 'X' until
the end of the scope.

These are the CEPL primitives that make that promise

with-viewport (and thus :with-fbo-viewport)
with-vao-bound
with-fbo-bound
with-blending
with-transform-feedback
with-gpu-query-bound

with-gpu-array-as-pointer
with-gpu-array-as-c-array
with-gpu-array-range-as-pointer
with-gpu-array-range-as-c-array

The rest of the state that is cached is really just a performance thing,
we dont want to change state when we dont have too.

For all of those cases we just need to set the value in the cache to
something that doesnt match anything else so it is forced to apply the
next user supplied change.

This could be a win over our current approach as it requires less info from the
user. We will probably have the make the api such that we can return 'unknown'
from the functions that get the current state. We mustnt allow the user to pass
unknown however (really, could be useful..would let the user tell us 3rd party
stuff changed using the existing api).

||#
