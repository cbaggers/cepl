#|| What a palaver

Ok so each option kind of sucks:

with our wip refactor we have proper lisp types we can use but we pay for the
conversions, which if the elements are structs containing arrays can be
expensive. Now this could be fine for push-g/pull-g but is pretty bad for map.

The current approach is nice in that we pay a low cost for getting a handle,
but we have a bunch of issues.
- make-* causes leaks (can be mitigated by using gc for those entries)
- conversion costs are paid on every access
- the lisp representation of the data is just nested lists...which is nasty

the refactor could benefit from with-g-struct which lets you access a foreign
element in the way we do now, however then we need to tell with-g-struct the
type so we are less dynamic that we are currently...is that true? well.. now at
least we dont have to track the element type so strictly... dont we? .. hmm.

one possibility is to have map-c take a pattern and, before iterating, fetch
the accessors.

There is also the question of array or c-array when accessing a slot

||#

#|| Ok, more tests done

The new major issue is with aref-c. If it returns a lisp struct then the
following doesnt work:

    (setf (g-pnt-pos (aref-c foo 0)) bar)

as you just modify the new struct. This means we would need to make aref-c
return a reference instead of a lisp struct...and then we are back in the
original damn situation we were before.

We could have a two wrapper types, one which has the pointer and the second
which inherits from the first but doesnt use the pointer... Nope, then you need
multiple versions of the accessor functions.

I think the answer is that this experiment has hit a dead end. We need to stick
with roughly what we have already and see how we can make it better.

||#
