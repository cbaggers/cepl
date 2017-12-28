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
