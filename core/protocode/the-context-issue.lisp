#||

Yo,

Ok so forever we have hated the need for the context object as it
takes control away from the user and feel 'enginey'. However it's use
has been unavoidable as otherwise any macro in the form (with* ...)
was binding and unbinding on every entrance and exit regardless of
previous state which ate ass for performance.

We do want people to have the option to use this mechanism as it does
alleviate a bunch of care that otherwise has to be taken even when you
are just trying to play with something.

What we need is to expose all the state modifying functions directly,
and have them always modify the state unless they are passed a
context. It's up to the user to make the context and keep it
populated.

In this version we would have no scoped macros, it's all deliberate
binding & unbinding function calls.

We probably still want the implicit default case so that people can
not think by default, just wrap with-cepl-context up high in the stack
and yay shit works

This seems to suggest having `if`s in every function though to check
if a context is bound, which is ass. I wonder if we can avoid that in
some sensible way.  We coudl have them explicitly set content to nil
and use a compiler-macro to elide the context check.

It's not great though. Maybe managed and unmanaged packages?
The concern is that people mix the code.. they could do that now with
cl-opengl of course. Hmm.

I wonder if we could have some value in the context slots which means
'this shit is disabled dude' and then if someone does some macro that
expects tracking it knows that. The fallback would be to do something
slower but correct, like always bind and unbind.

This feels like some kind of 'unknown' flag which might be useful in
general. Although it does add yet more conditionals to the context
related code which is blech.

||#
