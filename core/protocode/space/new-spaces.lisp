
(in clip-space
    (in model-space (sv! pos 1.0)))

(convert-from-space
 clip-space
 (let (*current-space* clip-space)
   (convert-from-space
    model-space
    (let (*current-space* model-space)
      (sv! pos 1.0)))))

(def-v-type-class v-in-space (v-compile-time-value)
  (space))

#||

Hmm..do we want the svec to be a ctv?..not really. It's the space that should be
the ctv

||#

(def-v-type-class v-space (v-compile-time-value)
  (uniform-name))


#||

let's look at 'in' again

||#

(in clip-space
    (in model-space
        (progn
          (sv! pos 1.0))))

#||

At the 'in' boundary we need to know the space that (sv! pos 1.0) was created
in.

That's a bit tricky. I this case we kind of want a ctv which doesnt remove
itself from the glsl. We still want all the computation at the various places
but we need to know the spaces..arse..too much wine. I can't think now

sleep

- - -

Hey again! the space of (sv! pos 1.0) is easy, it has to be model-space as
that's the whole point of 'in' :p

The first problem arises here:

||#

(in some-space
    (let ((x (sv! pos 1.0)))
      (in clip-space
          (in model-space
              (+ (sv! pos2 1.0) x)))))

#||

What space was x in? In this case we could tag the type with the space.

we don't have dynamic scope in shaders so functions should be fine.

you can't mutate a var from a surrounding scope

you cant pass closures, so no way to move a function's call-site outside of the
'in' scope.

So the way to cause a problem is with args. Can we pass a svec into a function

One would hope so. But then does it lose it's space? it would be a shame if it
did. So then maybe we do the same as with other ctvs? make a custom func? Well,
it cant be the same as varjo strips the args from the signature.


-----

Walk up tree, split env, if you passed a conditional or loop on way up then ctv
of thing is 'or' of current thing/s and new thing.

be able to tag env as conditional/loop.

-----

I was worried that I would need to have special code for handling loops as I do
for flow-ids. Luckily that's not the case. The reason for flow-ids needing it is
that new flow-ids can be created and assigned out of the loop, this means you
have to keep tracing until you have it all.

This is due to wanting to know the flow of all things, not just where compile
time stuff goes.

||#


(let ((x (lambda ((a :int)) a))
      (z #'foo))
  (while (not-done)
    (setq x z)
    (setq z (random 10))))
