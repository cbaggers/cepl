
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

||#
