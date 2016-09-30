(in-package :cepl.misc)

(docs:define-docs
  (defun draw-texture
      "
Takes a texture/sampler and optionally flags to say whether to call clear before
drawing and whether to swap afterwards.

The function will then draw the texture to the current viewport.


Note: As will all functions in the `cepl.misc` package these are not fast or
      in any way meant to be representative of good practice, they exist only
      because otherwise they get written a bunch of times to aid hacking around
      with stuff
")

  (defun draw-colored-quad
      "
Takes a color and optionally flags to say whether to call clear before drawing
and whether to swap afterwards.

The function will then draw the color to the current viewport.


Note: As will all functions in the `cepl.misc` package these are not fast or
      in any way meant to be representative of good practice, they exist only
      because otherwise they get written a bunch of times to aid hacking around
      with stuff
")
  )
