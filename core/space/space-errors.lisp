(in-package :spaces)

(cgl::deferror not-ancestor () (start-space ancestor-space)
    "spaces:collect-inverse-to - ~s is not an ancestor of ~s"
  ancestor-space start-space)

(cgl::deferror position->no-space () (start-space)
    "Cepl Spaces has detected expression in your code where a position is
crossing from space ~s to a code block with no defined space.
This is invalid.

Positions are statically checked to ensure they are valid for the current
space they occupy. To pass this value out of this block unchanged use v! to
convert the position to a regular vector."
  start-space)
