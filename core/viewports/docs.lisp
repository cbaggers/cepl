(in-package :cepl.viewports)

(docs:define-docs
  (defstruct viewport
    "
Viewport is a structure that represents a retangular region of the window that
CEPL can draw into.

There is always a viewport current when rendering. If one of your own is not
current then CEPL will use its default viewport, the resolution of which is the
same as that of the window when it was created.

The default viewport can be modified, so feel free to update the resolution if
the window size changes.
")

  (defun make-viewport
      "
This function returns a new viewport with the specified resolution and origin
")

  (defun viewport-resolution
      "
This function returns the resolution of the viewport as a vec2

If you call the generic function #'resolution with a viewport you will get
this value.

If you need this value as a list use #'viewport-dimensions or just the
generic function #'dimensions
")

  (defun viewport-dimensions
            "
This function returns the resolution of the viewport as a list of integers

If you call the generic function #'dimensions with a viewport you will get
this value.

If you need this value as a vec2 use #'viewport-resolution or just the
generic function #'resolution
")

  (defun copy-viewport
      "
This function returns a new viewport with identical origin and resolution to the
viewport provided.
")

  (defun current-viewport
      "
This function returns the viewport that is current in this scope.

If no other code in the callstack has uses with-viewport or with-fbo-viewport
then the result will be the default viewport.
")

  (defun viewport-origin
      "
This function returns the origin of the viewport.

This is the top-left corner of the rectangle within the window that OpenGL (and
thus CEPL) will draw into
")

  (defun viewport-p
      "
This function will return t if the value given is a viewport, otherwise it will
return nil
")

  (defun viewport-params-to-vec4
      "
Will return a vec4 packed with the origin and resolution. The format is as
follows:

    (v! origin-x origin-y resolution-x resolution-y)

If now viewport is provided the the current viewport is used.
")

  (defun with-viewport
      "
This macro sets the current viewport to the viewport given as the argument.
It will remain as the current viewport until the end of the scope.
")

  (defun with-fbo-viewport
      "
This macro behaves similarly to with-viewport in that is sets the
current viewport. However rather than passing in a viewport, one is
created[0] based on the dimensions of the gpu-array bound as the specified
attachment to the given fbo.

By default color-attachment0 will be used but you can provide your own
attachment index using the :attachment argument.

It will remain as the current viewport until the end of the scope.

[0] CEPL is free to reuse internal viewport objects where it makes sense instead
    of consing up a new viewport. This means that the viewport inside the scope
    may be eq to the viewport outside, but with different values in its slots
    for the duration of the body.
")

  (defun viewport (obj)
    "This is a placeholder generic function for future apis.
Feel free to implement this in your own programs when yo want to get a viewport
from some container"))
