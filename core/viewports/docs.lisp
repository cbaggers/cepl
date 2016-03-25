(in-package :cepl.viewports)

(docs:define-docs
  (defstruct viewport
    "
Viewport is a structure that represents a retangular region of the window that
CEPL can draw into.

There is always a viewport current when rendering. If one of your own is not
current then CEPL will use it's default viewport, the resolution of which is the
same as that of the window when it was created.

The default viewport can be modified, so feel free to update the resolution if
the window size changes.
")

  (defun make-viewport
      "
This function returns a new viewport with the specified resolution and origin
")

  (defun viewport-resolution))
