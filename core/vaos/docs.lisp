(in-package :cepl.vaos)

(docs:define-docs
  (defmacro with-vao-bound
            "
Binds the vao to the gl context and guarentees it will be unbound after the
body scope.

Usually you will not need to interact with the vao directly as you can simply
use a buffer-stream and let map-g handle when it should be bound and unbound.
")

  (defun free-vao
      "
Takes a GL VAO and deletes it
")

  (defun free-vaos
      "
Takes a list of GL VAOs and deletes them.

This function exists as a minor optimization.
")

  (defun make-vao
      "
This function returns a new GL VAO when given

- a list of gpu-arrays
- optionally 1 gpu-array to be used as an index
")

  (defun make-vao-from-id
      "
This function takes an existing OpenGL VAO and set it up to strema vertices from
the given arrays.
"))
