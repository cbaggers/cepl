(in-package :cepl.pipelines)

(docs:define-docs
  (defmacro defun-g
      "
Defun-g let's you define a function which will be run on the gpu.

Defun-g is rather similar to the standard defun, but there are differences:

- Type: All you aruments must have their types declared

- No &optional or &key: Instead we have &uniform and &context

Let's see a simple example of a gpu function we can then break down

    (defun-g vert ((vert pos-col))
      (values (v! (pos vert) 1.0)
	      (col vert)))
")

  (defun undefine-gpu-function
      "

")

  (defmacro def-glsl-stage
      "

")

  (defmacro defmacro-g
      "

")

  (defmacro define-compiler-macro-g
      "

")

  (defmacro defpipeline
      "

")

  (defmacro with-instances
      "

")

  (defmacro g->
      "

")

  (defmacro map-g
      "

"))
