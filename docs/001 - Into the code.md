# Blam! Code

Rather than mess around with a lot more introduction let's just look at a simple code example and use it as an index to the first chapters of this documentation. Don't expect to understand this code straight away, it will take a little getting used to.

A bunch of lines have comments that look like this `[x] some topic` where `x` is some number, look below the code to find the relevent information for that comment.

Below we will do the standard OpenGL version of 'hello world' and draw a colored triangle.

```
(in-package :cepl)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)
(defparameter *triangle-data*
   (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
         (list (v!    0   0.5 0) (v! 1 0 0 1))
         (list (v! -0.5 -0.36 0) (v! 0 0 1 1))))

;; [1] defining a struct that works on gpu and cpu
(defstruct-g pos-col ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

;; [7] defining a function that works on the gpu
(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

;; [8] and another function
(defun-g frag ((color :vec4))
  color)

;; [9] composing those gpu functions into a pipeline
(defpipeline prog-1 ()
    (g-> #'vert #'frag))

;; here is what we do each frame
(defun step-demo ()
  (step-host) ;; [3]
  (update-repl-link) ;; [4]
  (clear) ;; [5]
  (map-g #'prog-1 *stream*) ;; [6]
  (swap)) ;; [10]

(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array *triangle-data* :element-type 'pos-col) ;; [0]
        *stream* (make-buffer-stream *array*)) ;; [2]
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))

```

Phew! that was a pile of stuff. Here is a quick summary of what will happen when you compile the code below and call `(run-loop)`:

#### First things

`#'run-loop` will set `*running*` to `t`. When `*running*` is set to `nil` we will stop the main loop

#### [0] Make the data for our triangle on the gpu

On this line we create and array on the gpu to hold the data for our triangle.

We use lisp data that was defined in `*triangle-data*` and the `pos-col` type defined in `[1]`

To read up on where data can live in Cepl see [chapter 002 - memory](./chapter 002 - memory.md)

`[1]` defstruct-g is a way of defining a struct type that can be used in `c-arrays` and `gpu-arrays`




- we make a buffer-stream from the the gpu-array. We will render by running our pipeline over the data in this stream
- we run the main loop which will call `#'step-demo` until `*running*` is `nil`
