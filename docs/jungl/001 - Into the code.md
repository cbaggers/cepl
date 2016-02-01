# Blam! Code

Rather than mess around with a lot more introduction let's just look at a simple code example and use it as an index to the rest of this documentation.

Many lines have comments that look like this `[x] some topic` where `x` is some number, look below the code to find the relevent information for that comment.

The following is a very silly way of drawing a triangle. It is not usually thing complicated! See [the example in cepl.examples here]() for a see a more normal version of a GL *hello-world*.

Below we will draw the triangle into an fbo in the first pass and then on the second pass draw the triangle to the screen with inverted colors

```
(in-package :cepl)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g pos-col ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

(defun-g frag ((color :vec4))
  color)

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'prog-1 *stream*)
  (update-display))

(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
                                      (list (v!    0   0.5 0) (v! 1 0 0 1))
                                      (list (v! -0.5 -0.36 0) (v! 0 0 1 1)))
                                :element-type 'pos-col)
        *stream* (make-buffer-stream *array*))
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))

```
