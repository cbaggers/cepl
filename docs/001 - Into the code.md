# Blam! Code

Rather than mess around with a lot more introductions let's just look at a simple code example. Don't expect to understand the details right away; it will take a little getting used to.

Below is the standard OpenGL version of 'hello world', a colored triangle.

```
(in-package :cepl)
;;;; cepl-learn.lisp

(in-package #:cepl-learn)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

;; Lisp data for triangle vertices
(defparameter *triangle-data*
   (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
         (list (v!    0   0.5 0) (v! 1 0 0 1))
         (list (v! -0.5 -0.36 0) (v! 0 0 1 1))))

;; A struct that works on gpu and cpu
(defstruct-g pos-col 
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

;; A GPU vertex shader using a Lisp syntax (see Varjo)
(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

;; A GPU fragment shader 
(defun-g frag ((color :vec4))
  color)

;; Composing those gpu functions into a pipeline
(def-g-> prog-1 ()
    vert frag)

;; Here is what we do each frame:
(defun step-demo ()
  (step-host)        ;; Advance the host environment frame
  (update-repl-link) ;; Keep the REPL responsive while running
  (clear)            ;; Clear the drawing buffer 
  (map-g #'prog-1 *stream*) ;; Render data from GPU datastream
  (swap))            ;; Display newly rendered buffer 


(defun run-loop ()
  (setf *running* t
	;; Create a gpu array from our Lisp vertex data
        *array* (make-gpu-array *triangle-data* :element-type 'pos-col)
	;; Create a GPU datastream
        *stream* (make-buffer-stream *array*))
  ;; continue rendering frames until *running* is set to nil
  (loop :while (and  *running*
		     (not (shutting-down-p))) :do
     (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))

```

Phew! That was a pile of stuff. Follow the comments to get an understanding of what will happen and invoke `(run-loop)`.
