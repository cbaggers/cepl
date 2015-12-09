(in-package :space-gpu)
(in-readtable fn:fn-reader)

;; ok so we need to make spaces available on the gpu
;; this is going to require two passes when compiling, one to get
;; all the flow information, the second to compile the new result

;; we are going to mock this out here.

;; lets have a type to represent a space

(varjo::def-v-type-class space-g (varjo::v-type)
  ((varjo::core :initform nil :reader varjo::core-typep)
   (varjo::glsl-string :initform "#<space>" :reader varjo::v-glsl-string)))

;; we need a function so we can see when a space is set

(varjo:v-defun use-space (s) "#<use-space(~a)>" (space-g) :void)

;; a name for the space
(defvar *current-space* (gensym "current-space"))

;; and let's make the 'in macro that uses it

(varjo:v-defmacro in (space &body body)
  `(let ((,*current-space* ,space))
     (use-space ,*current-space*)
     ,@body))

;; and a cpu version for formatting

(defmacro in (space &body body)
  (declare (ignore space))
  `(progn ,@body))

;; now we need positions

(varjo::def-v-type-class pos4 (varjo::v-vec4)
  ((varjo::core :initform nil :reader varjo::core-typep)
   (varjo::glsl-string :initform "#<pos4>" :reader varjo::v-glsl-string)))

(varjo:v-defmacro p! (v)
  `(%p! ,v ,*current-space*))

(varjo:v-defun %p! (v s) "#<pos4(~a, ~a)>" (:vec4 space-g) pos4)

;; we need some varjo code to compile

(defun first-pass ()
  (varjo::defshader test ((vert :vec4) &uniform (ws space-g))
    (in ws
      (let ((p (p! vert)))
	(+ p p)
	(v! 1 2 3 4)))))

(defun returned-flow-id (compile-result flow-id)
  (find-if λ(and (last1 _) (id= (last1 _) flow-id))
	   (function-calls compile-result)))

(defun flow-id-n! (&rest ids)
  (make-instance 'varjo::flow-identifier :ids ids))

(defun which-space (compile-result flow-id)
  (let ((origin (returned-flow-id compile-result flow-id)))
    (second (second origin))))
