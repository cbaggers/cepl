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
  (varjo::defshader test ((vert :vec4) &uniform (ws space-g) (cs space-g))
    (in cs                  ;; this would be implicit
      (labels ((func () (p! (v! 0 0 0 0)))
	       (func2 () (func)))
	(let ((p (p! vert)))
	  (in ws
	    (+ p (func2)) ;; these need to be transformed
	    (v! 1 2 3 4)))))))

;; for making specific flow ids for testing
(defun flow-id-n! (&rest ids)
  (make-instance 'varjo::flow-identifier :ids ids))

(defun hmm ()
  (visit-ast-nodes
   λ(when (v-typep (ast-return-type _) (type-spec->type 'pos4))
      (let ((origin (caar (ast-flow-id-origin _))))
	(when (eq (ast-node-kind origin) '%p!)
	  (let ((current-space (varjo::get-var *current-space* (ast-starting-env _)))
		(o-space (varjo::get-var *current-space* (ast-starting-env origin))))
	    (unless (eq current-space o-space)
	      (print
	       (list (ast-node-kind _) (ast-args _)
		     (varjo::v-glsl-name current-space)
		     (varjo::v-glsl-name o-space))))))))
   (first-pass)))

(defun ast->formid (x)
  (labels ((f (node walk)
	     (let ((node-kind (ast-node-kind node))
		   (args (ast-args node))
		   (flow-id-origin (ast-flow-id-origin node)))
	       (let ((name (if (typep node-kind 'v-function)
			       (varjo::name node-kind)
			       node-kind)))
		 `(,name ,node ,@(unless (eq node (caar flow-id-origin))
					 (list flow-id-origin))
			 ,(mapcar walk args))))))
    (walk-ast #'f x)))
