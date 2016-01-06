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

;; a name for the space
(defvar *current-space* (gensym "current-space"))

;; and let's make the 'in macro that uses it

(varjo:v-defmacro in (space &body body)
  `(let ((,*current-space* ,space))
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

(varjo:v-defun v! (p) "~a" (pos4) :vec4)
(varjo:v-defun v! (p) "~a" (:vec4) :vec4)

;;----------------------------------------------------------------------

;; now lets define the real compiler pass

(defun ast-space (node)
  (get-var *current-space* node))

(defun p!-form-p (node)
  (ast-kindp node '%p!))

(defun in-form-p (node)
  (and (ast-kindp node 'let)
       (dbind (args . body) (ast-args node)
	 (declare (ignore body))
	 (eq (caar args) *current-space*))))

(defun cross-space-form-p (node)
  (and (ast-typep node 'pos4)
       (let ((origin (first (val-origins node))))
	 (and (ast-kindp origin '%p!)
	      (not (id= (flow-ids (ast-space node))
			(flow-ids (ast-space origin))))))))

(defun cross-space->matrix-multiply (node env)
  (print 'cross-space->matrix-multiply)
  (labels ((name! (from-name to-name)
	     (symb from-name '-to- to-name '-transform)))
    (let* ((transforms
	     (or (gethash 'transforms env)
		 (setf (gethash 'transforms env)
		       (make-hash-table :test #'equal))))
	   (node-space (ast-space node))
	   (origin-space (ast-space (first (val-origins node))))
	   (from-name (aref (first (flow-id-origins (flow-ids origin-space)
						    t node))
			    1)))
      (unless node-space
	(error 'spaces::position->no-space :start-space from-name))
      (let* ((key (concatenate 'string (v-glsl-name node-space)
			       (v-glsl-name origin-space)))
	     (to-name (aref (first (flow-id-origins (flow-ids node-space)
						    t node))
			    1))
	     (var-name (or (gethash key transforms)
			   (setf (gethash key transforms)
				 (name! from-name to-name)))))
	(set-uniform var-name :mat4 env)
	(set-arg-val var-name `(spaces:get-transform ,from-name ,to-name) env)
	`(* ,var-name ,node)))))

(defun p!->v! (node env)
  (declare (ignore env))
  (print 'p!->v!)
  (first (ast-args node)))

(defun in-form->progn (node env)
  (print 'in-form->progn)
  (dbind (((% space-form)) . body) (ast-args node)
    (declare (ignore %))
    (let* ((origin (val-origins space-form))
	   (uniform-name (aref (first origin) 1)))
      (remove-uniform uniform-name env)
      `(progn ,@body))))

(def-compile-pass space-pass
  (#'cross-space-form-p  #'cross-space->matrix-multiply)
  (#'p!-form-p  #'p!->v!)
  (#'in-form-p  #'in-form->progn))
