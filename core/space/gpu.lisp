(in-package :space)
(in-readtable fn:fn-reader)

;; and let's make the 'in macro that uses it

(varjo:v-defmacro in (space &body body)
  `(let ((,*current-space* ,space))
     ,@body))

;; and a cpu version for formatting

(defmacro in (space &body body)
  (declare (ignore space))
  `(progn ,@body))

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
  (and (ast-typep node 'pos4-g)
       (let ((origin (first (val-origins node))))
	 (and (ast-kindp origin '%p!)
	      (not (id= (flow-ids (ast-space node))
			(flow-ids (ast-space origin))))))))

(defun cross-space->matrix-multiply (node env)
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
	(error 'position->no-space :start-space from-name))
      (let* ((key (concatenate 'string (v-glsl-name node-space)
			       (v-glsl-name origin-space)))
	     (to-name (aref (first (flow-id-origins (flow-ids node-space)
						    t node))
			    1))
	     (var-name (or (gethash key transforms)
			   (setf (gethash key transforms)
				 (name! from-name to-name)))))
	(set-uniform var-name :mat4 env)
	(set-arg-val var-name `(get-transform ,from-name ,to-name) env)
	`(* ,var-name ,node)))))

(defun p!->v! (node env)
  (declare (ignore env))
  (first (ast-args node)))

(defun in-form->progn (node env)
  (declare (ignore env))
  (dbind (% . body) (ast-args node)
    (declare (ignore %))
    `(progn ,@body)))

(defun redundent-in-form-p (node)
  (when (in-form-p node)
    (dbind (((% space-form)) . body) (ast-args node)
      (declare (ignore % body))
      (let ((outer-space (get-var *current-space* (ast-starting-env node)))
            (inner-space space-form))
	(when outer-space
	  (id~= (flow-ids outer-space) (first (flow-ids inner-space))))))))

(def-compile-pass remove-redundent-in-forms
  (#'redundent-in-form-p  #'in-form->progn))

(def-compile-pass (space-pass :depends-on remove-redundent-in-forms)
  (#'cross-space-form-p  #'cross-space->matrix-multiply)
  (#'p!-form-p  #'p!->v!)
  (#'in-form-p  #'in-form->progn))
