(in-package :jungl.space)
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

(defun has-space (node)
  (not (null (get-var *current-space* node))))

(defun p!-form-p (node)
  (ast-kindp node '%p!))

(defun in-form-p (node)
  (and (ast-kindp node 'let)
       (dbind (args . body) (ast-args node)
	 (declare (ignore body))
	 (eq (caar args) *current-space*))))

(defun cross-space-form-p (node)
  "Predicate for cases where a position is crossing
   between two valid spaces"
  (and (ast-typep node 'pos4-g)
       (let ((origin (first (val-origins node))))
	 (and (ast-kindp origin '%p!)
	      (has-space node)
	      (has-space origin)
	      (not (id= (flow-ids (ast-space node))
			(flow-ids (ast-space origin))))))))

progn
(defun cross-space->matrix-multiply (node env)
  ;;[0] get or create the transforms table. Note that env is the
  ;;    'transform environment' we add a transforms hash-table to
  ;;    to this for reasons best known to past me :|
  (let* ((transforms
	  (or (gethash 'transforms env)
	      (setf (gethash 'transforms env)
		    (make-hash-table :test #'equal))))
	 ;; right so there are to spaces of interest:
	 ;; 1. The one the current node is in (to-space)
	 ;; 2. The one where the value (which is crossing between these
	 ;;    two spaces) is from
	 ;;
	 ;; Let's start with 1.
	 ;; get the space-scope the ast-node is inside
	 (to-space (ast-space node))
	 ;; We then can query the origin of the node's flow id and
	 ;; get the ast-node/s it was from
	 (to-name (origin-name (first (flow-id-origins (flow-ids to-space)
						       t node))))
	 ;; A bit more work to get 2.
	 ;; the ast-node has some value, that value originated
	 ;; somewhere else in the program. Find the ast-node where
	 ;; it originated and then the space scope that it was in when
	 ;; created
	 (from-space (ast-space (first (val-origins node))))
	 ;; with that we can get back further. A flow-id could have
	 ;; originated from a function call. This traces it back to
	 ;; the ast-node inside the function where it was created
	 (from-origins (flow-id-origins (flow-ids from-space)
					t node))
	 (from-name (origin-name (first from-origins)))
	 ;; Now we are done with the searching, time to use the results
	 ;;
	 ;; we could have multiple of the same transform so we should
	 ;; deduplicate. This is a hacky way to get a key
	 (key (concatenate 'string (v-glsl-name to-space)
			   (v-glsl-name from-space)))
	 ;; and let's make a name for the lisp variable containing the
	 ;; space->space transform
	 (var-name (or (gethash key transforms)
		       (setf (gethash key transforms)
			     (%gen-space->space-name from-name to-name)))))
    ;; we now have everything we need:
    ;; let's add a uniform with our new name
    (set-uniform var-name :mat4 env)
    ;; here we set how we get the transform we are uploading
    (set-arg-val var-name `(get-transform ,from-name ,to-name) env)
    ;; and here is the replacement code for our crossing the spaces
    (if (eq (ast-kind node) 'let)
	`(p! (* ,var-name
		(let ,@(butlast (ast-args node))
		  (v! ,(last1 (ast-args node))))))
	`(p! (* ,var-name (v! ,node))))))



(defun %gen-space->space-name (from-name to-name)
  "generate a name for the uniform that will contain the
   space->space transform."
  (symb from-name '-to- to-name '-transform))


(defun cross-to-null-space-form-p (node)
  "Predicate for cases where position is crossing
   from a valid space to a null one."
  (and (ast-typep node 'pos4-g)
       (let ((origin (first (val-origins node))))
	 (and (ast-kindp origin '%p!)
	      (not (has-space node))
	      (has-space origin)))))

(defun cross-to-null-space (node env)
  (declare (ignore env))
  `(values-safe (v! ,node)))

(defun p!->v! (node env)
  (declare (ignore env))
  (let ((args (ast-args node)))
    `(v! ,@(butlast args))))

(defun in-form->progn (node env)
  (declare (ignore env))
  (dbind (% . body) (ast-args node)
    (declare (ignore %))
    `(progn ,@body)))

(defun redundent-in-form-p (node)
  "Check if the *current-space* outside the scope is the
   same *current-space* as inside the scope."
  (declare (optimize (debug 3) (speed 0)))
  (when (in-form-p node)
    (dbind (((% space-form)) . body) (ast-args node)
      (declare (ignore % body))
      (let ((outer-space (get-var *current-space* (ast-starting-env node)))
            (inner-space space-form))
	(when outer-space
	  (id~= (flow-ids outer-space) (flow-ids inner-space)))))))

(defun redundent-v!-form-p (node)
  (and (eq (ast-kind node) 'v!)
       (= (length (ast-args node)) 1)))

(defun splice-v!-form (node env)
  (declare (ignore env))
  (first (ast-args node)))


(def-compile-pass remove-redundent-in-forms
  (#'redundent-in-form-p  #'in-form->progn))

(def-deep-pass (cross-space-pass :depends-on remove-redundent-in-forms)
    :filter #'cross-space-form-p
    :transform #'cross-space->matrix-multiply)

(def-compile-pass (space-pass :depends-on cross-space-pass)
  (#'cross-to-null-space-form-p #'cross-to-null-space)
  (#'p!-form-p  #'p!->v!)
  (#'in-form-p  #'in-form->progn))

(def-compile-pass (remove-redundent-v!-forms :depends-on space-pass)
  (#'redundent-v!-form-p  #'splice-v!-form))
