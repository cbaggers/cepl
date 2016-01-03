(in-package :cgl)
(in-readtable fn:fn-reader)

;; Often we want to run varjo on some code and use the metadata in the
;; varjo-compile-result to inform modifications to the code and uploads.
;;
;; After making the modifications we pass the new code and args into the
;; varjo for another pass.
;;
;; Below we have versions of varjo's translate and rolling-translate functions
;; that take optional extra passes.

(defclass compile-pass ()
  ((ast-filter :initarg :ast-filter
	       :initform nil
	       :reader ast-filter)
   (ast-transform :initarg :ast-transform
		  :initform nil
		  :reader ast-transform)))

(defun compile-pass! (ast-filter ast-transform)
  (assert (and (when ast-transform ast-filter)
	       (when ast-filter ast-transform)))
  (make-instance 'compile-pass
		 :ast-filter ast-filter
		 :ast-transform ast-transform))

;;--------------------------------------------------

(defvar *registered-passes* nil)

(defmacro def-compile-pass (name &key ast-filter ast-transform)
  (assert (and ast-filter ast-transform))
  `(setf *registered-passes*
	 (cons (cons ',name (compile-pass! ,ast-filter ,ast-transform))
	       (if (assoc ',name *registered-passes*)
		   (remove ',name *registered-passes* :key #'car)
		   *registered-passes*))))

(defun undef-compile-pass (name)
  (setf *registered-passes*
	(remove-if λ(eq name (car _)) *registered-passes*))
  t)

(defun %get-passes () (mapcar #'cdr *registered-passes*))

;;--------------------------------------------------

(defun make-pass-env (arg-val-map)
  (assert (typep arg-val-map 'hash-table))
  (let ((e (make-hash-table)))
    (setf (gethash 'set-uniforms e) nil
	  (gethash 'remove-uniforms e) nil
	  (gethash 'uniform-vals e) arg-val-map)
    e))

(defun v-translate (in-args uniforms context body
		    &optional tp-meta transform-passes)
  "Cepl allows you to use varjo in a multi pass fashion by providing a list of
   transform-passes to be run on the varjo-compile-result of each pass.

   This function runs all the passes until there are no more changes to make.
   It knows this when none of the passes' ast-filter methods return any nodes to
   be transformed."
  (let* ((tp-meta (or tp-meta (make-hash-table)))
	 (arg-val-map (or (gethash 'uniform-vals tp-meta)
			  (let ((h (make-hash-table)))
			    (loop :for (n . r) :in uniforms
			       :do (setf (gethash n h) n))
			    h)))
	 (passes (mapcar λ(cons _ (make-pass-env arg-val-map))
			 transform-passes)))
    (labels ((on-pass (c-result pass-pair)
	       (dbind (pass . transform-env) pass-pair
		 (let ((filtered (filter-pass c-result pass)))
		   (if filtered
		       (apply #'varjo:translate
			      (run-pass c-result pass filtered transform-env
					in-args uniforms context))
		       c-result))))
	     (once-through (initial)
	       (reduce #'on-pass passes :initial-value initial))
	     (until-no-change (initial)
	       (let ((new-result (once-through initial)))
		 (if (eq new-result initial)
		     initial
		     (until-no-change new-result)))))
      (let ((compile-result
	     (until-no-change
	      (varjo:translate in-args uniforms context body tp-meta))))
	(with-hash (av 'uniform-vals) (third-party-metadata compile-result)
	  (setf av arg-val-map))
	(cepl::peek compile-result)))))

(defun v-rolling-translate (stages &optional transform-passes)
  (varjo:rolling-translate stages (fn:fn~r #'v-translate transform-passes)))

(defun filter-pass (v-compile-result pass)
  (with-slots (ast-filter ast-transform) pass
    (when (and ast-filter ast-transform)
      (filter-ast-nodes ast-filter v-compile-result))))

(defun run-pass (v-compile-result pass filtered-nodes env
		 original-in-args original-uniforms original-context)
  (with-slots (ast-transform) pass
    (let* ((changes (mapcar λ(funcall ast-transform _ env)
			    filtered-nodes))
	   (new-body (varjo::ast->code v-compile-result :changes changes))
	   (new-in-args original-in-args)
	   (new-uniforms
	    (remove-duplicates
	     (append (reduce λ(remove _1 _ :key #'car)
			     (gethash 'remove-uniforms env)
			     :initial-value original-uniforms)
		     (gethash 'set-uniforms env))
	     :key #'car)))
      (list new-in-args new-uniforms original-context new-body))))

(defun set-uniform (name type env)
  (with-hash (su 'set-uniforms) env
    (setf su (remove-duplicates (cons (list name type) su)
				:key #'car))))

(defun remove-uniform (name env)
  (with-hash (su 'remove-uniforms) env
    (setf su (remove-duplicates (cons name su)))))

(defun set-arg-val (name form env &key (current-val-symb :current))
  (with-hash (arg-vals 'uniform-vals) env
    (with-hash (arg name) arg-vals
      (setf arg (subst arg current-val-symb form)))))
