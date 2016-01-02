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
		  :reader ast-transform)
   (in-arg-transform :initarg :in-arg-transform
		     :initform nil
		     :reader in-arg-transform)
   (uniform-transform :initarg :uniform-transform
		     :initform nil
		     :reader uniform-transform)))

(defun compile-pass! (ast-filter ast-transform
		      in-arg-transform uniform-transform)
  (assert (and (when ast-transform ast-filter)
	       (when ast-filter ast-transform)))
  (make-instance 'compile-pass
		 :ast-filter ast-filter
		 :ast-transform ast-transform
		 :in-arg-transform in-arg-transform
		 :uniform-transform uniform-transform))

;;--------------------------------------------------

(defvar *registered-passes* nil)

(defmacro def-compile-pass (name &key ast-filter ast-transform in-arg-transform
				   uniform-transform)
  (assert (and ast-filter ast-transform))
  `(unless (assoc ',name *registered-passes*)
     (push (cons ',name (compile-pass! ,ast-filter ,ast-transform
				       ,in-arg-transform ,uniform-transform))
	   *registered-passes*)))

(defun undef-compile-pass (name)
  (setf *registered-passes*
	(remove-if λ(eq name (car _)) *registered-passes*))
  t)

(defun %get-passes () (mapcar #'cdr *registered-passes*))

;;--------------------------------------------------

(defun v-translate (in-args uniforms context body &optional transform-passes)
  "Cepl allows you to use varjo in a multi pass fashion by providing a list of
   transform-passes to be run on the varjo-compile-result of each pass.

   This function runs all the passes until there are no more changes to make.
   It knows this when none of the passes' ast-filter methods return any nodes to
   be transformed."
  (labels ((on-pass (c-result pass)
	     (let ((filtered (filter-pass c-result pass)))
	       (if filtered
		   (apply #'varjo:translate (run-pass c-result pass filtered
						      in-args uniforms context))
		   c-result)))
	   (once-through (initial)
	     (reduce #'on-pass transform-passes :initial-value initial))
	   (until-no-change (initial)
	     (let ((new-result (once-through initial)))
	       (if (eq new-result initial)
		   initial
		   (until-no-change new-result)))))
    (until-no-change
     (varjo:translate in-args uniforms context body))))

(defun v-rolling-translate (stages &optional transform-passes)
  (varjo:rolling-translate stages (fn:fn~r #'v-translate transform-passes)))

(defun filter-pass (v-compile-result pass)
  (with-slots (ast-filter ast-transform) pass
    (when (and ast-filter ast-transform)
      (filter-ast-nodes ast-filter v-compile-result))))

(defun run-pass (v-compile-result pass filtered-nodes
		 original-in-args original-uniforms original-context)
  (with-slots (ast-transform in-arg-transform uniform-transform) pass
    (let* ((changes (mapcar ast-transform filtered-nodes))
	   (new-body (varjo::ast->code v-compile-result :changes changes))
	   (new-in-args (if in-arg-transform
			    (funcall in-arg-transform
				     (in-args v-compile-result))
			    original-in-args))
	   (tp-meta (third-party-metadata v-compile-result))
	   (new-uniforms
	    (if uniform-transform
		(process-args-for-pass (uniforms v-compile-result)
				       uniform-transform
				       tp-meta)
		original-uniforms)))
      (list new-in-args new-uniforms original-context new-body))))

(defun process-args-for-pass (args transform tp-meta)
  (loop :for u-arg :in args
     :for s = (gensym "val-form")
     :for (uf uval) = (if transform
			  (funcall transform u-arg s)
			  `(,args))
     :collect uf
     :do (symbol-macrolet ((last-uval (gethash s tp-meta)))
	   (setf last-uval `(let ((,s ,last-uval)) ,uval)))))
