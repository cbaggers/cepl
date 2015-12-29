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

(defun v-translate (in-args uniforms context body &optional transform-passes)
  "Cepl allows you to use varjo is a multi pass fashion by providing a list of
   transform-passes to be run on the varjo-compile-result of each pass."
  (reduce λ(apply #'varjo:translate (run-pass _ _1)) transform-passes
	  :initial-value (varjo:translate in-args uniforms context body)))


(defun run-pass (v-compile-result pass)
  (with-slots (ast-filter ast-transform in-arg-transform uniform-transform) pass
    (let* ((changes
	    (when (and ast-filter ast-transform)
	      (mapcar ast-transform
		      (filter-ast-nodes ast-filter v-compile-result))))
	   (new-body (varjo::ast->code v-compile-result :changes changes))
	   (new-in-args (if in-arg-transform
			    (funcall in-arg-transform
				     (in-args v-compile-result))
			    (in-args v-compile-result)))
	   (new-uniforms (if uniform-transform
			     (funcall uniform-transform
				      (uniforms v-compile-result))
			     (uniforms v-compile-result)))
	   (new-context (context v-compile-result)))
      (list new-in-args new-uniforms new-context new-body))))
