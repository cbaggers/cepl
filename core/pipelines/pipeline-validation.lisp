(in-package :cepl.pipelines)

;;--------------------------------------------------
;; Shader Pipelines

(defun assert-valid-gpipe-form (pipeline-name gpipe-args)
  (labels ((check (x) (member x (cons :post varjo:*stage-types*))))
    (let ((invalid (remove-if #'check (remove-if-not #'keywordp gpipe-args))))
      (when invalid
        (error 'invalid-keywords-for-shader-gpipe-args
               :pipeline-name pipeline-name :keys invalid))))
  (let ((without-keys (remove :post (remove-if #'keywordp gpipe-args))))
    (if (< (length without-keys) 2)
	(error 'not-enough-args-for-implicit-gpipe-stages
	       :pipeline-name pipeline-name
	       :clauses gpipe-args)
	(assert-valid-gpipe-shader-implicit-form pipeline-name without-keys)))
  (let ((keys (remove-if-not #'keywordp gpipe-args)))
    (unless (assendingp (mapcar (lambda (x) (position x varjo:*stage-types*))
				keys))
      (error 'invalid-shader-gpipe-stage-keys
	     :pipeline-name pipeline-name :keys keys))))

(defun assert-valid-gpipe-shader-implicit-form (pipeline-name gpipe-args)
  (let ((valid-forms (remove-if-not #'stage-formp gpipe-args))
        (invalid-forms (remove-if #'stage-formp gpipe-args)))
    (unless (every #'stage-formp gpipe-args)
      (error 'invalid-shader-gpipe-form
             :pipeline-name pipeline-name
             :valid-forms valid-forms
             :invalid-forms invalid-forms)))
  t)

(defun assert-valid-stage-specs (names)
  (labels ((check (x) (if (gpu-func-spec x) nil x)))
    (let ((invalid-names (remove-if #'gpu-func-spec (mapcar #'check names))))
      (when invalid-names
        (error 'invalid-stages :invalid-names invalid-names)))))

;;--------------------------------------------------
;; Helper funcs

(defun find-invalid-defpipeline-options (options valid-keys)
  (labels ((check (x) (member (car x) valid-keys)))
    (remove-if #'check options)))

(defun function-formp (x)
  (and (listp x)
       (eq (first x) 'function)
       (= (length x) 2)
       (or (symbolp (second x))
	   (listp (second x)))))

(defun typed-defp (x)
  (and (listp x)
       (symbolp (first x))
       (every (lambda (s)
		(or (listp s)
		    (symbolp s)))
	      (rest x))))

(defun stage-formp (x)
  (or (function-formp x)
      (symbolp x)
      (listp x)))

(defun xsymbolp (x)
  (and (symbolp x) (not (keywordp x))))

(defun assendingp (list)
  (if (< (length list) 2)
      t
      (not (null (reduce (lambda (x y) (when (numberp x) (when (< x y) y))) list)))))
