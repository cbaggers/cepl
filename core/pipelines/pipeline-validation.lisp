(in-package :cepl.pipelines)

;;--------------------------------------------------
;; Shader Pipelines

(defun+ assert-valid-gpipe-form (pipeline-name gpipe-args)
  (labels ((check (x) (member x (cons :post varjo:*stage-names*))))
    (let* ((kwds (remove-if-not #'keywordp gpipe-args))
           (invalid (remove-if #'check kwds)))
      (when invalid
        (error 'invalid-keywords-for-shader-gpipe-args
               :pipeline-name pipeline-name :keys invalid))
      (when (and (find :compute kwds) (> (length kwds) 1))
        (error 'compute-pipeline-must-be-single-stage
               :name pipeline-name :stages kwds))))
  (let ((without-keys (remove :post (remove-if #'keywordp gpipe-args))))
    (if (< (length without-keys) 1)
        (error 'not-enough-args-for-implicit-gpipe-stages
               :pipeline-name pipeline-name
               :clauses gpipe-args)
        (assert-valid-gpipe-shader-implicit-form pipeline-name without-keys)))
  (let ((keys (remove-if-not #'keywordp gpipe-args)))
    (unless (assendingp (mapcar (lambda (x) (position x varjo:*stage-names*))
                                keys))
      (error 'invalid-shader-gpipe-stage-keys
             :pipeline-name pipeline-name :keys keys))))

(defun+ assert-valid-gpipe-shader-implicit-form (pipeline-name gpipe-args)
  (let ((valid-forms (remove-if-not #'stage-formp gpipe-args))
        (invalid-forms (remove-if #'stage-formp gpipe-args)))
    (unless (every #'stage-formp gpipe-args)
      (error 'invalid-shader-gpipe-form
             :pipeline-name pipeline-name
             :valid-forms valid-forms
             :invalid-forms invalid-forms)))
  t)

(defun+ assert-valid-stage-specs (names)
  (labels ((check (x) (if (gpu-func-spec x) nil x)))
    (let ((invalid-names (remove-if #'gpu-func-spec (mapcar #'check names))))
      (when invalid-names
        (error 'invalid-stages :invalid-names invalid-names)))))

;;--------------------------------------------------
;; Gpu Function Arg Validation

(defun assert-valid-gpu-function-args (function-name args)
  (let* ((allowed-lone-symbols (copy-list '(:&uniform :&context :&shared)))
         (unknown-key-arguments nil)
         (invalid-syntax nil)
         (constant-names nil)
         (incorrectly-typed-args nil))
    (loop
       :for arg :in args
       :do (cond
             ((symbolp arg)
              (if (find arg allowed-lone-symbols)
                  (setf allowed-lone-symbols
                        (remove arg allowed-lone-symbols))
                  (push arg unknown-key-arguments)))
             ((not (listp arg))
              (push arg invalid-syntax))
             ((< (length arg) 2)
              (push arg invalid-syntax))
             (t (destructuring-bind (name type &rest qualifiers) arg
                  (declare (ignore qualifiers))
                  (if (constantp name)
                      (push name constant-names)
                      (handler-case (type-spec->type type)
                        (varjo.internals::unknown-type-spec (e)
                          (push (list name (slot-value e 'varjo.internals::type-spec))
                                incorrectly-typed-args))))))))
    (assert (and (null unknown-key-arguments)
                 (null invalid-syntax)
                 (null constant-names)
                 (null incorrectly-typed-args))
            ()
            'invalid-gpu-function-args
            :name function-name
            :unknown-key-arguments unknown-key-arguments
            :invalid-syntax invalid-syntax
            :constant-names constant-names
            :incorrectly-typed-args incorrectly-typed-args)))

;;--------------------------------------------------
;; Helper funcs

(defun+ find-invalid-defpipeline-options (options valid-keys)
  (labels ((check (x) (member (car x) valid-keys)))
    (remove-if #'check options)))

(defun+ function-formp (x)
  (and (listp x)
       (eq (first x) 'function)
       (= (length x) 2)
       (or (symbolp (second x))
           (listp (second x)))))

(defun+ typed-defp (x)
  (and (listp x)
       (symbolp (first x))
       (every (lambda (s)
                (or (listp s)
                    (symbolp s)))
              (rest x))))

(defun+ stage-formp (x)
  (or (function-formp x)
      (symbolp x)
      (listp x)))

(defun+ xsymbolp (x)
  (and (symbolp x) (not (keywordp x))))

(defun+ assendingp (list)
  (if (< (length list) 2)
      t
      (not (null (reduce (lambda (x y) (when (numberp x) (when (< x y) y))) list)))))
