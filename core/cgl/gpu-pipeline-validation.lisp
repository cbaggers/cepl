(in-package :cgl)

(defun assert-valid-gpipe-form (pipeline-name clauses context)
  (case context
    (:shader (assert-valid-gpipe-shader-form pipeline-name clauses))
    (:compose (assert-valid-gpipe-compose-form pipeline-name clauses))
    (otherwise (error 'invalid-context-for-assert-gpipe :context context)))
  t)

(defun assert-valid-options (pipeline-name options context)
  (case context
    (:shader (assert-valid-shader-options pipeline-name options))
    (:compose (assert-valid-compose-options pipeline-name options))
    (otherwise (error 'invalid-context-for-assert-options :context context))))

;;--------------------------------------------------
;; Shader Pipelines

(defun assert-valid-gpipe-shader-form (pipeline-name clauses)
  (varjo:pipe-> (pipeline-name clauses)
    #'check-shader-gpipe-keywords
    #'check-shader-gpipe-stage-order))

(defun check-shader-gpipe-keywords (pipeline-name clauses)
  (labels ((check (x) (member x (cons :context varjo:*stage-types*))))
    (let ((invalid (remove-if #'check (remove-if-not #'keywordp clauses))))
      (when invalid
        (error 'invalid-keywords-for-shader-gpipe-args
               :pipeline-name pipeline-name :keys invalid))))
  (values pipeline-name clauses))

(defun check-shader-gpipe-stage-order (pipeline-name clauses)
  (multiple-value-bind (clauses context) clauses
    (declare (ignore context))
    (let ((without-keys (remove-if #'keywordp clauses)))
      (if (< (length without-keys) 2)
          (error 'not-enough-args-for-implicit-gpipe-stages
                 :pipeline-name pipeline-name
                 :clauses clauses)
          (assert-valid-gpipe-shader-implicit-form pipeline-name without-keys)))
    (let ((keys (remove-if-not #'keywordp clauses)))
      (unless (assendingp (mapcar (lambda (x) (position x varjo:*stage-types*))
                                  keys))
        (error 'invalid-shader-gpipe-stage-keys
               :pipeline-name pipeline-name :keys keys))))
  (values pipeline-name clauses))

(defun assert-valid-gpipe-shader-implicit-form (pipeline-name clauses)
  (let ((valid-forms (remove-if-not #'function-formp clauses))
        (invalid-forms (remove-if #'function-formp clauses)))
    (unless (every #'function-formp clauses)
      (error 'invalid-shader-gpipe-form
             :pipeline-name pipeline-name
             :valid-forms valid-forms
             :invalid-forms invalid-forms)))
  t)

(defun split-context (args)
  (let ((cut-pos (or (position :context args) (length args))))
    (destructuring-bind (&key context) (subseq args cut-pos)
      (values (subseq args 0 cut-pos) context))))

(defun assert-valid-stage-specs (names)
  (labels ((check (x) (if (gpu-func-spec x) nil x)))
    (let ((invalid-names (remove nil (mapcar #'check names))))
      (when invalid-names
        (error 'invalid-stages :invalid-names invalid-names)))))

;;--------------------------------------------------
;; Compose Pipelines

;; (defpipeline bloom (stream &uniform tx)
;;     (g-> (c0 (blit stream :tex tx))
;;          ((c1 0) (meh))
;;          (c2 (smooth stream :tex (attachment h3 0) :offset (v! 0 (/ 1.2 64))))
;;          (nil (combine stream
;;                        :t0 (attachment c0 0) :t1 (attachment c1 0)
;;                        :t2 (attachment c2 0) :t3 (attachment c3 0))))
;;   :fbos
;;   (c0 '(:c :dimensions (512 512) :magnify-filter :linear))
;;   (c1 '(:c :dimensions (256 256) :magnify-filter :linear))
;;   (h2 '(:c :dimensions (512 512))))

(defun assert-valid-gpipe-compose-form (pipeline-name clauses)
  (varjo:pipe-> (pipeline-name clauses)
    #'check-compose-gpipe-simple-cases)
  t)

(defun check-compose-gpipe-simple-cases (pipeline-name clauses)
  (labels ((triv-valid (x)
             (and (or (listp (first x)) (xsymbolp (first x)))
                  (and (listp (last1 x))
                       (xsymbolp (first (last1 x)))))))
    (let ((invalid-passes (remove-if #'triv-valid clauses)))
      (when invalid-passes
        (error 'invalid-compose-gpipe-form
               :pipeline-name pipeline-name
               :clauses invalid-passes))))
  (values pipeline-name clauses))

(defun assert-valid-shader-options (pipeline-name options)
  (let* ((valid '(:context :post))
         (invalid (find-invalid-defpipeline-options options valid)))
    (when invalid
        (error 'invalid-defpipeline-options
               :pipeline-name pipeline-name
               :invalid-options invalid
               :valid-options valid))))

(defun assert-valid-compose-options (pipeline-name options)

  (let* ((valid '(:fbos :context :post))
         (invalid (find-invalid-defpipeline-options options valid)))
    (when invalid
      (error 'invalid-defpipeline-options
             :pipeline-name pipeline-name
             :invalid-options invalid
             :valid-options valid))))

(defun assert-valid-pipeline-specs (names)
  (let ((names (remove-duplicates names)))
    (labels ((check (x) (if (pipeline-spec x) nil x)))
      (let ((invalid-names (remove nil (mapcar #'check names))))
        (when invalid-names
          (error 'invalid-stages :invalid-names invalid-names)))))
  t)

;;--------------------------------------------------
;; Helper funcs

(defun find-invalid-defpipeline-options (options valid-keys)
  (labels ((check (x) (member (car x) valid-keys)))
    (remove-if #'check options)))

(defun function-formp (x) (and (listp x) (eq (first x) 'function)))

(defun xsymbolp (x)
  (and (symbolp x) (not (keywordp x))))

(defun assendingp (list)
  (if (< (length list) 2)
      t
      (not (null (reduce (lambda (x y) (when (numberp x) (when (< x y) y))) list)))))
