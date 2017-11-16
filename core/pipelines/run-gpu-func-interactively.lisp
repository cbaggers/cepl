(in-package :cepl.pipelines)

;;------------------------------------------------------------

(defun run-on-gpu (name args)
  (multiple-value-bind (in-vals uniform-keys-and-vals)
      (split-args-for-dyn-call args)
    (let* ((spec (find-gpu-func-spec-by-name-and-vals name in-vals))
           (in-arg-names (with-gpu-func-spec spec
                           (mapcar #'first in-args)))
           (in-arg-keys (mapcar #'kwd in-arg-names))
           (in-arg-keys-and-vals (mapcan #'list in-arg-keys in-vals))
           (call-args (append in-arg-keys-and-vals
                              uniform-keys-and-vals)))
      (multiple-value-bind (pline stages)
          (dyn-code-to-pipeline-and-stages
           (gen-vertex-stage-code-calling-func
            spec))
        (unwind-protect
             (let* ((ret-types (get-dyn-return-types-from-stage (first stages))))
               (dispatch-dyn-gpu-call pline call-args ret-types))
          (free-pipeline pline))))))

(defun split-args-for-dyn-call (args)
  (let* ((split (or (position-if #'keywordp args)
                    (length args)))
         (in-args (subseq args 0 split))
         (uniforms (subseq args split)))
    (values in-args uniforms)))

(defun find-gpu-func-spec-by-name-and-vals (name in-vals)
  (let* ((vtypes (mapcar #'guess-a-varjo-type in-vals))
         (gfunc (%gpu-function (cons name vtypes))))
    (cepl.pipelines::gpu-func-spec gfunc)))

(defun gen-vertex-stage-code-calling-func (spec)
  (with-gpu-func-spec spec
    (let ((gname (gensym "foo")))
      `(lambda-g (&uniform ,@(append in-args uniforms))
         (labels ((,gname () ,@body))
           (spliced-values ,gname (v! 0 0 0 0)))))))

(defun dyn-code-to-pipeline-and-stages (code)
  (make-lambda-pipeline-inner (list :vertex (compile-g nil code))
                                  '(:points)))

(defun to-cepl-type-spec (spec)
  (labels ((inner (spec)
             (assert (symbolp spec))
             (if (and (equal (package-name (symbol-package spec)) "VARI.TYPES")
                      (equal (subseq (symbol-name spec) 0 2) "V-"))
                 (intern (subseq (symbol-name spec) 2) :keyword)
                 spec)))
    (if (listp spec)
        (cons (inner (first spec)) (rest spec))
        (values (inner spec)))))

(defun get-dyn-return-types-from-stage (stage)
  (labels ((inner (x)
             (to-cepl-type-spec
              (varjo:type->type-spec
               (v-type-of x)))))
    (mapcar #'inner (rest (varjo:output-variables stage)))))

(defun dispatch-dyn-gpu-call (pline call-args ret-types)
  (let ((single-point-stream (cepl:make-buffer-stream nil :primitive :points))
        (garrays (mapcar (lambda (ret-type)
                           (make-gpu-array nil :element-type ret-type
                                           :dimensions 1))
                         ret-types)))
    (unwind-protect
         (let ((tfs (apply #'cepl:make-transform-feedback-stream garrays)))
           (cepl:with-transform-feedback (tfs)
             (apply pline (cepl-context) single-point-stream
                    call-args))
           (values-list (mapcar (lambda (arr) (first (pull-g arr)))
                                garrays)))
      (map nil #'free-gpu-array garrays)
      (free-buffer-stream single-point-stream))))

(varjo.internals:v-defspecial spliced-values (name form)
  :args-valid t
  :return
  (let* ((scanned (varjo.internals::map-environments
                   (lambda (e)
                     (second (find name (varjo.internals:v-form-bindings e)
                                   :key #'first)))
                   env))
         (trimmed (first (remove nil scanned))))
    (assert trimmed)
    (let* ((return-spec (varjo.internals:v-return-spec trimmed))
           (feedback (loop :for i :below (length return-spec) :collect
                        `((:feedback ,i) ,(gensym)))))
      (varjo.internals:compile-form
       `(multiple-value-bind ,(mapcar #'second feedback) (,name)
          (values
           ,form
           ,@feedback))
       env))))
