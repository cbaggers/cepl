(in-package :cepl.pipelines)

(defun run-on-gpu (name &rest args)
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
             (let* ((ret-type (get-dyn-return-type-from-stage (first stages))))
               (dispatch-dyn-gpu-call pline call-args ret-type))
          1;;(free-pipeline pline)
          )))))

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
    `(lambda-g (&uniform ,@(append in-args uniforms))
       (labels ((,name ()
                  ,@body))
         (values (v! 0 0 0 0)
                 (:feedback (,name)))))))

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

(defun get-dyn-return-type-from-stage (stage)
  (to-cepl-type-spec
   (varjo:type->type-spec
    (v-type-of
     (second
      (varjo:output-variables stage))))))

(defun dispatch-dyn-gpu-call (pline call-args ret-type)
  (let ((single-point-stream (cepl:make-buffer-stream nil :primitive :points))
        (garray (make-gpu-array nil :element-type ret-type
                                :dimensions 1)))
    (unwind-protect
         (let ((tfs (cepl:make-transform-feedback-stream garray)))
           (cepl:with-transform-feedback (tfs)
             (apply pline (cepl-context) single-point-stream
                    call-args))
           (first (pull-g garray)))
      (free-gpu-array garray)
      (free-buffer-stream single-point-stream))))
