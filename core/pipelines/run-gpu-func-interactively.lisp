(in-package :cepl.pipelines)

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

(defun run-on-gpu (name &rest args)
  (let* ((split (or (position-if #'keywordp args)
                    (length args)))
         (important-args (subseq args 0 split))
         (vtypes
          (mapcar #'guess-a-varjo-type important-args))
         (gfunc (%gpu-function (cons name vtypes)))
         (spec (cepl.pipelines::gpu-func-spec gfunc)))
    (with-gpu-func-spec spec
      (let* ((code `(lambda-g (&uniform ,@(append in-args uniforms))
                      (labels ((,name ()
                                 ,@body))
                        (values (v! 0 0 0 0)
                                (:feedback (,name))))))
             (glam (compile-g nil code)))
        (multiple-value-bind (pline stages)
            (make-lambda-pipeline-inner (list :vertex glam) '(:points))
          (let* ((ret-type (to-cepl-type-spec
                            (varjo:type->type-spec
                             (v-type-of
                              (second
                               (varjo:output-variables
                                (first stages)))))))
                 (key-names (mapcan
                             (lambda (x y)
                               (list (intern (symbol-name (car x))
                                             :keyword)
                                     y))
                             in-args args))
                 (call-args (append key-names (subseq args split)))
                 (garray (make-gpu-array nil :element-type ret-type
                                         :dimensions 1))
                 (tfs (cepl:make-transform-feedback-stream
                       garray)))
            (cepl:with-transform-feedback (tfs)
              (apply pline (cepl-context)
                     (cepl:make-buffer-stream nil :primitive :points)
                     call-args))
            (first (pull-g garray))))))))
