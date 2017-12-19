(in-package :cepl.pipelines)
(in-readtable :fn.reader)

;; (bake-uniforms 'foo :jam #'(ham :int))

(defmacro bake-uniforms (pipeline &rest uniforms &key &allow-other-keys)
  (labels ((expand-literal-func (x)
             (if (and (listp x) (eq (first x) 'function))
                 `(gpu-function ,(second x))
                 x)))
    `(%bake ,pipeline ,@(mapcar #'expand-literal-func uniforms))))

(defun+ %bake (pipeline &rest uniforms &key &allow-other-keys)
  (let* ((pipeline (typecase pipeline
                     ((or list symbol) (pipeline-spec pipeline))
                     (function (cepl.pipelines::function-keyed-pipeline
                                pipeline))
                     (t (error 'bake-invalid-pipeling-arg
                               :invalid-arg pipeline))))
         ;;
         ;; get pipeline details
         (stage-pairs (pairs-key-to-stage (pipeline-stage-pairs pipeline)))
         (stages (mapcar #'cdr stage-pairs))
         (pipeline-uniforms (cepl.pipelines::aggregate-uniforms stages))
         (context (slot-value pipeline 'context))
         (draw-mode (varjo.internals:get-primitive-type-from-context context))
         ;;
         ;; get uniform details
         (uniform-pairs-to-bake (group uniforms 2))
         (uniform-names-to-bake (mapcar #'first uniform-pairs-to-bake))
         (uniform-vals-to-bake (mapcar #'second uniform-pairs-to-bake)))
    ;;
    ;; check for proposed uniforms that arent in the pipeline
    (let ((invalid (remove-if λ(find _ pipeline-uniforms :key #'first
                                     :test #'string=)
                              uniform-names-to-bake)))
      (assert (not invalid) () 'bake-invalid-uniform-name
              :proposed uniforms :invalid invalid))
    ;;
    ;; check in case user passed symbol instead of a gpu-func
    (let ((symbol/list-values (remove-if-not λ(or (symbolp _) (listp _))
                                             uniform-vals-to-bake)))
      (assert (not symbol/list-values) ()
              'bake-uniform-invalid-values
              :proposed uniforms :invalid symbol/list-values))
    (let* ((vals-with-func-identifiers
            (mapcar λ(if (typep _ 'func-key)
                         (func-key->name _)
                         _)
                    uniform-vals-to-bake))
           (final-uniform-pairs
            (mapcar #'list uniform-names-to-bake vals-with-func-identifiers)))
      ;;
      ;; Go for it
      (bake-and-g-> draw-mode stage-pairs final-uniform-pairs))))


(defun+ bake-and-g-> (draw-mode stage-pairs uniforms-to-bake)
  (let* ((stage-pairs (pairs-key-to-stage stage-pairs))
         (glsl-version (compute-glsl-version-from-stage-pairs stage-pairs))
         (stage-pairs (swap-versions stage-pairs glsl-version)))
    ;;

    ;;
    (make-lambda-pipeline
     (mapcan (lambda (pair)
               (list (first pair)
                     (let* ((stage (parsed-gpipe-args->v-translate-args
                                    draw-mode pair uniforms-to-bake))
                            (in-args (mapcar #'varjo.internals:to-arg-form
                                             (varjo:input-variables stage)))
                            (uniforms (mapcar #'varjo.internals:to-arg-form
                                              (varjo:uniform-variables stage)))
                            (body (varjo.internals:lisp-code stage))
                            (args (append in-args
                                          (when uniforms
                                            (cons '&uniforms uniforms)))))
                       (make-gpu-lambda args body))))
             stage-pairs)
     nil)))
