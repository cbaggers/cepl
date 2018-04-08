(in-package :cepl.pipelines)

(defstruct (compile-context (:copier %copy-compile-context))
  (primitive (error "BUG: context without primitive"))
  (versions (error "BUG: context without versions"))
  (stage (error "BUG: context without stage"))
  (static-p (error "BUG: context without 'static' boolean")))

(defmethod make-load-form ((ctx compile-context) &optional environment)
  (declare (ignore environment))
  `(make-compile-context
    :primitive ,(compile-context-primitive ctx)
    :versions ',(compile-context-versions ctx)
    :stage ',(compile-context-stage ctx)
    :static-p ',(compile-context-static-p ctx)))

(defun copy-compile-context (compile-context
                             &key
                               (primitive nil p-set)
                               (versions nil v-set)
                               (stage nil g-set)
                               (static-p nil s-set))
  (let ((new (%copy-compile-context compile-context)))
    (when p-set
      (setf (compile-context-primitive new)
            (varjo.internals:primitive-name-to-instance primitive)))
    (when v-set
      (setf (compile-context-versions new)
            (uiop:ensure-list versions)))
    (when g-set
      (assert (find stage varjo:*stage-names*) ()
              'unknown-stage-kind
              :stage stage)
      (setf (compile-context-stage new)
            stage))
    (when s-set
      (setf (compile-context-static-p new)
            (not (null static-p))))
    new))

(defun parse-compile-context (name raw-context for)
  (assert (member for '(:function :pipeline :glsl-stage)))
  (labels ((is-version-p (v)
             (find v *supported-versions*))
           (get-prim-type (raw-context)
             (or (find-if #'varjo:valid-primitive-name-p raw-context)
                 :triangles)))
    (let* ((static-p (not (null (find :static raw-context))))
           (raw-context (if static-p
                            (remove :static raw-context)
                            raw-context))
           (primitive-symb (get-prim-type raw-context))
           (raw-context (if primitive-symb
                            (remove primitive-symb raw-context)
                            raw-context))
           (stage (first (intersection raw-context varjo:*stage-names*)))
           (raw-context (if stage
                            (remove stage raw-context)
                            raw-context))
           (primitive (varjo.internals:primitive-name-to-instance primitive-symb))
           (versions raw-context))
      (assert (every #'is-version-p versions)
              () 'unknown-symbols-in-pipeline-context
              :name name
              :full raw-context
              :issue (remove-if #'is-version-p versions)
              :for for)
      (when stage
        (assert (find stage varjo:*stage-names*) ()
                'unknown-stage-kind
                :stage stage)
        (assert (not (eq for :pipeline)) ()
                'stage-in-context-only-valid-for-glsl-stages
                :name name))
      (make-compile-context
       :primitive primitive
       :versions (uiop:ensure-list versions)
       :stage stage
       :static-p static-p))))
