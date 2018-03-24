(in-package :cepl.pipelines)

;;------------------------------------------------------------

(defclass gpu-lambda ()
  ((in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (body :initarg :body)
   (instancing :initarg :instancing)
   (doc-string :initarg :doc-string)
   (declarations :initarg :declarations)
   (context :initarg :context)
   (func-spec :initarg :func-spec :initform nil))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod lambda-g->func-spec ((lambda-g gpu-lambda))
  (slot-value lambda-g 'func-spec))

(defmethod initialize-instance :after ((lambda-g gpu-lambda) &key)
  ;; need to emit warning if called
  (closer-mop:set-funcallable-instance-function lambda-g #'%lambda-g)
  ;; need to make the func-spec so can be used in pipelines
  (with-slots (in-args uniforms body instancing doc-string
                       declarations context func-spec) lambda-g
    (setf func-spec
          (%test-&-process-spec
           (%make-gpu-func-spec
            nil in-args uniforms context body instancing nil nil
            nil doc-string declarations nil (get-gpu-func-spec-tag))
           :cache-spec nil))))

(defun+ %lambda-g (&rest args)
  (declare (ignore args))
  (warn "GPU Functions cannot currently be used from the cpu"))

;;------------------------------------------------------------

(defun+ make-gpu-lambda  (args body)
  ;; seperate any doc-string or declarations from the body
  (let ((doc-string (when (stringp (first body)) (pop body))))
    ;; split the argument list into the categoried we care aboutn
    (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
                 (instancing :&instancing))
        (varjo.utils:lambda-list-split '(:&uniform :&context :&instancing) args)
      ;; check the arguments are sanely formatted
      (mapcar #'(lambda (x) (assert-arg-format nil x)) in-args)
      (mapcar #'(lambda (x) (assert-arg-format nil x)) uniforms)
      (make-instance 'gpu-lambda
                     :in-args in-args
                     :uniforms uniforms
                     :body body
                     :instancing instancing
                     :doc-string doc-string
                     :declarations nil
                     :context context))))

(defmacro lambda-g (args &body body)
  (make-gpu-lambda args body)
  `(make-gpu-lambda ',args ',body))

(defun compile-g (name &optional definition)
  (assert (and (not name) (eq (first definition) 'lambda-g)) ()
          'compile-g-missing-requested-feature :form (cons name definition))
  (destructuring-bind (l args &body body) definition
    (declare (ignore l))
    (make-gpu-lambda args body)))

(defun lambda-g->varjo-lambda-code (glambda)
  (with-slots (in-args uniforms body) glambda
    `(lambda (,@in-args ,@(when uniforms (cons '&uniform uniforms)))
       ,@body)))

;;------------------------------------------------------------

(defmacro pipeline-g (context &body gpipe-args)
  (labels ((unfunc (x)
             (if (and (listp x) (eq (first x) 'function))
                 `(quote ,(second x))
                 x)))
    (let ((args (mapcar #'unfunc gpipe-args)))
      `(the function (make-lambda-pipeline (list ,@args) ',context)))))

#+nil
(defun+ example ()
  (pipeline-g nil
    '(cepl.misc::draw-texture-vert :vec4)
    '(cepl.misc::draw-texture-frag :vec2)))

;;------------------------------------------------------------

(defun+ make-lambda-pipeline (gpipe-args context-with-primitive)
  ;; we have the body of the work in the *-inner function as
  ;; make-complete-lambda-pipeline returns two values and whilst we
  ;; do want both for funcall-g, we only want the first value to
  ;; be returned to users who use lambda-g
  (values (make-lambda-pipeline-inner gpipe-args context-with-primitive)))

(defun+ make-lambda-pipeline-inner (gpipe-args context-with-primitive)
  (destructuring-bind (stage-pairs post) (parse-gpipe-args gpipe-args)
    (let* ((func-specs (mapcar #'cdr stage-pairs)))
      (if (stages-require-partial-pipeline func-specs)
          (make-partial-lambda-pipeline func-specs)
          (make-complete-lambda-pipeline context-with-primitive
                                         stage-pairs
                                         func-specs
                                         post)))))

(defun+ make-partial-lambda-pipeline (func-specs)
  (let ((stages (remove-if-not (lambda (x)
                                 (with-gpu-func-spec x
                                   (some #'function-arg-p uniforms)))
                               func-specs)))
    (error 'partial-lambda-pipeline
           :partial-stages stages)))

(defun get-primitive-type-from-context (context)
  (or (find-if #'varjo:valid-primitive-name-p
               context)
      :triangles))

(defun+ make-complete-lambda-pipeline (context-with-primitive
                                       stage-pairs
                                       func-specs
                                       post)
  (let* ((aggregate-uniforms (aggregate-uniforms func-specs t))
         (primitive (varjo.internals:primitive-name-to-instance
                     (get-primitive-type-from-context
                      context-with-primitive))))
    (multiple-value-bind (compiled-stages
                          prog-id
                          prog-ids
                          tfb-group-count)
        (%compile-link-and-upload nil
                                  primitive
                                  stage-pairs)
      (declare (ignore prog-ids))
      ;;
      (let* ((ctx *pipeline-body-context-var*)
             ;; handle implicit uniforms here so we dont need to have an
             ;; 'implicit uniform uploader'
             (varjo-implicit
              (remove-if #'varjo:ephemeral-p
                         (mapcat #'varjo:implicit-uniforms compiled-stages)))
             (implicit-uniform-arg-forms
              (mapcar #'varjo.internals:to-arg-form varjo-implicit))
             (implicit-uniform-assigners
              (make-arg-assigners implicit-uniform-arg-forms))
             (implicit-uniform-transforms
              (remove-duplicates
               (mapcar (lambda (x)
                         (list (varjo:name x)
                               (varjo.internals:cpu-side-transform x)))
                       varjo-implicit)
               :test #'equal))
             (implicit-u-uploads
              (mapcar #'gen-uploaders-block implicit-uniform-assigners))
             (implicit-u-lets
              (mapcat #'let-forms implicit-uniform-assigners))
             ;;
             (uniform-assigners
              (make-arg-assigners aggregate-uniforms))
             ;; we generate the func that compiles & uploads the pipeline
             ;; and also populates the pipeline's local-vars
             (uniform-names
              (mapcar #'first (aggregate-uniforms func-specs)))
             (u-uploads
              (mapcar #'gen-uploaders-block uniform-assigners))
             (u-cleanup
              (mapcar #'gen-cleanup-block (reverse uniform-assigners)))
             (u-lets
              (mapcat #'let-forms uniform-assigners))
             ;;
             (compute (find :compute stage-pairs :key #'car))
             (stream-symb (if compute 'space 'stream))
             (stream-type (if compute 'compute-space 'buffer-stream)))
        ;;
        (values
         (funcall (compile nil (gen-complete-lambda-pipeline-code
                                ctx
                                compute
                                implicit-u-lets
                                implicit-u-uploads
                                implicit-uniform-transforms
                                post
                                primitive
                                stream-symb
                                stream-type
                                u-cleanup
                                u-lets
                                u-uploads
                                uniform-names))
                  compiled-stages
                  prog-id
                  tfb-group-count)
         compiled-stages)))))

(defun gen-complete-lambda-pipeline-code (ctx
                                          compute
                                          implicit-u-lets
                                          implicit-u-uploads
                                          implicit-uniform-transforms
                                          post
                                          primitive
                                          stream-symb
                                          stream-type
                                          u-cleanup
                                          u-lets
                                          u-uploads
                                          uniform-names)
  `(lambda (compiled-stages prog-id tfb-group-count)
     (use-program (cepl-context) prog-id)
     (register-lambda-pipeline
      prog-id
      compiled-stages
      (let* ( ;; all image units will be >0 as 0 is used as scratch tex-unit
             (image-unit 0)
             ;; The primitive used by transform feedback. When nil
             ;; the primitive comes from the render-mode
             (tfs-primitive (when (> tfb-group-count 0)
                              (get-transform-feedback-primitive compiled-stages)))
             (tfs-array-count tfb-group-count)
             ;; If there are no implicit-uniforms we need a no-op
             ;; function to call
             (has-fragment-stage
              (not (null (find-if (lambda (x)
                                    (typep x 'compiled-fragment-stage))
                                  compiled-stages))))
             ;;
             ;; {todo} explain
             ,@(mapcar (lambda (x)
                         `(,(assigner-name x) ,(assigner-body x)))
                       u-lets)
             ,@(mapcar (lambda (x)
                         `(,(assigner-name x) ,(assigner-body x)))
                       implicit-u-lets))
        (declare (ignorable image-unit
                            tfs-primitive
                            tfs-array-count
                            has-fragment-stage)
                 (type symbol tfs-primitive)
                 (type (unsigned-byte 8) tfs-array-count)
                 ,@(mapcar (lambda (x)
                             `(type ,(assigner-type x) ,(assigner-name x)))
                           implicit-u-lets))
        (use-program (cepl-context) 0)
        ;;
        ;; generate the code that actually renders
        (%post-init ,post)
        (lambda (,ctx ,stream-symb ,@(when uniform-names `(&key ,@uniform-names)))
          (declare (optimize (speed 3) (safety 1))
                   (type (or null ,stream-type) ,stream-symb)
                   (ignorable ,ctx ,@uniform-names))
          #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          ,@(unless (or compute (typep primitive 'varjo::dynamic))
                    `((when ,stream-symb
                        (assert
                         (= ,(draw-mode-group-id primitive)
                            (buffer-stream-primitive-group-id ,stream-symb))
                         ()
                         'buffer-stream-has-invalid-primitive-for-stream
                         :name "<lambda>"
                         :pline-prim ',(varjo::lisp-name primitive)
                         :stream-prim (buffer-stream-primitive ,stream-symb)))))
          (let ,implicit-uniform-transforms
            (use-program ,ctx prog-id)
            ,@u-uploads
            ,@implicit-u-uploads)
          (when ,stream-symb
            ,(if compute
                 (compute-expander nil stream-symb)
                 (draw-expander nil ctx stream-symb 'draw-mode primitive)))
          ,@u-cleanup
          ,(if compute
               nil
               `(draw-fbo-bound ,ctx)))))))

;;------------------------------------------------------------

(defun+ register-lambda-pipeline (prog-id compiled-stages closure)
  (setf (function-keyed-pipeline closure)
        (make-lambda-pipeline-spec prog-id compiled-stages))
  closure)

;;------------------------------------------------------------

(defmethod pull-g ((object gpu-lambda))
  (let ((vresult (pull1-g object)))
    (when vresult
      (varjo:glsl-code vresult))))

(defmethod pull1-g ((object gpu-lambda))
  (with-slots (func-spec) object
    (let ((compiled (slot-value func-spec 'cached-compile-results)))
      (if compiled
          compiled
          (warn 'func-keyed-pipeline-not-found
                :callee 'pull-g :func object)))))
