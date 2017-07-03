(in-package :cepl.pipelines)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defclass gpu-lambda ()
  ((in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (body :initarg :body)
   (instancing :initarg :instancing)
   (doc-string :initarg :doc-string)
   (declarations :initarg :declarations)
   (context :initarg :context)
   (func-spec :initform nil))
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
          (%test-&-update-spec
           (%make-gpu-func-spec
            nil in-args uniforms context body instancing nil nil
            nil doc-string declarations nil)))))

(defun2 %lambda-g (&rest args)
  (declare (ignore args))
  (warn "GPU Functions cannot currently be used from the cpu"))

;;------------------------------------------------------------

(defun2 make-gpu-lambda  (args body)
  "Define a function that runs on the gpu."
  ;; The code here splits and validates the arguments but the meat
  ;; of gpu function definition happens in the %def-gpu-function call
  ;; at the tail
  ;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
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
  (make-gpu-lambda args body))

(defmacro glambda (args &body body)
  `(lambda-g ,args ,@body))

;;------------------------------------------------------------

(defun2 make-lambda-pipeline (gpipe-args context)
  (destructuring-bind (stage-pairs post) (parse-gpipe-args gpipe-args)
    (let* ((stage-keys (mapcar #'cdr stage-pairs))
           (aggregate-uniforms (aggregate-uniforms stage-keys nil t)))
      (if (stages-require-partial-pipeline stage-keys)
          (make-partial-lambda-pipeline stage-keys)
          (make-complete-lambda-pipeline
           stage-pairs stage-keys aggregate-uniforms context post)))))

(defun2 make-partial-lambda-pipeline (stage-keys)
  (let ((stages (remove-if-not λ(with-gpu-func-spec (gpu-func-spec _)
                                  (some #'function-arg-p uniforms))
                               stage-keys)))
    (error 'partial-lambda-pipeline
           :partial-stages stages)))

(defun2 make-complete-lambda-pipeline (stage-pairs stage-keys aggregate-uniforms
                                      context post)
  (let* ((uniform-assigners (mapcar #'make-arg-assigners aggregate-uniforms))
         ;; we generate the func that compiles & uploads the pipeline
         ;; and also populates the pipeline's local-vars
         (uniform-names (mapcar #'first (aggregate-uniforms stage-keys)))
         (u-uploads (mapcar #'gen-uploaders-block uniform-assigners))
         (u-cleanup (mapcar #'gen-cleanup-block (reverse uniform-assigners)))
         (u-lets (mapcat #'let-forms uniform-assigners))
         (primitive (varjo.internals:primitive-name-to-instance
                     (varjo.internals:get-primitive-type-from-context context))))
    `(multiple-value-bind (compiled-stages prog-id)
         (%compile-link-and-upload nil ,primitive ,(serialize-stage-pairs stage-pairs))
       (register-lambda-pipeline
        compiled-stages
        (let* ((image-unit -1)
               ;; If there are no implicit-uniforms we need a no-op
               ;; function to call
               (implicit-uniform-upload-func
                (or (%create-implicit-uniform-uploader compiled-stages
                                                       ',uniform-names)
                    #'fallback-iuniform-func))
               ;;
               ;; {todo} explain
               ,@u-lets)
          (declare (ignorable image-unit)
                   (type function implicit-uniform-upload-func))
          ;;
          ;; generate the code that actually renders
          (%post-init ,post)
          (lambda (mapg-context stream ,@(when uniform-names `(&key ,@uniform-names)))
            (declare (optimize (speed 3) (safety 1))
                     (ignore mapg-context) (ignorable ,@uniform-names))
            #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            ,@(unless (typep primitive 'varjo::dynamic)
                      `((when stream
                          (assert
                           (= ,(draw-mode-group-id primitive)
                              (buffer-stream-primitive-group-id stream))
                           ()
                           'buffer-stream-has-invalid-primtive-for-stream
                           :name "<lambda>"
                           :pline-prim ',(varjo::lisp-name primitive)
                           :stream-prim (buffer-stream-primitive stream)))))
            (use-program prog-id)
            ,@u-uploads
            (funcall implicit-uniform-upload-func prog-id ,@uniform-names)
            (when stream (draw-expander stream ,primitive))
            ,@u-cleanup
            stream))))))

(defun2 make-n-compile-lambda-pipeline (gpipe-args context)
  (let ((code (make-lambda-pipeline gpipe-args context)))
    (funcall (compile nil `(lambda () ,code)))))

;;------------------------------------------------------------

(defmacro pipeline-g (context &body gpipe-args)
  (labels ((unfunc (x)
             (if (and (listp x) (eq (first x) 'function))
                 `(quote ,(second x))
                 x)))
    (let ((args (mapcar #'unfunc gpipe-args)))
      (if (every #'constantp args)
          (make-lambda-pipeline gpipe-args context)
          `(make-n-compile-lambda-pipeline (list ,@args) ',context)))))

(defmacro g-> (context &body gpipe-args)
  `(pipeline-g ,context ,@gpipe-args))

#+nil
(defun2 example ()
  (g-> nil
    #'cepl.misc::draw-texture-vert
    #'cepl.misc::draw-texture-frag))

;;------------------------------------------------------------

(defun2 register-lambda-pipeline (compiled-stages closure)
  (setf (function-keyed-pipeline closure)
        (make-lambda-pipeline-spec compiled-stages))
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
