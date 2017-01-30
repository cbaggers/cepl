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

(defmethod glambda->func-spec ((glambda gpu-lambda))
  (slot-value glambda 'func-spec))

(defmethod initialize-instance :after ((glambda gpu-lambda) &key)
  ;; need to emit warning if called
  (closer-mop:set-funcallable-instance-function
   glambda #'fallback-g-lambda-body)
  ;; need to make the func-spec so can be used in pipelines
  (with-slots (in-args uniforms body instancing doc-string
                       declarations context func-spec) glambda
    (setf func-spec
          (%test-&-update-spec
           (%make-gpu-func-spec
            nil in-args uniforms context body instancing nil nil
            nil doc-string declarations nil)))))

(defun fallback-g-lambda-body (&rest args)
  (declare (ignore args))
  (warn "GPU Functions cannot currently be used from the cpu"))

;;------------------------------------------------------------

(defun make-gpu-lambda  (args body)
  "Define a function that runs on the gpu."
  ;; The code here splits and validates the arguments but the meat
  ;; of gpu function definition happens in the %def-gpu-function call
  ;; at the tail
  ;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
  ;; seperate any doc-string or declarations from the body
  (let ((doc-string (when (stringp (first body)) (pop body)))
        (declarations (when (and (listp (car body)) (eq (caar body) 'declare))
                        (pop body))))
    ;; split the argument list into the categoried we care aboutn
    (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
                 (instancing :&instancing))
        (varjo:lambda-list-split '(:&uniform :&context :&instancing) args)
      ;; check the arguments are sanely formatted
      (mapcar #'(lambda (x) (assert-arg-format nil x)) in-args)
      (mapcar #'(lambda (x) (assert-arg-format nil x)) uniforms)
      (make-instance 'gpu-lambda
                     :in-args in-args
                     :uniforms uniforms
                     :body body
                     :instancing instancing
                     :doc-string doc-string
                     :declarations declarations
                     :context context))))

(defmacro glambda (args &body body)
  (make-gpu-lambda args body))

;;------------------------------------------------------------

(defun make-lambda-pipeline (gpipe-args context)
  (destructuring-bind (stage-pairs post) (parse-gpipe-args gpipe-args)
    (let* ((stage-keys (mapcar #'cdr stage-pairs))
	   (aggregate-uniforms (aggregate-uniforms stage-keys nil t))
	   (uniform-assigners (mapcar #'make-arg-assigners aggregate-uniforms))
           ;; we generate the func that compiles & uploads the pipeline
           ;; and also populates the pipeline's local-vars
           (uniform-names (mapcar #'first (aggregate-uniforms stage-keys)))
           (prim-type (varjo:get-primitive-type-from-context context))
           (u-uploads (mapcar #'gen-uploaders-block uniform-assigners))
           (u-cleanup (mapcar #'gen-cleanup-block (reverse uniform-assigners)))
           (u-lets (mapcat #'let-forms uniform-assigners)))
      `(multiple-value-bind (compiled-stages prog-id)
           (%compile-link-and-upload nil ,(serialize-stage-pairs stage-pairs))
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
              (use-program prog-id)
              ,@u-uploads
              (locally (declare (optimize (speed 3) (safety 1)))
                (funcall implicit-uniform-upload-func prog-id
                         ,@uniform-names))
              (when stream (draw-expander stream ,prim-type))
              (use-program 0)
              ,@u-cleanup
              stream)))))))

(defun make-n-compile-lambda-pipeline (gpipe-args context)
  (let ((code (make-lambda-pipeline gpipe-args context)))
    (funcall (compile nil `(lambda () ,code)))))

;;------------------------------------------------------------

(defmacro g-> (context &body gpipe-args)
  (labels ((unfunc (x)
             (if (and (listp x) (eq (first x) 'function))
                 `(quote ,(second x))
                 x)))
    (let ((args (mapcar #'unfunc gpipe-args)))
      (if (every #'constantp args)
          (make-lambda-pipeline gpipe-args context)
          `(make-n-compile-lambda-pipeline (list ,@args) ',context)))))

#+nil
(defun example ()
  (g-> nil
    #'cepl.misc::draw-texture-vert
    #'cepl.misc::draw-texture-frag))

;;------------------------------------------------------------

(defun register-lambda-pipeline (compiled-stages closure)
  (setf (function-keyed-pipeline closure)
        (make-lambda-pipeline-spec compiled-stages))
  closure)

;;------------------------------------------------------------
