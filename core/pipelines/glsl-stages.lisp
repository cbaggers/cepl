(in-package :cepl.pipelines)

;;----------------------------------------------------------------------
;; The code in this file enables def-glsl-stage.
;; An example of its usage is:

;; (def-glsl-stage test (("a" :float) &context :vertex :330)
;;   "void main ()
;;    {
;;        gl_Position = vec4(a, 0f, 0f, 1f);
;;        tex_coord = vec2(1,2);
;;    }
;;   "
;;   (("tex_coord" :vec2)))

;;----------------------------------------------------------------------

(defun get-body-string (body)
  (cond
    ((stringp body) body)
    ((and (listp body)
          (eq (first body) :file)
          (and (or (stringp (second body))
                   (symbolp (second body)))
               (constantp (second body)))
          (stringp (third body)))
     (let ((fspec (asdf:system-relative-pathname (second body) (third body))))
       (alexandria:read-file-into-string fspec)))
    (t (error "def-glsl-stage: Invalid shader body ~a" body))))

;; extract details from args and delegate to %def-gpu-function
;; for the main logic
(defmacro def-glsl-stage (name args body-form outputs)
  ;; [0] makes a glsl-stage-spec that will be populated a stored later.

  ;; [1] use varjo to create a compiled-stage from the glsl

  ;; [2] %make-stand-in-lisp-func-for-glsl-stage is called at expand time to
  ;;     write a lisp function with the same signature as the glsl stage.
  ;;     This gives code hinting and also a decent error message if you try
  ;;     calling it cpu side.
  ;;
  ;; split the argument list into the categoried we care about
  (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
               (instancing :&instancing))
      (varjo.utils:lambda-list-split '(:&uniform :&context :&instancing) args)
    ;; check the arguments are sanely formatted
    (mapcar #'(lambda (x) (assert-glsl-arg-format name x)) in-args)
    (mapcar #'(lambda (x) (assert-glsl-arg-format name x)) uniforms)
    ;; check we can use the type from glsl cleanly
    (assert-glsl-stage-types in-args uniforms)
    ;; now the meat
    (assert-context name context)
    (let* ((cepl-in-args (mapcar #'process-glsl-arg in-args))
           (cepl-uniforms (mapcar #'process-glsl-arg uniforms))
           (body-string (get-body-string body-form))
           (stage-kind (varjo.internals:get-stage-kind-from-context context))
           (context (remove stage-kind context))
           (spec (%make-glsl-stage-spec ;;[0]
                  name cepl-in-args cepl-uniforms context body-string
                  (varjo.internals:glsl-to-compile-result ;;[1]
                   stage-kind in-args uniforms outputs context body-string))))
      (%update-glsl-stage-data spec)
      `(progn
         ,(%make-stand-in-lisp-func-for-glsl-stage spec);;[2]
         (recompile-pipelines-that-use-this-as-a-stage ,(spec->func-key spec))
         ',name))))

(defun assert-context (name context)
  (let ((allowed (and (some (lambda (s) (member s context))
                            varjo:*stage-names*)
                      (some (lambda (s) (member s context))
                            varjo:*supported-versions*))))
    (unless allowed
      (error 'invalid-context-for-def-glsl-stage :name name :context context))))

(defun type-contains-structs (type)
  (cond
    ((v-typep type 'v-user-struct) t)
    ((v-typep type 'v-array)
     (type-contains-structs (v-element-type type)))
    (t nil)))

(defun assert-glsl-stage-types (in-args uniforms)
  (labels ((check (x)
             (when (type-contains-structs (varjo:type-spec->type (second x)))
               (first x))))
    (let* ((struct-args
            (remove nil (mapcar #'check (append in-args uniforms)))))
      (when struct-args
        (error 'struct-in-glsl-stage-args :arg-names struct-args)))))

(defun %make-stand-in-lisp-func-for-glsl-stage (spec)
  "Makes a regular lisp function with the same names and arguments
  (where possible) as the glsl-stage who's spec is provided.

  If called the function will throw an error saying that the function
  can't currently be used from the cpu."
  (with-glsl-stage-spec spec
    (let ((arg-names (mapcar #'first in-args))
          (uniform-names (mapcar #'first uniforms)))
      `(defun ,name (,@arg-names
                     ,@(when uniforms (cons (symb :&key) uniform-names)))
         (declare (ignore ,@arg-names ,@uniform-names))
         (warn "GLSL stages cannot be used from the cpu")))))

(defun process-glsl-arg (arg)
  (destructuring-bind (glsl-name type . qualifiers) arg
    (let ((name (symb (string-upcase glsl-name))))
      `(,name ,type ,@qualifiers ,glsl-name))))

(defun assert-glsl-arg-format (name arg)
  (destructuring-bind (glsl-name type . qualifiers) arg
    (assert (and (stringp glsl-name)
                 (varjo:type-specp type)
                 (every #'keywordp qualifiers))
            () 'invalid-inline-glsl-stage-arg-layout
            :name name :arg arg)))
