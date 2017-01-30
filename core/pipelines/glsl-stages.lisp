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

;; extract details from args and delegate to %def-gpu-function
;; for the main logic
(defmacro def-glsl-stage (name args body-string outputs)
  ;; [0] makes a glsl-stage-spec that will be populated a stored later.

  ;; [1] use varjo to create a varjo-compile-result. We have to bodge a bit
  ;;     of the data but it's worth it to not duplicate varjo's logic

  ;; [2] %make-stand-in-lisp-func-for-glsl-stage is called at expand time to write
  ;;     a lisp function with the same signature as the glsl stage. This gives
  ;;     code hinting and also a decent error message if you try calling it
  ;;     cpu side.
  ;;
  ;; split the argument list into the categoried we care about
  (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
	       (instancing :&instancing))
      (varjo:lambda-list-split '(:&uniform :&context :&instancing) args)
    ;; check the arguments are sanely formatted
    (mapcar #'(lambda (x) (assert-arg-format name x)) in-args)
    (mapcar #'(lambda (x) (assert-arg-format name x)) uniforms)
    ;; check we can use the type from glsl cleanly
    (assert-glsl-stage-types in-args uniforms)
    ;; now the meat
    (assert-context name context)
    (let ((spec (%make-glsl-stage-spec;;[0]
		 name in-args uniforms context body-string
		 (glsl-stage-spec-to-varjo-compile-result ;;[1]
		  in-args uniforms outputs context body-string))))
      (%update-glsl-stage-data spec)
      `(progn
	 ,(%make-stand-in-lisp-func-for-glsl-stage spec);;[2]
	 (recompile-pipelines-that-use-this-as-a-stage ,(spec->func-key spec))
	 ',name))))

(defun assert-context (name context)
  (let ((allowed (and (some (lambda (s) (member s context))
			    varjo:*supported-stages*)
		      (some (lambda (s) (member s context))
			    varjo:*supported-versions*))))
    (unless allowed
      (error 'invalid-context-for-def-glsl-stage :name name :context context))))

(defun type-contains-structs (type)
  (let ((type (varjo:type-spec->type type)))
    (cond
      ((v-typep type 'v-user-struct) t)
      ((v-typep type 'v-array)
       (type-contains-structs (v-element-type type)))
      (t nil))))

(defun assert-glsl-stage-types (in-args uniforms)
  (labels ((check (x)
	     (when (type-contains-structs (second x))
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
  (labels ((name (x) (symb (string-upcase (first x)))))
    (with-glsl-stage-spec spec
      (let ((arg-names (mapcar #'name in-args))
	    (uniform-names (mapcar #'name uniforms)))
	`(defun ,name (,@arg-names
		       ,@(when uniforms (cons (symb :&key) uniform-names)))
	   (declare (ignore ,@arg-names ,@uniform-names))
	   (warn "GLSL stages cannot be used from the cpu"))))))

(defun glsl-stage-spec-to-varjo-compile-result
    (in-args uniforms outputs context body-string)
  "Here our goal is to simple reuse as much from varjo as possible.
   This will mean we have less duplication, even if things seem a little
   ugly here"
  (let* ((in-args (mapcar #'process-glsl-arg in-args))
         (uniforms (mapcar #'process-glsl-arg uniforms))
         (none-type (varjo:type-spec->type :none))
         (arg-types (mapcar #'type-spec->type
                            (append (mapcar #'second in-args)
                                    (mapcar #'second uniforms))))
         (stage (varjo:make-stage in-args uniforms context
                                  nil nil)))
    (first
     (multiple-value-list
      (varjo:flow-id-scope
	(let ((env (varjo::%make-base-environment)))
	  (pipe-> (stage env)
            #'varjo::set-env-context
	    #'varjo::process-in-args
	    #'varjo::process-uniforms
            #'(lambda (stage env)
                (values (make-instance
                         'varjo::compiled-function-result
                         :function-obj nil
                         :signatures nil
                         :ast (varjo:ast-node! :error nil none-type nil nil)
                         :used-types nil
                         :glsl-code body-string
                         :stemcells nil
                         :out-vars (make-varjo-outvars outputs env)
                         :used-types arg-types)
                        stage
                        env))
	    #'varjo::make-post-process-obj
            #'varjo::check-stemcells
            #'varjo::filter-used-items
	    ;;#'(lambda (_) (fill-in-post-proc _ body-string outputs))
	    #'varjo::gen-in-arg-strings
	    #'varjo::gen-out-var-strings
	    #'varjo::final-uniform-strings
	    #'varjo::final-string-compose
	    #'varjo::package-as-final-result-object)))))))

(defun process-glsl-arg (arg)
  (destructuring-bind (glsl-name type . qualifiers) arg
    (let ((name (symb (string-upcase glsl-name)))
	  (prefixed-glsl-name (format nil "@~a" glsl-name)))
      `(,name ,type ,@qualifiers ,prefixed-glsl-name))))

(defun make-varjo-outvars (outputs env)
  (loop :for (glsl-name type . qualifiers) :in outputs :collect
     (let ((name (symb (string-upcase glsl-name))))
       `(,name
	 ,qualifiers
	 ,(varjo::v-make-value
           (varjo:type-spec->type type (varjo:flow-id!))
           env :glsl-name glsl-name)))))
