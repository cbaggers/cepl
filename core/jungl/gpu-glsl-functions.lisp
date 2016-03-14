(in-package :jungl)


;; extract details from args and delegate to %def-gpu-function
;; for the main logic
(defmacro def-glsl-stage (name args body-string outputs)
  (let ((doc-string (when (stringp (first body)) (pop body)))
        (declarations (when (and (listp (car body)) (eq (caar body) 'declare))
                        (pop body))))
    ;; split the argument list into the categoried we care about
    (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
                 (instancing :&instancing))
        (varjo:lambda-list-split '(:&uniform :&context :&instancing) args)
      ;; check the arguments are sanely formatted
      (mapcar #'(lambda (x) (assert-arg-format name x)) in-args)
      (mapcar #'(lambda (x) (assert-arg-format name x)) uniforms)
      ;; now the meat
      (%def-gpu-function name in-args uniforms context body instancing
			 nil nil doc-string declarations))))

(defun %def-gpu-function (name in-args uniforms context body instancing
                          equivalent-inargs equivalent-uniforms
                          doc-string declarations)
  "This is the meat of defun-g. it is broken down as follows:

   [0] makes a gpu-func-spec that will be populated a stored later.

   [2] %test-&-update-spec compiles the code to check for errors and log
       dependencies. (this is called at runtime)

   [3] %make-gpu-func-spec is called at expand time to write a lisp function
       with the same signature as the gpu-function. This gives code hinting and
       also a decent error message if you try calling it from the cpu.

   [4] the purpose of %recompile-gpu-function is actually to recompile pipelines
       that depend on this gpu function. I should change the name. It does this
       by calling %recompile-gpu-function on all the gpu function that depend on
       this func and then the recompile-function for all pipelines that depend
       on this gpu function. To this end it walks depth first too all affected
       pipelines. (this also happens at runtime)

   [5] At runtime this looks for any gpu function that listed this function as
       one of it's missing dependencies and calls %test-&-update-spec on them.
       Note that this will (possibly) update the spec but will not trigger a
       recompile in the pipelines."
  (let ((spec (%make-gpu-func-spec name in-args uniforms context body instancing
                                   equivalent-inargs equivalent-uniforms nil nil
                                   doc-string declarations nil))) ;;[0]
    (let* ((in-arg-names (mapcar #'first in-args))
	   (uniform-names (mapcar #'first uniforms))
	   (macro-func-name (gensym (symbol-name name))))
      (%update-gpu-function-data spec nil nil)
      `(progn
         (%test-&-update-spec ,(%serialize-gpu-func-spec spec));;[2]
         ,(%make-stand-in-lisp-func-for-glsl-stage spec);;[3]
         (%recompile-gpu-function ',name);;[4]
	 (update-specs-with-missing-dependencies);;[5]
	 ',name))))

(defun %make-stand-in-lisp-func-for-glsl-stage (spec)
  "Makes a regular lisp function with the same names and arguments
  (where possible) as the gpu function who's spec is provided.

  If called the function will throw an error saying that the function
  can't currently be used from the cpu.

  This means we get function arg hints, doc-string and also we have the
  opportunity to provide a cpu implementation one day we want to."
  (with-gpu-func-spec spec
    (let ((arg-names (mapcar #'first in-args))
          (uniform-names (mapcar #'first uniforms)))
      `(defun ,name (,@arg-names
		     ,@(when uniforms (cons (symb :&key) uniform-names)))
	 ,@(when doc-string (list doc-string))
         (declare (ignore ,@arg-names ,@uniform-names))
         (warn "GPU Functions cannot currently be used from the cpu")))))

(def-glsl-stage test (("a" :float))
  "
   gl_Position = vec4(a, 0f, 0f, 1f);
   tex_coord = vec2(1,2);
  "
  (("tex_coord" :vec2)))
