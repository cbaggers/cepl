(in-package :cgl)

;; extract details from args and delegate to %def-gpu-function
;; for the main logic
(defmacro defun-g (name args &body body)
  "Define a function that runs on the gpu. "
  (let ((doc-string (when (stringp (first body)) (pop body)))
        (declarations (when (and (listp (car body)) (eq (caar body) 'declare))
                        (pop body))))
    (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
                 (instancing :&instancing))
        (varjo:lambda-list-split '(:&uniform :&context :&instancing) args)
      (mapcar #'(lambda (x) (assert-arg-format name x)) in-args)
      (mapcar #'(lambda (x) (assert-arg-format name x)) uniforms)
      (let* ((in-args (swap-equivalent-types in-args))
	     (uniforms (swap-equivalent-types uniforms))
	     (equivalent-inargs (mapcar (lambda (_)
					  (when (equivalent-typep _) _))
					(mapcar #'second in-args)))
	     (equivalent-uniforms (mapcar (lambda (_)
					    (when (equivalent-typep _) _))
					  (mapcar #'second uniforms))))
        (assert (every #'null equivalent-inargs))
        (%def-gpu-function name in-args uniforms context body instancing
                           equivalent-inargs equivalent-uniforms
                           doc-string declarations)))))

(defun assert-arg-format (gfunc-name x)
  (unless (listp x)
    (error 'gfun-invalid-arg-format :gfun-name gfunc-name :invalid-pair x))
  x)

(defun undefine-gpu-function (name)
  (%unsubscibe-from-all name)
  (remhash name *gpu-func-specs*)
  (remhash name *dependent-gpu-functions*)
  nil)

(defvar *warn-when-cant-test-compile* t)

(defun %test-&-update-spec (spec)
  (with-gpu-func-spec spec
    (setf missing-dependencies nil)
    (%update-gpu-function-data
     spec
     (handler-case
    	 (varjo::with-stemcell-infer-hook
    	     #'try-guessing-a-varjo-type-for-symbol
    	   (remove-if-not
    	    #'gpu-func-spec
    	    (varjo::used-macros
    	     (v-translate in-args uniforms
			  (union '(:vertex :fragment :iuniforms :330)
				 context)
			  `(progn ,@body)
			  (%get-passes)))))
       (varjo::could-not-find-function (e)
    	 (setf (slot-value spec 'missing-dependencies)
    	       (list (slot-value e 'varjo::name))))))))



;;--------------------------------------------------

(defun %def-gpu-function (name in-args uniforms context body instancing
                          equivalent-inargs equivalent-uniforms
                          doc-string declarations)
  (let ((spec (%make-gpu-func-spec name in-args uniforms context body instancing
                                   equivalent-inargs equivalent-uniforms
                                   doc-string declarations nil)))
    ;; this gets the functions used in the body of this function
    ;; it is *not* recursive
    (let* ((in-arg-names (mapcar #'first in-args))
	   (uniform-names (mapcar #'first uniforms))
	   (macro-func-name (gensym))
	   (fbody `(list 'varjo::labels-no-implicit
			 '((,macro-func-name (,@in-args ,@uniforms) ,@body))
			 (list ',macro-func-name
			       ,@in-arg-names ,@uniform-names)))
	   (to-compile `(lambda (,@in-arg-names ,@uniform-names)
			  ,fbody)))
      ;;(print to-compile)
      (varjo::add-macro name (compile nil to-compile)
       context varjo::*global-env*)
      `(progn
         (%test-&-update-spec ,(%serialize-gpu-func-spec spec))
         ,(%make-stand-in-lisp-func spec)
         (%recompile-gpu-function ',name)
	 (update-specs-with-missing-dependencies)
	 ',name))))

(defun %recompile-gpu-function (name)
  ;; recompile gpu-funcs that depends on name
  (mapcar #'%recompile-gpu-function (funcs-that-use-this-func name))
  ;; and recompile pipelines that depend on name
  (mapcar (lambda (_)
            (let ((recompile-pipeline-name (recompile-name _)))
	      (when (fboundp recompile-pipeline-name)
		(funcall (symbol-function recompile-pipeline-name)))))
          (pipelines-that-use-this-func name)))


(defun %subscribe-to-gpu-func (name subscribe-to-name)
  (assert (not (eq name subscribe-to-name)))
  (symbol-macrolet ((func-specs (funcs-that-use-this-func subscribe-to-name)))
    (when (and (gpu-func-spec subscribe-to-name)
               (not (member name func-specs)))
      (format t "; func ~s subscribed to ~s~%" name subscribe-to-name)
      (push name func-specs))))

(defun %unsubscibe-from-all (name)
  (labels ((%remove-gpu-function-from-dependancy-table (func-name dependencies)
             (when (member name dependencies)
               (setf (funcs-that-use-this-func func-name)
                     (remove name dependencies)))))
    (maphash #'%remove-gpu-function-from-dependancy-table
             *dependent-gpu-functions*)))

(defun %update-gpu-function-data (spec depends-on)
  "Add or update the spec and also (re)subscribe to all the dependencies"
  (with-slots (name) spec
    (%unsubscibe-from-all name)
    (mapcar (lambda (_) (%subscribe-to-gpu-func name _)) depends-on)
    (setf (gpu-func-spec name) spec)))

(defun %gpu-func-compiles-in-some-context (spec)
  (declare (ignorable spec))
  t)

(defun %expand-all-macros (spec)
  (with-gpu-func-spec spec
    (let ((env (varjo::%make-base-environment)))
      (%%expand-all-macros body context env))))

(defun %%expand-all-macros (body context env)
  (varjo:pipe-> (nil nil context body env)
    #'varjo::split-input-into-env
    #'varjo::process-context
    (equal #'varjo::symbol-macroexpand-pass
           #'varjo::macroexpand-pass
           #'varjo::compiler-macroexpand-pass)))

(defun varjo-func-namep (x)
  (assert (symbolp x))
  (or (second (multiple-value-list (gethash x varjo::*global-env-funcs*)))
      (second (multiple-value-list (gethash (kwd x) varjo::*global-env-funcs*)))))

(defun %make-stand-in-lisp-func (spec)
  (with-gpu-func-spec spec
    (let ((arg-names (mapcar #'first in-args))
          (uniform-names (mapcar #'first uniforms)))
      `(defun ,name (,@arg-names
                     ,@(when uniforms (cons (symb :&key) uniform-names) ))
         (declare (ignore ,@arg-names ,@uniform-names))
         (warn "GPU Functions cannot currently be used from the cpu")))))

(defun %find-recursion (name depends-on)
  (declare (ignore name depends-on))
  nil)

;;--------------------------------------------------

(defun %aggregate-uniforms (uniforms &optional accum)
  (if uniforms
      (let ((u (first uniforms)))
        (cond
          ;; if there is no other uniform with matching name, hense no collision
          ((not (find (first u) accum :test #'equal :key #'first))
           (%aggregate-uniforms (rest uniforms) (cons u accum)))
          ;; or the uniform matches perfectly so no collision
          ((find u accum :test #'equal)
           (%aggregate-uniforms (rest uniforms) accum))
          ;; or it's a clash
          (t (error "Uniforms for the functions are incompatible: ~a ~a"
                    u accum))))
      accum))

(defun %uniforms-pre-equiv (spec)
  (with-gpu-func-spec spec
    (mapcar (lambda (_ _1)
              (if _ `(,(car _1) ,_ ,@(cddr _1)) _1))
            equivalent-uniforms uniforms)))

(defun aggregate-uniforms (names &optional accum)
  (if names
      (aggregate-uniforms
       (rest names)
       (%aggregate-uniforms (%uniforms-pre-equiv (gpu-func-spec (first names)))
                            accum))
      accum))

(defun aggregate-in-args (names &optional (args-accum t))
  (if names
      (with-gpu-func-spec (gpu-func-spec (first names))
        (aggregate-in-args
         (rest names)
         (if (eql t args-accum) in-args args-accum)))
      args-accum))

(defmacro with-processed-func-specs (names &body body)
  `(let* ((in-args (aggregate-in-args ,names))
          (uniforms (aggregate-uniforms ,names)))
     ,@body))

;;--------------------------------------------------

(defun get-func-as-stage-code (name)
  (with-gpu-func-spec (gpu-func-spec name)
    (list in-args uniforms context body)))

;;--------------------------------------------------

(defun %varjo-compile-as-pipeline (parsed-gpipe-args)
  (varjo::with-stemcell-infer-hook #'try-guessing-a-varjo-type-for-symbol
    (v-rolling-translate
     (mapcar #'parsed-gpipe-args->v-translate-args parsed-gpipe-args)
     (%get-passes))))

(defun parsed-gpipe-args->v-translate-args (stage-pair)
  (dbind (stage-type . stage-name) stage-pair
    (dbind (in-args uniforms context code) (get-func-as-stage-code stage-name)
      ;; ensure context doesnt specify a stage or that it matches
      (let ((n (count-if (lambda (_)
			   (member _ varjo::*stage-types*))
			 context)))
	(assert (and (<= n 1) (if (= n 1) (member stage-type context) t))))
      (let ((context (cons :iuniforms
			   (cons stage-type
				 (remove stage-type context)))))
	(list in-args
	      uniforms
	      context
	      `(progn ,@code))))))

;;--------------------------------------------------

(defun parse-gpipe-args (args)
  (let ((cut-pos (or (position :context args) (length args))))
    (destructuring-bind (&key context) (subseq args cut-pos)
      (list
       (let ((args (subseq args 0 cut-pos)))
         (if (and (= (length args) 2) (not (some #'keywordp args)))
             `((:vertex . ,(cadar args)) (:fragment . ,(cadadr args)))
             (let* ((stages (copy-list varjo:*stage-types*))
                     (results (loop :for a :in args
                                :if (keywordp a) :do (setf stages (cons a (subseq stages (1+ (position a stages)))))
                                :else :collect (cons (or (pop stages) (error "Invalid gpipe arguments, no more stages"))
                                                     (cadr a)))))
               (remove :context results :key #'car))))
       context))))

;;--------------------------------------------------

(defun try-guessing-a-varjo-type-for-symbol (s)
  ;; only works on specials because of symbol-value
  (when (boundp s)
    (guess-a-varjo-type (symbol-value s))))

(defun guess-a-varjo-type (x)
  (typecase x
    (number (guess-a-varjo-number-type x))
    (array (guess-a-varjo-array-type x))
    (boolean (guess-a-varjo-bool-type x))
    (t (error "Cant guess a suitable type for ~s" x))))

(defun guess-a-varjo-bool-type (x)
  (if (eql x t)
      :bool
      (error "Cant guess a suitable type for ~s" x)))

(defun guess-a-varjo-array-type (x)
  (typecase x
    ((simple-array single-float (2)) :vec2)
    ((simple-array single-float (3)) :vec3)
    ((simple-array single-float (4)) :vec4)
    ((simple-array single-float (9)) :mat3)
    ((simple-array single-float (16)) :mat4)))

(defun guess-a-varjo-number-type (x)
  (typecase x
    ((or single-float double-float) (guess-a-varjo-float-type x))
    (integer (guess-a-varjo-integer-type x))
    (t (error "Cant guess a suitable type for ~s" x))))

(defun guess-a-varjo-float-type (x)
  (if (typep x 'single-float)
      :float
      :double))

(defun guess-a-varjo-integer-type (x)
  (typecase x
    ((signed-byte 32) :int)
    ((unsigned-byte 32) :uint)
    (t (error "Cant guess a suitable type for ~s" x))))
