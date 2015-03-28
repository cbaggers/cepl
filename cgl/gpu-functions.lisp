(in-package :cgl)
(named-readtables:in-readtable fn_::fn_lambda)

;; defun-gpu is at the bottom of this file

;;--------------------------------------------------

;; extract details from args and delegate to %def-gpu-function
;; for the main logic
(defmacro defun-g (name args &body body)
  "Define a function that runs on the gpu. "
  (let ((doc-string (when (stringp (first body)) (pop body)))
        (declarations (when (and (listp (car body)) (eq (caar body) 'declare))
                        (pop body))))
    (assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context)
                 (instancing :&instancing))
        (varjo::lambda-list-split '(:&uniform :&context :&instancing) args)
      (%def-gpu-function name in-args uniforms context body instancing
                         doc-string declarations))))

(defun undefine-gpu-function (name)
  (%unsubscibe-from-all name)
  (remhash name *gpu-func-specs*)
  (remhash name *dependent-gpu-functions*)
  nil)

(defun %test-compile (in-args uniforms context body)
  (varjo::translate in-args uniforms (union '(:vertex :fragment :330) context)
                    `(progn ,@body (v! 1 2 3 4))))

;;--------------------------------------------------

(defun %def-gpu-function (name in-args uniforms context body instancing
                          doc-string declarations)
  (let ((spec (%make-gpu-func-spec name in-args uniforms context body
                                   instancing doc-string declarations)))
    (assert (%gpu-func-compiles-in-some-context spec))
    (let ((depends-on (%find-gpu-functions-depended-on spec))
          (pipelines-depending-on (pipelines-that-use-this-func name))) ;; {TODO} why isnt this used?
      (assert (not (%find-recursion name depends-on)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (%update-gpu-function-data ,(%serialize-gpu-func-spec spec)
                                      ',depends-on))
         (%test-compile ',in-args ',uniforms ',context ',body)
         ,(%make-stand-in-lisp-func spec)
         (%recompile-gpu-function ',name)))))


(defun %recompile-gpu-function (name)
  ;; recompile gpu-funcs that depends on name
  (mapcar #'%recompile-gpu-function (funcs-that-use-this-func name))
  ;; and recompile pipelines that depend on name
  (mapcar λ(let ((recompile-pipeline-name (recompile-name %)))
             (when (symbol-function recompile-pipeline-name)
               (handler-case
                   (funcall (symbol-function recompile-pipeline-name))
                 (undefined-function () (format t "~%%recompile-gpu-functions: no function ~a exists" recompile-pipeline-name))
                 (error () nil))))
          (pipelines-that-use-this-func name)))


(defun %subscribe-to-gpu-func (name subscribe-to-name)
  (assert (not (eq name subscribe-to-name)))
  (symbol-macrolet ((func-specs (funcs-that-use-this-func subscribe-to-name)))
    (when (and (gpu-func-spec subscribe-to-name)
               (not (member name func-specs)))
      (format t "; func ~s subscribed to ~s" name subscribe-to-name)
      (push name func-specs))))

(defun %unsubscibe-from-all (name)
  (labels ((%remove-gpu-function-from-dependancy-table (func-name dependencies)
             (when (member name dependencies)
               (setf (funcs-that-use-this-func func-name)
                     (remove name dependencies)))))
    (maphash #'%remove-gpu-function-from-dependancy-table
             *dependent-gpu-functions*)))

(defun %update-gpu-function-data (spec depends-on)
  (with-slots (name) spec
    (%unsubscibe-from-all name)
    (mapcar (fn_ #'%subscribe-to-gpu-func name) depends-on)
    (setf (gpu-func-spec name) spec)
    ;(%recompile-gpu-function name)
    ))

(defun %gpu-func-compiles-in-some-context (spec)
  (declare (ignorable spec))
  t)

(defun %expand-all-macros (spec)
  (with-gpu-func-spec (spec)
    (let ((env (make-instance 'varjo::environment)))
      (%%expand-all-macros body context env))))

(defun %%expand-all-macros (body context env)
  (varjo::pipe-> (nil nil context body env)
    #'varjo::split-input-into-env
    #'varjo::process-context
    (equal #'varjo::symbol-macroexpand-pass
           #'varjo::macroexpand-pass
           #'varjo::compiler-macroexpand-pass)))

(defun %find-gpu-funcs-in-source (source &optional locally-defined)
  (unless (atom source)
    (remove-duplicates
     (alexandria:flatten
      (let ((s (first source)))
        (cond
          ;; first element isnt a symbol, keep searching
          ((listp s)
           (append (%find-gpu-funcs-in-source s locally-defined)
                   (mapcar (lambda (x) (%find-gpu-funcs-in-source x locally-defined))
                           (rest source))))
          ;; it's a let so ignore the var name
          ((eq s 'varjo::%glsl-let) (%find-gpu-funcs-in-source (cadadr source)
                                                        locally-defined))
          ;; it's a function so skip to the body and
          ((eq s 'varjo::%make-function)
           (%find-gpu-funcs-in-source (cddr source) locally-defined))
          ;; it's a clone-env-block so there could be function definitions in
          ;; here. check for them and add any names to the locally-defined list
          ((eq s 'varjo::%clone-env-block)

           (let* (;; labels puts %make-function straight after the clone-env
                  (count (length (remove 'varjo::%make-function
                                         (mapcar #'first (rest source))
                                         :test-not #'eq)))
                  (names (mapcar #'second (subseq source 1 (1+ count)))))
             (%find-gpu-funcs-in-source (subseq source (1+ count))
                                 (append names locally-defined))))
          ;; its a symbol, just check it isn't varjo's and if we shouldnt ignore
          ;; it then record it
          ((and (symbolp s)
                (not (equal (package-name (symbol-package s)) "VARJO"))
                (not (member s locally-defined)))
           (cons s (mapcar (lambda (x) (%find-gpu-funcs-in-source x locally-defined))
                           (rest source))))
          ;; nothing to see, keep searching
          (t (mapcar (lambda (x) (%find-gpu-funcs-in-source x locally-defined))
                     (rest source)))))))))

(defun %find-gpu-functions-depended-on (spec)
  (%find-gpu-funcs-in-source (%expand-all-macros spec)))

(defun %make-stand-in-lisp-func (spec)
  (with-gpu-func-spec (spec)
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

(defun extract-args-from-gpu-functions (function-names)
  (let ((specs (mapcar #'gpu-func-spec function-names)))
    (assert (every #'identity specs))
    (aggregate-args-from-specs specs)))

(defun aggregate-args-from-specs (specs &optional (args-accum t) uniforms-accum)
  (if specs
      (let ((spec (first specs)))
        (with-gpu-func-spec (spec)
          (aggregate-args-from-specs
           (rest specs)
           (if (eql t args-accum) in-args args-accum)
           (aggregate-uniforms uniforms uniforms-accum))))
      `(,@args-accum ,@(when uniforms-accum (cons '&uniform uniforms-accum)))))

(defun aggregate-uniforms (uniforms &optional accum)
  (if uniforms
      (let ((u (first uniforms)))
        (cond
          ;; if there is no other uniform with matching name, hense no collision
          ((not (find (first u) accum :test #'equal :key #'first))
           (aggregate-uniforms (rest uniforms) (cons u accum)))
          ;; or the uniform matches perfectly so no collision
          ((find u accum :test #'equal)
           (aggregate-uniforms (rest uniforms) accum))
          ;; or it's a clash
          (t (error "Uniforms for the functions are incompatible: ~a ~a"
                    u accum))))
      accum))

(defun %get-stage-uniforms (names &optional uniforms-accum)
  (if names
      (with-gpu-func-spec ((gpu-func-spec (first names)))
        (%get-stage-uniforms
         (rest names)
         (aggregate-uniforms uniforms uniforms-accum)))
      uniforms-accum))

(defmacro with-processed-func-specs (stages &body body)
  `(let ((args (extract-args-from-gpu-functions ,stages)))
     (utils:assoc-bind ((in-args nil) (unexpanded-uniforms :&uniform))
         (varjo::lambda-list-split '(&uniform) args)
       ,@body)))

;;--------------------------------------------------

(defun get-func-as-stage-code (name)
  (with-gpu-func-spec ((gpu-func-spec name))
    (list
     in-args
     uniforms
     context
     `(labels ,(mapcar (lambda (spec)
                         (with-gpu-func-spec (spec)
                           `(,name ,in-args ,@body)))
                       (mapcar #'gpu-func-spec (funcs-this-func-uses name)))
        ,@body))))

(defun varjo-compile-as-stage (name)
  (apply #'varjo:translate
         (get-func-as-stage-code name)))

(defun varjo-compile-as-pipeline (args)
  (destructuring-bind (stage-pairs context) (parse-gpipe-args args)
    (declare (ignore context))
    (%varjo-compile-as-pipeline stage-pairs )))
(defun %varjo-compile-as-pipeline (parsed-gpipe-args)
  (rolling-translate (mapcar #'prepare-stage parsed-gpipe-args)))

(defun prepare-stage (stage-pair)
  (let ((stage-type (car stage-pair))
        (stage-name (cdr stage-pair)))
    (destructuring-bind (in-args uniforms context code)
        (get-func-as-stage-code stage-name)
      ;; ensure context doesnt specify a stage or that it matches
      (let ((n (count-if λ(member % varjo::*stage-types*) context)))
        (assert (and (<= n 1) (if (= n 1) (member stage-type context) t))))
      (list in-args
            uniforms
            (cons stage-type (remove stage-type context))
            code))))

;;--------------------------------------------------

(defun parse-gpipe-args (args)
  (let ((cut-pos (or (position :context args) (length args))))
    (destructuring-bind (&key context) (subseq args cut-pos)
      (list
       (let ((args (subseq args 0 cut-pos)))
         (if (and (= (length args) 2) (not (some #'keywordp args)))
             `((:vertex . ,(cadar args)) (:fragment . ,(cadadr args)))
             (let* ((stages (copy-list varjo::*stage-types*))
                    (results (loop :for a :in args
                                :if (keywordp a) :do (setf stages (cons a (subseq stages (1+ (position a stages)))))
                                :else :collect (cons (or (pop stages) (error "Invalid gpipe arguments, no more stages"))
                                                     (cadr a)))))
               (remove :context results :key #'car))))
       context))))

(defun get-gpipe-arg (key args)
  (destructuring-bind (stage-pairs context) (parse-gpipe-args args)
    (declare (ignore context))
    (%get-gpipe-arg key stage-pairs)))
(defun %get-gpipe-arg (key parsed-args)
  (cdr (assoc key parsed-args)))

(defun validate-gpipe-args (args)
  (not (null (reduce #'%validate-gpipe-arg args))))

(defun %validate-gpipe-arg (previous current)
  (if (keywordp current)
      (progn
        (when (keywordp previous)
          (error "Invalid gpipe arguments: ~s follows ~s" current previous))
        (unless (member current varjo::*stage-types*)
          (error "Invalid gpipe arguments: ~s is not the name of a stage" current))
        current)
      (if (symbolp current)
          current
          (error "Invalid gpipe argument: ~a" current))))
