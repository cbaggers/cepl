(in-package :cgl)
(named-readtables:in-readtable fn_::fn_lambda)

;; defun-gpu is at the bottom of this file

(defclass gpu-func-spec ()
  ((name :initarg :name)
   (in-args :initarg :in-args)
   (uniforms :initarg :uniforms)
   (context :initarg :context)
   (body :initarg :body)
   (instancing :initarg :instancing)
   (doc-string :initarg :doc-string)
   (declarations :initarg :declarations)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gpu-func-spec ((func-spec) &body body)
    `(with-slots (name in-args uniforms context body instancing
                       doc-string declarations) ,func-spec
       (declare (ignorable name in-args uniforms context body instancing
                           doc-string declarations))
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gpu-func-specs* (make-hash-table :test #'eq))
  (defvar *dependent-gpu-functions* (make-hash-table :test #'eq))
  (defvar *dependent-external-functions* (make-hash-table :test #'eq)))

(defun funcs-that-use-this-func (name)
  (gethash name *dependent-gpu-functions*))

(defun (setf funcs-that-use-this-func) (value name)
  (setf (gethash name *dependent-gpu-functions*) value))

(defun funcs-to-call-on-change (name)
  (gethash name *dependent-external-functions*))

(defun add-func-to-call-on-change (name func)
  (when (not (member func (gethash name *dependent-external-functions*)
                     :test #'eq))
    (push func (gethash name *dependent-external-functions*))))

(defun map-hash (function hash-table)
  "map through a hash and actually return something"
  (let* ((head (list nil))
         (tail head))
    (labels ((do-it (k v)
               (rplacd tail (setq tail (list (funcall function k v))))))
      (maphash #'do-it hash-table))
    (cdr head)))

(defun funcs-this-func-uses (name)
  "Recursivly searches for functions by this function.
Sorts the list of function names by dependency so the earlier
names are depended on by the functions named later in the list"
  (mapcar #'car
          (remove-duplicates
           (sort (%funcs-this-func-uses name) #'> :key #'cdr)
           :key #'car :from-end t)))

(defun %funcs-this-func-uses (name &optional (depth 0))
  (let ((this-func-calls
         (remove nil (map-hash
                      (lambda (k v)
                        (when (member name v)
                          (cons k depth)))
                      *dependent-gpu-functions*))))
    (append this-func-calls
            (apply #'append
                   (mapcar (lambda (x)
                             (%funcs-this-func-uses (car x) (1+ depth)))
                           this-func-calls)))))

(defun gpu-func-spec (name) (gethash name *gpu-func-specs*))
(defun (setf gpu-func-spec) (value name)
  (setf (gethash name *gpu-func-specs*) value))

(defun %recompile-gpu-function (name)
  (mapcar #'%recompile-gpu-function (funcs-that-use-this-func name))
  (labels ((safe-call (func)
             (funcall func)
             ;; (handler-case (funcall func)
             ;;   (error () (format t "~%%recompile-gpu-functions: Unable to call ~a"
             ;;                     func)))
             ))
    (mapcar #'safe-call (funcs-to-call-on-change name))))

(defun %make-gpu-func-spec (name in-args uniforms context body instancing
                            doc-string declarations)
  (make-instance 'gpu-func-spec
                 :name name
                 :in-args (mapcar #'listify in-args)
                 :uniforms (mapcar #'listify uniforms)
                 :context context
                 :body body
                 :instancing instancing
                 :doc-string doc-string
                 :declarations declarations))

(defun %serialize-gpu-func-spec (spec)
  (with-gpu-func-spec (spec)
    `(%make-gpu-func-spec ',name ',in-args ',uniforms ',context ',body
                          ',instancing ,doc-string ',declarations)))

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

(defun %make-stand-in-lisp-func (spec depends-on)
  (with-gpu-func-spec (spec)
    (let ((arg-names (mapcar #'first in-args))
          (uniform-names (mapcar #'first uniforms)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (%update-gpu-function-data ,(%serialize-gpu-func-spec spec)
                                      ',depends-on))
         (%recompile-gpu-function ',name)
         (defun ,name (,@arg-names
                       ,@(when uniforms (cons (symb :&uniform) uniform-names) ))
           (declare (ignore ,@arg-names ,@uniform-names))
           (warn "GPU Functions cannot currently be used from the cpu"))))))

(defun %find-recursion (name depends-on)
  (declare (ignore name depends-on))
  nil)

(defun %def-gpu-function (name in-args uniforms context body instancing
                          doc-string declarations)
  (let ((spec (%make-gpu-func-spec name in-args uniforms context body
                                   instancing doc-string declarations)))
    (assert (%gpu-func-compiles-in-some-context spec))
    (let ((depends-on (%find-gpu-functions-depended-on spec)))
      (assert (not (%find-recursion name depends-on)))
      (%make-stand-in-lisp-func spec depends-on))))

;;--------------------------------------------------

(defmacro defun-g (name args &body body)
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
  (remhash name *dependent-external-functions*)
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
      `(,args-accum &uniform ,@uniforms-accum)))

(defun aggregate-uniforms (from into)
  (if from
      (let ((u (first from)))
        (cond ((not (find (first u) into :test #'equal :key #'first))
               (aggregate-uniforms (rest from) (cons u into)))
              ((find u into :test #'equal)
               (aggregate-uniforms (rest from) into))
              (t (error "Uniforms for the functions are incompatible: ~a ~a"
                        u into))))
      into))

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
             `((:vertex . ,(first args)) (:fragment . ,(second args)))
             (let* ((stages (append (copy-list varjo::*stage-types*) (list :post)))
                    (results (loop :for a :in args
                                :if (keywordp a) :do (setf stages (cons a (subseq stages (1+ (position a stages)))))
                                :else :collect (cons (or (pop stages) (error "Invalid gpipe arguments, no more stages"))
                                                     a))))
               (remove :post (remove :context results :key #'car) :key #'car))))
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
