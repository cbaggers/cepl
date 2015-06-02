(in-package :cgl)

(defun parse-compose-gpipe-args (args)
  `(,(mapcar (fn+ #'car #'last1) args)
     nil))

(defun %defpipeline-compose (name args options gpipe-args)
  (assert (and (every #'symbolp args) (not (some #'keywordp args))))
  (destructuring-bind (user-args &optional user-uniforms)
      (split-sequence :&uniform args :test #'string-equal)
    (assoc-bind ((fbos :fbos) (context :context) (post :post))
        (parse-options options)
      (destructuring-bind (pipeline-names gpipe-context)
          (parse-compose-gpipe-args gpipe-args)
        (assert (not (and gpipe-context context)))
        (let* ((uniform-args (make-pipeline-uniform-args
                              pipeline-names
                              gpipe-args))
               (uniforms
                (append (mapcar #'first uniform-args)
                        user-uniforms)))
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (update-pipeline-spec
                (make-compose-pipeline-spec
                 ',name ',pipeline-names ',user-args ',uniform-args
                 ',(or gpipe-context context))))
             (let (,@(when fbos (mapcar #'car fbos))
                   (initd nil))
               (def-compose-dispatch
                   ,name
                   ,(append user-args (make-pipeline-stream-args gpipe-args))
                 ,uniforms ,context
                 ,gpipe-args ,fbos ,post))
             (def-compose-dummy
                 ,name
                 ,(append user-args (make-pipeline-stream-args gpipe-args))
               ,uniforms)))))))

;;--------------------------------------------------

(defconstant +db-ptr-sym+ '%--dbuffer-master-ptr*)
(defconstant +db-pass-ptr-sym+ '%--dbuffer-master-ptr*)

(defmacro def-compose-dispatch (name args uniforms context
                                gpipe-args fbos post)
  (declare (ignore context))
  (let* ((pass-data (mapcar #'make-map-g-pass gpipe-args
                            (make-pipeline-stream-args gpipe-args nil t)))
         (pass-code (mapcar #'first pass-data))
         (all-draw-buffers (apply #'append (mapcar #'second pass-data))))
    `(let (,@(when all-draw-buffers
                   `((,+db-ptr-sym+
                       (foreign-alloc 'cl-opengl-bindings:enum :count
                                      ,(length all-draw-buffers)
                                      :initial-contents ',all-draw-buffers)))))
       (defun ,(dispatch-func-name name)
           (,@args ,@(when uniforms `(&key ,@uniforms)))
         (unless initd
           ,@(mapcar #'fbo-comp-form fbos)
           (setf initd t)
           ,(when post `(funcall ,post)))
         ;; symbol-macrolet will go here
         (labels ((cgl:attachment (fbo attachment-name)
                    (slot-value (cgl::attachment-gpu-array
                                 (cgl::%attachment fbo attachment-name))
                                'cgl::texture)))
           ,@pass-code)))))

(defun fbo-comp-form (form)
  (destructuring-bind (name . make-fbo-args) form
    `(setf ,name (make-fbo ,@make-fbo-args))))

(defun make-map-g-pass (pass-form stream-args)
  (destructuring-bind (fbo &rest call-forms) pass-form
    (let* ((lisp-forms (butlast call-forms))
           (call-form (last1 call-forms))
           (func-name (first call-form))
           (override-offset (or (position-if #'keywordp call-form)
                                1))
           (map-g-form `(map-g #',func-name ,@stream-args
                               ,@(subseq call-form override-offset)
                               ,@(mapcat #'%uniform-arg-to-call
                                       (get-pipeline-uniforms func-name
                                                              call-form)))))
      ;; the return sig is a list of '(the-code the-draw-buffers)
      (if fbo
          (%gen-with-bind-fbo-result fbo lisp-forms map-g-form)
          (if lisp-forms
              (list `(progn ,@lisp-forms ,map-g-form) nil)
              (list map-g-form nil))))))

(defun %uniform-arg-to-call (uniform-arg)
  `(,(kwd (first uniform-arg)) ,(first uniform-arg)))

(defun %gen-with-bind-fbo-result (fbo-pattern lisp-forms map-g-form)
  (let* ((fbo-pattern (listify fbo-pattern))
         (key-start (or (position-if #'keywordp fbo-pattern)
                        (length fbo-pattern)))
         (fbo (first fbo-pattern))
         (buffer-pattern (subseq fbo-pattern 1 key-start))
         (keys (subseq fbo-pattern key-start)))
    (when (and (member :draw-buffers keys) buffer-pattern)
      (error "defpipeline: You must not specify an fbo target with buffers and a :draw-buffers keyword argument"))
    (when (and (not buffer-pattern) (not (member :draw-buffers keys)))
      (setf keys (append keys '(:draw-buffers t))))
    (list
     `(with-bind-fbo
          (,fbo
           ,@keys
           ,@(when buffer-pattern
                   `(:draw-buffers '(,+db-ptr-sym+ ,(length buffer-pattern)))))
        ,@lisp-forms
        ,map-g-form)
     buffer-pattern)))

;;--------------------------------------------------

(defmacro def-compose-dummy (name args uniforms)
  `(defun ,name (,@args ,@(when uniforms `(&key ,@uniforms)))
     (declare (ignorable ,@uniforms ,@args))
     (error "Pipelines do not take a stream directly, the stream must be map-g'd over the pipeline")))

;;--------------------------------------------------

(defun collate-args (pipeline-names)
  (mapcar #'%collate-args (get-pipeline-specs pipeline-names)))
(defmethod %collate-args ((spec shader-pipeline-spec))
  (slot-value (gpu-func-spec (first (slot-value spec 'stages))) 'in-args))
(defmethod %collate-args ((spec compose-pipeline-spec))
  (slot-value spec 'in-args))

(defun get-pipeline-specs (pipeline-names)
  (mapcar #'pipeline-spec pipeline-names))

(defun get-overidden-uniforms (pass-forms)
  (let* ((forms (mapcar #'last1 pass-forms)))
    (mapcar (lambda (_) (remove-if-not #'keywordp _)) forms)))

(defun get-overidden-uniforms2 (pass-forms)
  (let* ((forms (mapcar #'last1 pass-forms)))
    (mapcar (lambda (_)
              (group
               (subseq _ (or (position-if #'keywordp _)
                             (length _)))
               2))
            forms)))

(defun uniquify-names (names)
  (let ((seen nil)
        (final nil))
    (loop for name in names do
         (if (assoc name seen)
             (let* ((count (+ 1 (cdr (assoc name seen))))
                    (new-name (symb-package (symbol-package name) name count)))
               (push (cons name count) seen)
               (push new-name final))
             (progn
               (push (cons name 0) seen)
               (push name final))))
    (reverse final)))

(defun make-pipeline-stream-args (gpipe-forms &optional (flatten t)
                                                (include-overriden nil))
  (destructuring-bind (pipeline-names pipeline-stream-overrides)
      (loop :for fbo-form :in gpipe-forms
         :for call-form = (last1 fbo-form)
         :collect (first call-form) :into pipeline-names
         :collect (let ((args (rest call-form)))
                    (subseq args 0 (or (position-if #'keywordp args)
                                       (length args)))) :into streams
         :finally (return (list pipeline-names streams)))
    ;; '(standard-pass refract-pass) '(nil nil)
    (let* ((count -1)
           (result (mapcar (lambda (x y)
                             (append
                              (when include-overriden y)
                              (subseq (mapcar (lambda (_)
                                                (symb (car _) (incf count)))
                                              x)
                                      (length y))))
                           (collate-args pipeline-names)
                           pipeline-stream-overrides)))
      (if flatten (apply #'append result) result))))

;;{TODO} handle equivalent types
(defun make-pipeline-uniform-args (pipeline-names gpipe-args)
  (let* ((overriden-uniforms (get-overidden-uniforms gpipe-args))
         (all-uniforms
           (mapcat (lambda (uniforms overriden)
                     (loop :for uniform :in uniforms
                        :if (not (member (first uniform) overriden
                                         :test #'string-equal))
                        :collect uniform))
                   (mapcar #'get-pipeline-uniforms pipeline-names)
                   overriden-uniforms)))
    (%aggregate-uniforms all-uniforms)))

(defun get-pipeline-uniforms (pipeline-name &optional call-form)
  (%get-pipeline-uniforms (pipeline-spec pipeline-name) call-form))

(defmethod %get-pipeline-uniforms
    ((pipeline-spec shader-pipeline-spec) call-form)
  (let ((result (aggregate-uniforms (slot-value pipeline-spec 'stages)))
        (overriden-uniforms (remove-if-not #'keywordp call-form)))
    (remove-if (lambda (_) (member _ overriden-uniforms
                                   :test (lambda (x y) (string-equal (car x) y))))
               result)))

(defmethod %get-pipeline-uniforms
    ((pipeline-spec compose-pipeline-spec) call-form)
  (let ((result (slot-value pipeline-spec 'uniforms))
        (overriden-uniforms (remove-if-not #'keywordp call-form)))
    (remove-if (lambda (_) (member _ overriden-uniforms
                                   :test (lambda (x y) (string-equal (car x) y))))
               result)))
