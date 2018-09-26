(in-package :cepl.pipelines)

(defvar *init-pipeline-lock* (bt:make-lock))

(defun+ function-arg-p (arg)
  (typep (varjo:type-spec->type (second arg))
         'varjo:v-function-type))

(defun+ stages-require-partial-pipeline (func-specs)
  (some (lambda (x)
          (with-gpu-func-spec x
            (or (some #'function-arg-p in-args)
                (some #'function-arg-p uniforms))))
        func-specs))

(defun+ has-func-type-in-args (func-specs)
  (some (lambda (x)
          (with-gpu-func-spec x
            (some #'function-arg-p in-args)))
        func-specs))

(defun+ function-uniforms (func-specs)
  (mapcat (lambda (x)
            (with-gpu-func-spec x
              (remove-if-not #'function-arg-p uniforms)))
          func-specs))

(defun lambda-arg-p (x)
  (and (listp x) (eq (first x) 'lambda-g)))

(defmacro defpipeline-g (name context &body gpipe-args)
  (assert-valid-gpipe-form name gpipe-args)
  (if (some #'lambda-arg-p gpipe-args)
      (expand-lambda-defpipeline name context gpipe-args)
      (%defpipeline-gfuncs name gpipe-args context)))

(defun+ expand-lambda-defpipeline (name context gpipe-args)
  (let* (funcs
         new-args)
    (loop :for arg :in gpipe-args
       :for i :from 0 :do
       (if (lambda-arg-p arg)
           (dbind (args &rest body) (rest arg)
             (flet ((sym= (x y)
                      (when (and (symbolp x) (symbolp y))
                        (string= x y))))
               (let* ((l-name (hidden-symb name (format nil "LAMBDA-~a" i)))
                      (uniform-pos (or (position '&uniform args :test #'sym=)
                                       (length args)))
                      (in-args (subseq args 0 uniform-pos))
                      (in-types (mapcar #'second in-args))
                      (l-spec (cons l-name in-types)))
                 (push `(defun-g ,l-name ,args ,@body) funcs)
                 (push l-spec new-args))))
           (push arg new-args)))
    `(progn
       ,@(reverse funcs)
       (defpipeline-g ,name ,context ,@(reverse new-args)))))

(defun+ %defpipeline-gfuncs (name gpipe-args context)
  ;;
  ;; The heart of defpipeline-g. It is seperate from defpipeline-g so that
  ;; it can be used from the recompile func with having to deal with the
  ;; outer macro
  (destructuring-bind (stage-pairs post) (parse-gpipe-args gpipe-args)
    ;;
    (let* ((func-specs (mapcar #'cdr stage-pairs))
           (aggregate-actual-uniforms (aggregate-uniforms name
                                                          :pipeline
                                                          func-specs
                                                          t))
           (aggregate-public-uniforms (aggregate-uniforms name
                                                          :pipeline
                                                          func-specs)))
      (if (stages-require-partial-pipeline func-specs)
          (%def-partial-pipeline name
                                 func-specs
                                 stage-pairs
                                 aggregate-actual-uniforms
                                 context)
          (%def-complete-pipeline name
                                  gpipe-args
                                  stage-pairs
                                  aggregate-actual-uniforms
                                  aggregate-public-uniforms
                                  post context)))))

(defun+ %def-partial-pipeline (name
                               func-specs
                               stage-pairs
                               aggregate-actual-uniforms
                               raw-context)
  ;;
  ;; freak out if try and use funcs in stage args
  (when (has-func-type-in-args func-specs)
    (error 'functions-in-non-uniform-args :name name))
  ;;
  ;; update the spec immediately (macro-expansion time)
  (update-pipeline-spec
   (make-pipeline-spec name stage-pairs raw-context))
  ;;
  (let ((uniform-names (mapcar #'first aggregate-actual-uniforms)))
    `(progn
       ;;
       ;; we upload the spec at compile time (using eval-when)
       ,(gen-update-spec name stage-pairs raw-context)
       ;;
       ;; generate the dummy dispatch func
       (defun ,name (mapg-context draw-array stream
                     ,@(when uniform-names `(&key ,@uniform-names)))
         (declare (ignore mapg-context draw-array) (ignorable ,@uniform-names))
         (error 'mapping-over-partial-pipeline
                :name ',name
                :args ',(function-uniforms func-specs))
         stream)
       (register-named-pipeline ',name #',name)
       ',name)))

(defstruct pipeline-state
  (diff-tag 0 :type (unsigned-byte 16))
  (prog-ids
   (make-array +max-context-count+
               :initial-element +unknown-gl-id+
               :element-type 'gl-id)
   :type (simple-array gl-id (#.cepl.context:+max-context-count+)))
  ;;
  ;; If there are no implicit-uniforms we need a no-op
  ;; function to call
  (implicit-uniform-upload-func
   #'fallback-iuniform-func :type function)
  ;;
  ;; The primitive used by transform feedback. When nil
  ;; the primitive comes from the render-mode
  (tfs-primitive nil :type symbol)
  (tfs-array-count 0 :type (unsigned-byte 8))
  ;;
  ;; When nil we enable fragment-discard
  (has-fragment-stage t :type boolean)
  ;;
  ;; Uniform IDs
  (uniform-int-ids
   (make-array 100 :element-type '(signed-byte 32)
               :initial-element +unknown-uniform-int-id+)
   :type (array (signed-byte 32) (*)))
  (uniform-uint-ids
   (make-array 100 :element-type '(unsigned-byte 32)
               :initial-element +unknown-uniform-uint-id+)
   :type (array (unsigned-byte 32) (*))))

#+sbcl
(declaim (sb-ext:freeze-type pipeline-state))

(defun+ %def-complete-pipeline (name
                                original-gpipe-args
                                stage-pairs
                                aggregate-actual-uniforms
                                aggregate-public-uniforms
                                post
                                raw-context)
  (let* ((ctx *pipeline-body-context-var*)
         (state-var (hidden-symb name :pipeline-state))
         (init-func-name (hidden-symb name :init))
         (uniform-assigners (make-arg-assigners aggregate-actual-uniforms))
         (current-spec (pipeline-spec name))
         (current-stage-tags (when current-spec
                               (remove nil (slot-value current-spec 'diff-tags))))
         (new-stage-tags (mapcar (lambda (x)
                                   (let ((spec (cdr x)))
                                     (when spec
                                       (slot-value spec 'diff-tag))))
                                 stage-pairs))
         (structurally-unchanged-p (and (every #'identity current-stage-tags)
                                        (every #'identity new-stage-tags)
                                        (equal current-stage-tags new-stage-tags)))
         (state-tag (mod (reduce #'+ new-stage-tags)
                         (expt 2 16)))
         ;; we generate the func that compiles & uploads the pipeline
         ;; and also populates the pipeline's local-vars
         (compile-context (parse-compile-context name raw-context :pipeline))
         (primitive (compile-context-primitive compile-context))
         (is-compute (find :compute stage-pairs :key #'car)))
    ;;
    ;; update the spec immediately (macro-expansion time)
    (update-pipeline-spec
     (make-pipeline-spec name stage-pairs raw-context))
    (values
     `(progn
        ;;
        ;; eval-when so we dont get a 'use before compiled' warning
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (define-compiler-macro ,name (&whole whole ,ctx &rest rest)
            (declare (ignore rest))
            (unless (mapg-context-p ,ctx)
              (error 'dispatch-called-outside-of-map-g :name ',name))
            whole))
        ;;
        ;; The struct that holds the gl state for this pipeline
        (declaim (type pipeline-state ,state-var))
        (defvar ,state-var (make-pipeline-state))
        ;;
        ;; If the prog-id isnt know this function will be called
        (defun+ ,init-func-name (state)
          ,(gen-pipeline-init name
                              primitive
                              stage-pairs
                              post
                              aggregate-public-uniforms
                              uniform-assigners
                              state-tag))
        ;;
        ;; generate the code that actually renders
        ,(def-dispatch-func ctx compile-context name init-func-name
                            uniform-assigners aggregate-public-uniforms
                            state-var state-tag is-compute)
        ;;
        ;; generate the function that recompiles this pipeline
        ,(unless (compile-context-static-p compile-context)
                 (gen-recompile-func name original-gpipe-args raw-context))
        ;;
        ;; off to the races! Note that we upload the spec at compile
        ;; time (using eval-when)
        ,(gen-update-spec name stage-pairs raw-context)
        (register-named-pipeline ',name #',name)
        ',name)
     structurally-unchanged-p)))

(defun+ def-dispatch-func (ctx
                           compile-context
                           name
                           init-func-name
                           uniform-assigners
                           aggregate-public-uniforms
                           state-var
                           state-tag
                           compute)
  (let* ((static (compile-context-static-p compile-context))
         (primitive (compile-context-primitive compile-context))
         (uniform-names (mapcar #'first aggregate-public-uniforms))
         (typed-uniforms (loop :for name :in uniform-names :collect
                            `(,name t nil)))
         (stream-symb (if compute 'space 'stream))
         (stream-type (if compute 'compute-space 'buffer-stream))
         (return-type (if compute 'null 'fbo))
         (signature (if static
                        `(((,ctx cepl-context)
                           (,stream-symb (or null ,stream-type))
                           (draw-array (or null c-array gpu-array-bb))
                           ,@(when uniform-names `(&key ,@typed-uniforms)))
                          ,return-type)
                        `((,ctx
                           ,stream-symb
                           draw-array
                           ,@(when uniform-names `(&key ,@uniform-names))))))
         (def (if static 'defn 'defun+)))
    ;;
    ;; draw-function
    `(,def ,name ,@signature
       (declare (speed 3) (safety 1) (debug 1) (compilation-speed 0)
                (ignorable ,ctx draw-array ,@uniform-names)
                (inline pipeline-state-prog-ids
                        pipeline-state-implicit-uniform-upload-func
                        pipeline-state-tfs-primitive
                        pipeline-state-tfs-array-count
                        pipeline-state-has-fragment-stage)
                ,@(when static `((profile t))))
       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,@(unless (or compute (typep primitive 'varjo::dynamic))
           `((when ,stream-symb
               (assert
                (= ,(draw-mode-group-id primitive)
                   (buffer-stream-primitive-group-id ,stream-symb))
                ()
                'buffer-stream-has-invalid-primitive-for-stream
                :name ',name
                :pline-prim ',(varjo::lisp-name primitive)
                :stream-prim (buffer-stream-primitive ,stream-symb)))))
       (let* ((%ctx-id (context-id ,ctx))
              ;;
              ;; unpack state from var
              (state ,state-var)
              (prog-id (aref (pipeline-state-prog-ids state)
                             %ctx-id)))
         ;;
         ;; ensure program-id available, otherwise bail
         (when (or (= prog-id +unknown-gl-id+)
                   (/= (pipeline-state-diff-tag state)
                       ,state-tag))
           (let ((new-prog-ids (,init-func-name state)))
             (setf prog-id (aref new-prog-ids %ctx-id))
             (when (= prog-id +unknown-gl-id+)
               (return-from ,name))))

         (let ((implicit-uniform-upload-func
                (pipeline-state-implicit-uniform-upload-func state))
               ,@(unless compute
                   '((tfs-primitive
                      (pipeline-state-tfs-primitive state))
                     (tfs-array-count
                      (pipeline-state-tfs-array-count state))
                     (has-fragment-stage
                      (pipeline-state-has-fragment-stage state))))
               ;; Uniforms vars
               ,@(mapcan #'unpack-arrayd-assigner uniform-assigners))

           ;;
           (use-program ,ctx prog-id)

           ;; Upload uniforms
           (profile-block (,name :uniforms)
             ,@(mapcar #'gen-uploaders-block uniform-assigners)
             (funcall implicit-uniform-upload-func prog-id ,@uniform-names))

           ;; Start the draw
           (when ,stream-symb
             ,(if compute
                  (compute-expander name stream-symb)
                  (draw-expander name ctx stream-symb 'draw-mode primitive))))

         ;; uniform cleanup
         ,@(mapcar #'gen-cleanup-block (reverse uniform-assigners))

         ;; map-g is responsible for returning the correct fbo
         ,(if compute
              nil
              `(draw-fbo-bound ,ctx))))))

(defn fallback-iuniform-func ((prog-id gl-id) &rest uniforms) (values)
  (declare (ignore prog-id uniforms)
           (optimize (speed 3) (safety 0) (debug 0)))
  (values))

(defun+ gen-recompile-func (name original-gpipe-args raw-context)
  `(defun+ ,(recompile-name name) ()
     (format t "~&; recompile cpu side of (~a ...)~&" ',name)
     (force-output)
     (let ((*standard-output* (make-string-output-stream)))
       (handler-bind ((warning #'muffle-warning))
         (multiple-value-bind (src structurally-unchanged-p)
             (%defpipeline-gfuncs ',name ',original-gpipe-args ',raw-context)
           (unless structurally-unchanged-p
             (eval src)))))))

(defun+ gen-update-spec (name stage-pairs raw-context)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-pipeline-spec
      (make-pipeline-spec
       ',name ,(serialize-stage-pairs stage-pairs)
       ',raw-context))))

(defun+ register-named-pipeline (name func)
  (declare (profile t))
  (setf (function-keyed-pipeline func)
        name))

(defun+ %gl-make-shader-from-varjo (compiled-stage)
  (make-shader (varjo->gl-stage-names compiled-stage)
               (varjo:glsl-code compiled-stage)))

(defun+ pairs-key-to-stage (stage-pairs)
  (mapcar (lambda (pair)
            (dbind (name . key) pair
              (etypecase key
                (func-key (cons name (gpu-func-spec key)))
                (gpu-func-spec pair))))
          stage-pairs))

(defun+ swap-versions (stage-pairs glsl-version)
  (loop :for (stage-name . spec) :in stage-pairs :collect
     (let ((new-context
            (with-gpu-func-spec spec
              (copy-compile-context context :versions glsl-version))))
       (cons stage-name (clone-stage-spec spec :new-context new-context)))))

(defun+ %compile-link-and-upload (name primitive stage-pairs)
  (let* ((glsl-version (compute-glsl-version-from-stage-pairs stage-pairs))
         (stage-pairs (swap-versions stage-pairs glsl-version))
         (compiled-stages (%varjo-compile-as-pipeline name primitive stage-pairs))
         (stages-objects (mapcar #'%gl-make-shader-from-varjo compiled-stages)))
    (unless *suppress-upload-message*
      (format t "~&; uploading (~a ...)~&" (or name "GPU-LAMBDA")))
    ;; when compiling lambda pipelines prog-ids will be a number, not a vector
    (multiple-value-bind (prog-ids prog-id)
        (request-program-id-for (cepl-context) name)
      (multiple-value-bind (tfb-mode tfb-names tfb-group-count)
          (calc-feedback-style-and-names (get-feedback-out-vars compiled-stages))
        (link-shaders stages-objects prog-id compiled-stages tfb-mode tfb-names)
        (when (and name *cache-last-compile-result*)
          (add-compile-results-to-pipeline name compiled-stages))
        (mapcar #'%gl:delete-shader stages-objects)
        (values compiled-stages prog-id prog-ids tfb-group-count)))))

(defun+ compute-glsl-version-from-stage-pairs (stage-pairs)
  ;; - If not specified & the context is not yet available then use
  ;;   the highest version available.
  ;;
  ;; - If not specified & the context is available then use the context
  ;;   info to pick the highest glsl version supported
  ;;
  ;; - If one stage specifes a version and the others dont then use that
  ;;   stage's version
  (labels ((get-context (pair)
             (with-gpu-func-spec (cdr pair)
               context))
           (get-glsl-version (&rest contexts)
             (let ((versions (remove-duplicates
                              (loop
                                 :for context :in contexts
                                 :append (compile-context-versions context)))))
               (case= (length versions)
                 (0 (cepl.context::get-best-glsl-version (cepl-context)))
                 (1 (first versions))
                 (otherwise nil)))))
    (let ((contexts (mapcar #'get-context stage-pairs)))
      (or (apply #'get-glsl-version contexts)
          (error 'glsl-version-conflict
                 :pairs (loop
                           :for (name nil) :in stage-pairs
                           :for context :in contexts
                           :for versions := (compile-context-versions context)
                           :when versions
                           :collect (cons name versions)))))))


(defun+ %create-implicit-uniform-uploader (compiled-stages uniform-names)
  (let* ((varjo-implicit (remove-if #'varjo:ephemeral-p
                                    (mapcat #'varjo:implicit-uniforms
                                            compiled-stages)))
         (uniform-arg-forms (mapcar #'varjo.internals:to-arg-form varjo-implicit)))
    ;;
    (when uniform-arg-forms
      (let* ((assigners (make-arg-assigners uniform-arg-forms))
             (u-lets (mapcat #'let-forms assigners))
             (uniform-transforms
              (remove-duplicates (mapcar (lambda (x)
                                           (list (varjo:name x)
                                                 (varjo.internals:cpu-side-transform x)))
                                         varjo-implicit)
                                 :test #'equal)))
        (%compile-closure
         `(let ((initd nil)
                ,@(mapcar (lambda (x) `(,(assigner-name x) -1))
                          u-lets))
            (lambda (prog-id ,@uniform-names)
              (declare (optimize (speed 3) (safety 1) (debug 1))
                       (ignorable ,@uniform-names)
                       ,@(mapcar (lambda (x)
                                   `(type ,(assigner-type x)
                                          ,(assigner-name x)))
                                 u-lets))
              (unless initd
                ,@(mapcar (lambda (x)
                            `(setf ,(assigner-name x)
                                   ,(assigner-body x)))
                          u-lets)
                (setf initd t))
              (let ,uniform-transforms
                ,@(mapcar #'gen-uploaders-block assigners))
              (values))))))))

(defun+ %implicit-uniforms-dont-have-type-mismatches (uniforms)
  (loop :for (name type . i) :in uniforms
     :do (just-ignore i)
     :always
     (loop :for (n tp . i_)
        :in (remove-if-not (lambda (x) (equal (car x) name)) uniforms)
        :do (just-ignore n i_)
        :always (equal type tp))))

(defun+ %compile-closure (code)
  (funcall (compile nil `(lambda () ,code))))

(defn-inline %post-init ((func (or null function))) (values)
  (declare (profile t))
  (setf (vao-bound (cepl-context)) 0)
  (force-use-program (cepl-context) 0)
  (when func (funcall func))
  (values))

(defun+ gen-pipeline-init (name
                           primitive
                           stage-pairs
                           post
                           aggregate-public-uniforms
                           uniform-assigners
                           state-tag)
  (let ((uniform-names (mapcar #'first aggregate-public-uniforms)))
    (multiple-value-bind (upload-forms int-arr-size uint-arr-size)
        (generate-uniform-upload-forms uniform-assigners)
      `(prog1
           ;; all image units will be >0 as 0 is used as scratch tex-unit
           (let ((image-unit 0))
             (declare (ignorable image-unit))
             (%with-cepl-context-slots (current-tfs) (cepl-context)
               ;; we check %tfs-current-prog-id rather than %tfs-bound as this
               ;; let's us know that the transform feedback has actually be
               ;; started in GL, not just that we are the scope of CEPL's
               ;; with-transform-feedback macro.
               (when (and current-tfs
                          (/= (%tfs-current-prog-id current-tfs)
                              +unknown-gl-id+))
                 (error 'pipeline-recompile-in-tfb-scope :name ',name)))
             (bt:with-lock-held (*init-pipeline-lock*)
               (multiple-value-bind (compiled-stages
                                     new-prog-id
                                     new-prog-ids
                                     tfb-group-count)
                   (%compile-link-and-upload
                    ',name ',primitive ,(serialize-stage-pairs stage-pairs))
                 (declare (ignorable compiled-stages))


                 (setf (pipeline-state-prog-ids state)
                       new-prog-ids)
                 (setf (slot-value (pipeline-spec ',name) 'prog-ids)
                       new-prog-ids)

                 (use-program (cepl-context) new-prog-id)


                 (setf (pipeline-state-implicit-uniform-upload-func state)
                       (or (%create-implicit-uniform-uploader compiled-stages
                                                              ',uniform-names)
                           #'fallback-iuniform-func))

                 (let ((prog-id new-prog-id))
                   (declare (ignorable prog-id))
                   (setf (pipeline-state-uniform-int-ids state)
                         (make-array ,int-arr-size
                                     :element-type '(signed-byte 32)))

                   (setf (pipeline-state-uniform-uint-ids state)
                         (make-array ,uint-arr-size
                                     :element-type '(unsigned-byte 32)))
                   ,@upload-forms)

                 (when (> tfb-group-count 0)
                   (setf (pipeline-state-tfs-primitive state)
                         (get-transform-feedback-primitive compiled-stages))
                   (setf (pipeline-state-tfs-array-count state)
                         tfb-group-count))

                 (let ((frag (find-if (lambda (x)
                                        (typep x 'compiled-fragment-stage))
                                      compiled-stages)))
                   (setf (pipeline-state-has-fragment-stage state)
                         (not (null frag))))
                 (setf (pipeline-state-diff-tag state)
                       ,state-tag)
                 new-prog-ids)))
         (%post-init ,post)))))

(defun generate-uniform-upload-forms (uniform-assigners)
  (let ((u-lets (mapcat #'let-forms uniform-assigners))
        (int-count 0)
        (uint-count 0))

    (values
     (loop :for u-let :in u-lets :collect
        (cond
          ;;
          ((equal (assigner-type u-let) '(unsigned-byte 32))
           (incf uint-count)
           `(setf (aref (pipeline-state-uniform-uint-ids state)
                        ,(assigner-index u-let))
                  ,(assigner-body u-let)))
          ;;
          ((equal (assigner-type u-let) '(signed-byte 32))
           (incf int-count)
           `(setf (aref (pipeline-state-uniform-int-ids state)
                        ,(assigner-index u-let))
                  ,(assigner-body u-let)))
          ;;
          (t (error "CEPL: Invalid type ~a in uniform assigners"
                    (assigner-type u-let)))))
     int-count
     uint-count)))

(defun+ get-transform-feedback-primitive (stages)
  (let* ((prims (loop :for stage :in stages :collect
                   (typecase stage
                     (compiled-tessellation-evaluation-stage
                      (ecase (primitive-out stage)
                        (:isolines :lines)
                        (:triangles :triangles)))
                     (compiled-geometry-stage
                      (ecase (primitive-out stage)
                        (:points :points)
                        (:line-strip :lines)
                        (:triangle-strip :triangles)))))))
    ;; We want the primitive of the last applicable stage
    (first (last prims))))

(defun+ serialize-stage-pairs (stage-pairs)
  `(pairs-key-to-stage
    (list ,@(loop :for (k . v) :in stage-pairs :collect
               `(cons ,k ,(spec->func-key v))))))

(defn-inline handle-transform-feedback
    (ctx draw-mode prog-id tfs-primitive tfs-array-count)
    (values)
  (%with-cepl-context-slots (current-tfs) ctx
    (let ((tfs current-tfs))
      (when tfs
        (let ((tfs-alen (length (%tfs-arrays tfs))))
          (assert (= tfs-alen tfs-array-count)
                  () 'incorrect-number-of-arrays-in-tfs
                  :tfs tfs
                  :tfs-count tfs-alen
                  :count tfs-array-count))
        (let ((tfs-prog-id (%tfs-current-prog-id tfs)))
          (if (= tfs-prog-id +unknown-gl-id+)
              (progn
                (%gl:begin-transform-feedback
                 (or tfs-primitive draw-mode))
                (setf (%tfs-current-prog-id tfs) prog-id))
              (assert (= tfs-prog-id prog-id) ()
                      'mixed-pipelines-in-with-tb))))))
  (values))

(defun+ escape-tildes (str)
  (cl-ppcre:regex-replace-all "~" str "~~"))

(defun draw-expander (profile-name ctx-symb stream-symb draw-mode-symb
                      primitive)
  "This draws the single stream provided using the currently
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  `(let ((draw-mode ,(if (typep primitive 'varjo::dynamic)
                         `(buffer-stream-draw-mode-val ,stream-symb)
                         (varjo::lisp-name primitive))))
     (handle-transform-feedback ,ctx-symb draw-mode prog-id tfs-primitive
                                tfs-array-count)

     (when (not has-fragment-stage)
       (gl:enable :rasterizer-discard))
     (,@(if profile-name
            `(profile-block (,profile-name :draw))
            '(progn))
        (let* ((stream ,stream-symb)
               (draw-mode ,draw-mode-symb)
               (index-type (buffer-stream-index-type stream))
               (instance-count
                (cepl.context::%cepl-context-instance-count ,ctx-symb)))
          ,@(when (typep primitive 'varjo::patches)
              `((assert (= (buffer-stream-patch-length stream)
                           ,(varjo::vertex-count primitive)))
                (%gl:patch-parameter-i
                 :patch-vertices ,(varjo::vertex-count primitive))))
          (with-vao-bound (buffer-stream-vao stream)
            (if draw-array
                (etypecase draw-array
                  (c-array
                   ;;(assert is-1d-array yada yada )
                   (if index-type
                       (%gl:multi-draw-elements-indirect
                        draw-mode
                        (cffi-type->gl-type index-type)
                        (c-array-pointer draw-array)
                        (c-array-total-size draw-array)
                        #.(cffi:foreign-type-size 'elements-indirect-command))
                       (%gl:multi-draw-arrays-indirect
                        draw-mode
                        (c-array-pointer draw-array)
                        (c-array-total-size draw-array)
                        #.(cffi:foreign-type-size 'arrays-indirect-command))))
                  (gpu-array-bb
                   ;;(assert is-1d-array yada yada )
                   (setf (gpu-buffer-bound ,ctx-symb :draw-indirect-buffer)
                         (gpu-array-bb-buffer draw-array))
                   (if index-type
                       (%gl:multi-draw-elements-indirect
                        draw-mode
                        (cffi-type->gl-type index-type)
                        (gpu-array-bb-offset-in-bytes-into-buffer draw-array)
                        (first (gpu-array-dimensions draw-array))
                        #.(cffi:foreign-type-size 'elements-indirect-command))
                       (%gl:multi-draw-arrays-indirect
                        draw-mode
                        (gpu-array-bb-offset-in-bytes-into-buffer draw-array)
                        (first (gpu-array-dimensions draw-array))
                        #.(cffi:foreign-type-size 'arrays-indirect-command)))))
                (if index-type
                    (locally (declare (optimize (speed 3) (safety 0))
                                      #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                      (%gl:draw-elements-instanced
                       draw-mode
                       (buffer-stream-length stream)
                       (cffi-type->gl-type index-type)
                       (%cepl.types:buffer-stream-start-byte stream)
                       instance-count))
                    (locally (declare (optimize (speed 3) (safety 0))
                                      #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                      (%gl:draw-arrays-instanced
                       draw-mode
                       (buffer-stream-start stream)
                       (buffer-stream-length stream)
                       instance-count)))))))
     (when (not has-fragment-stage)
       (gl:disable :rasterizer-discard))))


(defun compute-expander (profile-name space-symb)
  "This runs the compute function over the provided space using the
   currently bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  `(progn
     (assert (not draw-array))
     (,@(if profile-name
            `(profile-block (,profile-name :draw))
            '(progn))
        (locally (declare (optimize (speed 3) (safety 0))
                          #+sbcl
                          (sb-ext:muffle-conditions sb-ext:compiler-note))
          (%gl:dispatch-compute (compute-space-size-x ,space-symb)
                                (compute-space-size-y ,space-symb)
                                (compute-space-size-z ,space-symb))))))

;;;--------------------------------------------------------------
;;; GL HELPERS ;;;
;;;------------;;;

;; [TODO] Expand on this and allow loading on strings/text files for making
;;        shaders
(defun+ shader-type-from-path (path)
  "This uses the extension to return the type of the shader.
   Currently it only recognises .vert or .frag files"
  (let* ((plen (length path))
         (exten (subseq path (- plen 5) plen)))
    (cond ((equal exten ".vert") :vertex-shader)
          ((equal exten ".frag") :fragment-shader)
          (t (error "Could not extract shader type from shader file extension (must be .vert or .frag)")))))

(defun+ make-shader
    (shader-type source-string &optional (shader-id (%gl:create-shader
                                                     shader-type)))
  "This makes a new opengl shader object by compiling the text
   in the specified file and, unless specified, establishing the
   shader type from the file extension"
  (gl:shader-source shader-id source-string)
  (%gl:compile-shader shader-id)
  ;;check for compile errors
  (when (not (gl:get-shader shader-id :compile-status))
    (error "Error compiling ~(~a~): ~%~a~%~%~a"
           shader-type
           (gl:get-shader-info-log shader-id)
           source-string))
  shader-id)

(defun+ load-shader (file-path
                     &optional (shader-type
                                (shader-type-from-path file-path)))
  (restart-case
      (make-shader (cepl-utils:file-to-string file-path) shader-type)
    (reload-recompile-shader () (load-shader file-path
                                             shader-type))))

(defun+ load-shaders (&rest shader-paths)
  (mapcar #'load-shader shader-paths))

(defun get-feedback-out-vars (stages)
  (flet ((get-em (stage)
           (loop :for out-var :in (varjo:output-variables stage)
              :for qualifiers := (varjo:qualifiers (varjo:v-type-of out-var))
              :for feedback := (find-if (lambda (x)
                                          (typep x 'varjo:feedback-qualifier))
                                        qualifiers)
              :when feedback
              :collect (list out-var (feedback-group feedback)))))
    ;; ↓↓- because only 1 stage can contain feedback vars
    (first (remove nil (mapcar #'get-em stages)))))

;;  void glTransformFeedbackVaryings(GLuint program​, GLsizei count​,
;;  const char **varyings​, GLenum bufferMode​);
;;
;; program
;;     The name of the target program object.
;; count
;;     The number of varying variables used for transform feedback.
;; varyings
;;     An array of count​ null-terminated strings specifying the names
;;     of the varying variables to use for transform feedback.
;; bufferMode
;;     Identifies the mode used to capture the varying variables when
;;     transform feedback is active. bufferMode​ must be
;;     GL_INTERLEAVED_ATTRIBS or GL_SEPARATE_ATTRIBS.

(defun get-varyings (prog-id index)
  (with-foreign-objects ((cstr :char 100)
                         (length '%gl:sizei 1)
                         (size '%gl:sizei 1)
                         (type '%gl:enum 1))
    (%gl:get-transform-feedback-varying
     prog-id index 100 length size type cstr)
    (let ((str (cffi:foreign-string-to-lisp
                cstr :count (cffi:mem-aref length '%gl:sizei)))
          (type-kwd (cffi:mem-aref type '%gl:enum)))
      (list str type-kwd))))

(defun calc-feedback-style-and-names (varying-pairs)
  "returns the mode, the var names & the number of streams"
  (labels ((get-name (pair)
             (let ((v (first pair)))
               (format nil "~@[~a.~]~a"
                       (varjo:block-name-string v)
                       (varjo:glsl-name v)))))
    (let ((groups (sort (remove-duplicates (mapcar #'second varying-pairs))
                        #'<)))
      (case= (length groups)
        (0 (values nil nil 0))
        (1 (values :interleaved-attribs
                   (mapcar #'get-name varying-pairs)
                   1))
        (otherwise
         (let ((pairs (sort (copy-list varying-pairs) #'< :key #'second)))
           (assert (and (consecutive-integers-p groups) (find 0 groups)) ()
                   'non-consecutive-feedback-groups
                   :groups groups)
           (values :separate-attribs
                   (mapcar #'get-name pairs)
                   (length groups))))))))


(defun enable-transform-feedback (prog-id mode names)
  "Returns the number of gpu-arrays that should be bound in the
   transform-feedback-stream"
  (let ((len (length names)))
    (when (> len 0)
      (let ((ptrs (map 'vector #'cffi:foreign-string-alloc names))
            (arr-type `(:array :pointer ,len)))
        (cffi:with-foreign-array (str-arr ptrs arr-type)
          (release-unwind-protect
              (%gl:transform-feedback-varyings prog-id len str-arr mode)
            (map nil #'cffi:foreign-free ptrs))))))
  (values))

(defun+ link-shaders (shaders
                      prog-id
                      compiled-stages
                      transform-feedback-mode
                      transform-feedback-names)
  "Links all the shaders into the program provided"
  (assert prog-id)
  (release-unwind-protect
      (progn
        (loop :for shader :in shaders :do
           (%gl:attach-shader prog-id shader))

        ;;
        ;; enable tranform feedback
        (when transform-feedback-names
          (enable-transform-feedback prog-id
                                     transform-feedback-mode
                                     transform-feedback-names))

        (%gl:link-program prog-id)
        ;;
        ;;check for linking errors
        (unless (gl:get-program prog-id :link-status)
          (error (format nil "Error Linking Program~%~a~%~%Compiled-stages:~{~%~% ~a~}"
                         (gl:get-program-info-log prog-id)
                         (mapcar #'glsl-code compiled-stages)))))
    (loop :for shader :in shaders :do
       (gl:detach-shader prog-id shader)))
  (values))

(defmethod free ((pipeline-name symbol))
  (free-pipeline pipeline-name))

(defmethod free ((pipeline-func function))
  (free-pipeline pipeline-func))

(defun+ free-pipeline (pipeline)
  (labels ((walking-delete (name)
             (let ((next (gethash name *gpu-pipeline-specs*)))
               (remhash name *gpu-pipeline-specs*)
               (when next
                 (walking-delete next)))))
    (with-slots (prog-ids) (pipeline-spec pipeline)
      (bt:with-lock-held (*gpu-pipeline-specs-lock*)
        (let ((prog-id (etypecase prog-ids
                         (array (aref prog-ids (context-id (cepl-context))))
                         (integer prog-ids))))
          (gl:delete-program prog-id)
          (setf prog-ids nil)
          (walking-delete pipeline)
          nil)))))
