(in-package :cepl.pipelines)
(in-readtable :fn.reader)

(defvar *init-pipeline-lock* (bt:make-lock))

(defun+ function-arg-p (arg)
  (typep (varjo:type-spec->type (second arg))
         'varjo:v-function-type))

(defun+ stages-require-partial-pipeline (stage-keys)
  (some λ(with-gpu-func-spec (gpu-func-spec _)
           (or (some #'function-arg-p in-args)
               (some #'function-arg-p uniforms)))
        stage-keys))

(defun+ has-func-type-in-args (stage-keys)
  (some λ(with-gpu-func-spec (gpu-func-spec _)
           (some #'function-arg-p in-args))
        stage-keys))

(defun+ function-uniforms (stage-keys)
  (mapcat λ(with-gpu-func-spec (gpu-func-spec _)
             (remove-if-not #'function-arg-p uniforms))
          stage-keys))

(defmacro def-g-> (name context &body gpipe-args)
  `(defpipeline-g ,name ,context ,@gpipe-args))

(defmacro defpipeline-g (name context &body gpipe-args)
  (assert-valid-gpipe-form name gpipe-args)
  (%defpipeline-gfuncs name gpipe-args context))

(defun+ %defpipeline-gfuncs
    (name gpipe-args context &optional suppress-compile)
  ;;
  ;; {todo} explain
  (destructuring-bind (stage-pairs post) (parse-gpipe-args gpipe-args)
    ;;
    (let* ((stage-keys (mapcar #'cdr stage-pairs))
           (aggregate-uniforms (aggregate-uniforms stage-keys nil t)))
      (if (stages-require-partial-pipeline stage-keys)
          (%def-partial-pipeline name stage-keys stage-pairs aggregate-uniforms
                                 context)
          (%def-complete-pipeline name stage-keys stage-pairs aggregate-uniforms
                                  post context suppress-compile)))))

(defun+ %def-partial-pipeline (name stage-keys stage-pairs aggregate-uniforms
                              context)
  ;;
  ;; freak out if try and use funcs in stage args
  (when (has-func-type-in-args stage-keys)
    (error 'functions-in-non-uniform-args :name name))
  ;;
  ;; update the spec immediately (macro-expansion time)
  (%update-spec name stage-pairs context)
  ;;
  (let ((uniform-names (mapcar #'first aggregate-uniforms)))
    `(progn
       ;;
       ;; we upload the spec at compile time (using eval-when)
       ,(gen-update-spec name stage-pairs context)
       ;;
       ;; generate the dummy dispatch func
       (defun ,name (mapg-context stream ,@(when uniform-names `(&key ,@uniform-names)))
         (declare (ignore mapg-context) (ignorable ,@uniform-names))
         (use-program mapg-context 0)
         (error 'mapping-over-partial-pipeline
                :name ',name
                :args ',(function-uniforms stage-keys))
         stream)
       (register-named-pipeline ',name #',name)
       ',name)))

(defun+ %def-complete-pipeline (name stage-keys stage-pairs aggregate-uniforms
                               post context &optional suppress-compile)
  (let* ((uniform-assigners (mapcar #'make-arg-assigners aggregate-uniforms))
         ;; we generate the func that compiles & uploads the pipeline
         ;; and also populates the pipeline's local-vars
         (primitive (varjo.internals:primitive-name-to-instance
                     (varjo.internals:get-primitive-type-from-context context)))
         (init-func (gen-pipeline-init name primitive stage-pairs post
                                       uniform-assigners stage-keys)))
    ;;
    ;; update the spec immediately (macro-expansion time)
    (%update-spec name stage-pairs context)
    (multiple-value-bind (cpr-macro dispatch)
        (def-dispatch-func name (first init-func) stage-keys
                           context primitive uniform-assigners
                           (find :static context))
      `(progn
         ,cpr-macro
         (let ((prog-ids (make-array +max-context-count+
                                     :initial-element +unknown-gl-id+
                                     :element-type 'gl-id))
               ;; If there are no implicit-uniforms we need a no-op
               ;; function to call
               (implicit-uniform-upload-func #'fallback-iuniform-func)
               ;;
               ;; {todo} explain
               ,@(mapcar λ`(,(first _) -1)
                         (mapcat #'let-forms uniform-assigners))
               ;;
               ;; The primitive used by transform feedback. When nil
               ;; the primitive comes from the render-mode
               (tfs-primitive nil)
               (tfs-array-count 0))
           ;;
           ;; To help the compiler we make sure it knows it's a function :)
           (declare (type function implicit-uniform-upload-func)
                    (type (simple-array gl-id (#.cepl.context:+max-context-count+))
                          prog-ids)
                    (type symbol tfs-primitive)
                    (type (unsigned-byte 8) tfs-array-count))
           ;;
           ;; we upload the spec at compile time (using eval-when)
           ,(gen-update-spec name stage-pairs context)
           ;;
           (labels (,init-func)
             ;;
             ;; generate the code that actually renders
             ,dispatch))
         ;;
         ;; generate the function that recompiles this pipeline
         ,(gen-recompile-func name stage-pairs post context)
         ,(unless suppress-compile `(,(recompile-name name)))))))

(defun+ fallback-iuniform-func (prog-id &rest uniforms)
  (declare (ignore prog-id uniforms) (optimize (speed 3) (safety 1))))

(defun+ gen-recompile-func (name stage-pairs post context)
  (let* ((stages (mapcat λ(list (car _) (func-key->name (cdr _))) stage-pairs))
         (gpipe-args (append stages (list :post post))))
    `(defun+ ,(recompile-name name) ()
       (format t "~&; recompile cpu side of (~a ...)~&" ',name)
       (force-output)
       (let ((*standard-output* (make-string-output-stream)))
         (handler-bind ((warning #'muffle-warning))
           (eval (%defpipeline-gfuncs
                  ',name ',gpipe-args ',context t)))))))

(defun+ %update-spec (name stage-pairs context)
  (update-pipeline-spec
   (make-pipeline-spec name stage-pairs context)))

(defun+ gen-update-spec (name stage-pairs context)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-pipeline-spec
      (make-pipeline-spec
       ',name ,(cons 'list (mapcar (lambda (x)
                                     (dbind (k . v) x
                                       `(cons ,k ,(spec->func-key v))))
                                   stage-pairs))
       ',context))))

(defun+ register-named-pipeline (name func)
  (declare (profile t))
  (setf (function-keyed-pipeline func)
        name))

(defvar *all-quiet* nil)

(defun+ quiet-warning-handler (c)
  (when *all-quiet*
    (let ((r (find-restart 'muffle-warning c)))
      (when r
        (invoke-restart r)))))

(defun+ %gl-make-shader-from-varjo (compiled-stage)
  (make-shader (varjo->gl-stage-names compiled-stage)
               (varjo:glsl-code compiled-stage)))

(defun+ pairs-key-to-stage (stage-pairs)
  (mapcar λ(dbind (name . key) _ (cons name (gpu-func-spec key)))
          stage-pairs))

(defun+ swap-versions (stage-pairs glsl-version)
  (mapcar λ(dbind (x . s) _
             (let ((new-context
                    (swap-version glsl-version (with-gpu-func-spec s context))))
               (cons x (clone-stage-spec s :new-context new-context))))
          stage-pairs))

(defun+ %compile-link-and-upload (name draw-mode stage-pairs)
  (let* ((stage-pairs (pairs-key-to-stage stage-pairs))
         (glsl-version (compute-glsl-version-from-stage-pairs stage-pairs))
         (stage-pairs (swap-versions stage-pairs glsl-version))
         (compiled-stages (%varjo-compile-as-pipeline draw-mode stage-pairs))
         (stages-objects (mapcar #'%gl-make-shader-from-varjo compiled-stages)))
    (format t "~&; uploading (~a ...)~&" (or name "GPU-LAMBDA"))
    ;; when compiling lambda pipelines prog-ids will be a number, not a vector
    (multiple-value-bind (prog-ids prog-id)
        (request-program-id-for (cepl-context) name)
      (multiple-value-bind (tfb-mode tfb-names tfb-group-count)
          (calc-feedback-style-and-names (get-feedback-out-vars compiled-stages))
        (link-shaders stages-objects prog-id compiled-stages tfb-mode tfb-names)
        (when (and name +cache-last-compile-result+)
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
           (get-version-from-context (context)
             (first (remove-if-not λ(member _ varjo:*supported-versions*)
                                   context)))
           (get-glsl-version (&rest contexts)
             (let* ((versions (mapcar #'get-version-from-context contexts))
                    (trimmed (remove-duplicates (remove nil versions))))
               (case= (length trimmed)
                 (0 (cepl.context::get-best-glsl-version))
                 (1 (first trimmed))
                 (otherwise nil)))))
    (let ((contexts (mapcar #'get-context stage-pairs)))
      (or (apply #'get-glsl-version contexts)
          (throw-version-error
           stage-pairs (mapcar #'get-version-from-context contexts))))))

(defun+ throw-version-error (pairs versions)
  (let ((issue (remove-if-not #'second
                              (mapcar λ(list (car _) _1)
                                      pairs versions))))
    (error 'glsl-version-conflict :pairs issue)))

(defun+ %create-implicit-uniform-uploader (compiled-stages uniform-names)
  (let* ((varjo-implicit (remove-if #'varjo:ephemeral-p
                                    (mapcat #'varjo:implicit-uniforms
                                            compiled-stages)))
         (uniform-arg-forms (mapcar #'varjo.internals:to-arg-form varjo-implicit)))
    ;;
    (when uniform-arg-forms
      (let* ((assigners (mapcar #'make-arg-assigners uniform-arg-forms))
             (u-lets (mapcat #'let-forms assigners))
             (uniform-transforms
              (remove-duplicates (mapcar λ(list (varjo:name _)
                                                (varjo.internals:cpu-side-transform _))
                                         varjo-implicit)
                                 :test #'equal)))
        (%compile-closure
         `(let ((initd nil)
                ;; {todo} is this related to the 'todo' above?
                ,@(mapcar (lambda (_) `(,(first _) -1)) u-lets))
            (lambda (prog-id ,@uniform-names)
              (declare (ignorable ,@uniform-names))
              (unless initd
                ,@(mapcar (lambda (_) (cons 'setf _)) u-lets)
                (setf initd t))
              (let ,uniform-transforms
                ,@(mapcar #'gen-uploaders-block assigners)))))))))

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

(defun+ gen-pipeline-init (name primitive stage-pairs post uniform-assigners
                                stage-keys)
  (let ((uniform-names
         (mapcar #'first (aggregate-uniforms stage-keys))))
    `(,(gensym "init")
       ()
       (prog1
           (let (;; all image units will be >0 as 0 is used as scratch tex-unit
                 (image-unit 0))
             (declare (ignorable image-unit))
             (bt:with-lock-held (*init-pipeline-lock*)
               (multiple-value-bind (compiled-stages
                                     new-prog-id
                                     new-prog-ids
                                     tfb-group-count)
                   (%compile-link-and-upload
                    ',name ',primitive ,(serialize-stage-pairs stage-pairs))
                 (declare (ignorable compiled-stages))
                 (setf prog-ids new-prog-ids)
                 (setf (slot-value (pipeline-spec ',name) 'prog-ids) prog-ids)
                 (use-program (cepl-context) new-prog-id)
                 (setf implicit-uniform-upload-func
                       (or (%create-implicit-uniform-uploader compiled-stages
                                                              ',uniform-names)
                           #'fallback-iuniform-func))
                 (let ((prog-id new-prog-id))
                   (declare (ignorable prog-id))
                   ,@(let ((u-lets (mapcat #'let-forms uniform-assigners)))
                       (loop for u in u-lets collect (cons 'setf u))))
                 (when (> tfb-group-count 0)
                   (setf tfs-primitive
                         (get-transform-feedback-primitive compiled-stages))
                   (setf tfs-array-count tfb-group-count))
                 new-prog-ids)))
         (%post-init ,post)))))

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
  (cons 'list (mapcar λ`(cons ,(car _) ,(spec->func-key (cdr _)))
                      stage-pairs)))



(defun+ def-dispatch-func (name init-func-name stage-keys context
                                primitive uniform-assigners static)
  (let* ((ctx *pipeline-body-context-var*)
         (uniform-names (mapcar #'first (aggregate-uniforms stage-keys)))
         (u-uploads (mapcar #'gen-uploaders-block uniform-assigners))
         (u-cleanup (mapcar #'gen-cleanup-block (reverse uniform-assigners)))
         (typed-uniforms (loop :for name :in uniform-names :collect
                            `(,name t nil)))
         (def (if static 'defn 'defun+))
         (signature (if static
                        `(((,ctx cepl-context)
                           (stream (or null buffer-stream))
                           ,@(when uniform-names `(&key ,@typed-uniforms)))
                          (values))
                        `((,ctx
                           stream
                           ,@(when uniform-names `(&key ,@uniform-names)))))))
    (values
     ;;
     ;; eval-when so we dont get a 'use before compiled' warning
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (define-compiler-macro ,name (&whole whole ,ctx &rest rest)
          (declare (ignore rest))
          (unless (mapg-context-p ,ctx)
            (error 'dispatch-called-outside-of-map-g :name ',name))
          whole))

     ;;
     ;; touch function
     `(progn
        (defun+ ,(symb :%touch- name) (&key verbose)
          #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (let* ((ctx-id (context-id (cepl-context)))
                 (prog-id (aref prog-ids ctx-id)))
            (when (= prog-id +unknown-gl-id+)
              (setf prog-ids (,init-func-name))
              (setf prog-id (aref prog-ids ctx-id)))
            (when verbose
              (format t
                      "~a~%~%prog-id: ~a"
                      ,(escape-tildes
                        (format nil
                                "~%----------------------------------------
~%name: ~s~%~%pipeline compile context: ~s~%~%uniform assigners:~%~s~%~%
~%----------------------------------------"
                                name
                                context
                                (mapcar λ(list (let-forms _) _1)
                                        uniform-assigners u-uploads)))
                      prog-id)))
          t)

        ;;
        ;; draw-function
        (,def ,name ,@signature
          (declare (speed 3) (debug 1) (safety 1) (compilation-speed 0)
                   (ignorable ,ctx ,@uniform-names)
                   ,@(when static `((profile t))))
          #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          ,@(unless (typep primitive 'varjo::dynamic)
                    `((when stream
                        (assert
                         (= ,(draw-mode-group-id primitive)
                            (buffer-stream-primitive-group-id stream))
                         ()
                         'buffer-stream-has-invalid-primitive-for-stream
                         :name ',name
                         :pline-prim ',(varjo::lisp-name primitive)
                         :stream-prim (buffer-stream-primitive stream)))))
          (let* ((%ctx-id (context-id ,ctx))
                 (prog-id (aref prog-ids %ctx-id)))

            ;; ensure program-id available, otherwise bail
            (when (= prog-id +unknown-gl-id+)
              (setf prog-ids (,init-func-name))
              (setf prog-id (aref prog-ids %ctx-id))
              (when (= prog-id +unknown-gl-id+) (return-from ,name)))

            ;;
            (use-program ,ctx prog-id)

            ;; Upload uniforms
            (profile-block (,name :uniforms)
              ,@u-uploads
              (funcall implicit-uniform-upload-func prog-id ,@uniform-names))

            ;; Start the draw
            (when stream
              (let ((draw-type ,(if (typep primitive 'varjo::dynamic)
                                    `(buffer-stream-draw-mode-val stream)
                                    (varjo::lisp-name primitive))))
                ;; if we are capturing transform feedback
                (%with-cepl-context-slots (current-tfs) ,ctx
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
                               (or tfs-primitive draw-type))
                              (setf (%tfs-current-prog-id tfs) prog-id))
                            (assert (= tfs-prog-id prog-id) ()
                                    'mixed-pipelines-in-with-tb))))))
                (profile-block (,name :draw)
                  (draw-expander stream draw-type ,primitive))))

            ;; uniform cleanup
            ,@u-cleanup

            ;; map-g is responsible for returning the correct fbo
            (values)))

        ;; top level run forms
        (register-named-pipeline ',name #',name)
        ',name))))

(defun+ escape-tildes (str)
  (cl-ppcre:regex-replace-all "~" str "~~"))

(defmacro draw-expander (stream draw-type primitive)
  "This draws the single stream provided using the currently
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  `(let* ((stream ,stream)
          (draw-type ,draw-type)
          (index-type (buffer-stream-index-type stream)))
     ,@(when (typep primitive 'varjo::patches)
             `((assert (= (buffer-stream-patch-length stream)
                          ,(varjo::vertex-count primitive)))
               (%gl:patch-parameter-i
                :patch-vertices ,(varjo::vertex-count primitive))))
     (with-vao-bound (buffer-stream-vao stream)
       (if (= (the (unsigned-byte 16) |*instance-count*|) 0)
           (if index-type
               (locally (declare (optimize (speed 3) (safety 0))
                                 #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                 (%gl:draw-elements draw-type
                                    (buffer-stream-length stream)
                                    (cffi-type->gl-type index-type)
                                    (%cepl.types:buffer-stream-start-byte stream)))
               (locally (declare (optimize (speed 3) (safety 0))
                                 #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                 (%gl:draw-arrays draw-type
                                  (buffer-stream-start stream)
                                  (buffer-stream-length stream))))
           (if index-type
               (%gl:draw-elements-instanced
                draw-type
                (buffer-stream-length stream)
                (cffi-type->gl-type index-type)
                (%cepl.types:buffer-stream-start-byte stream)
                |*instance-count*|)
               (%gl:draw-arrays-instanced
                draw-type
                (buffer-stream-start stream)
                (buffer-stream-length stream)
                |*instance-count*|))))))

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
              :for qualifiers := (varjo:qualifiers out-var)
              :for feedback := (find-if λ(typep _ 'varjo:feedback-qualifier)
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
    (let ((groups (remove-duplicates (mapcar #'second varying-pairs))))
      (case= (length groups)
        (0 (values nil nil nil))
        (1 (values :interleaved-attribs
                   (mapcar #'get-name varying-pairs)
                   1))
        (otherwise
         (let ((pairs (sort (copy-list varying-pairs) #'< :key #'second)))
           (assert (and (consecutive-integers-p groups) (find 0 groups)) ()
                   'non-consecutive-feedback-groups
                   :groups (sort groups #'<))
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
  (with-slots (prog-ids) (pipeline-spec pipeline)
    (let ((prog-id (aref prog-ids (context-id (cepl-context)))))
      (gl:delete-program prog-id)
      (setf prog-ids nil))))
