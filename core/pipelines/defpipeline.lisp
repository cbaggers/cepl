(in-package :cepl.pipelines)
(in-readtable :fn.reader)

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

(defun+ %defpipeline-gfuncs (name gpipe-args context &optional suppress-compile)
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
         (use-program 0)
         (error 'mapping-over-partial-pipeline
                :name ',name
                :args ',(function-uniforms stage-keys))
         stream)
       (register-named-pipeline ',name #',name)
       ',name)))

(defun+ %def-complete-pipeline (name stage-keys stage-pairs aggregate-uniforms
                               post context &optional suppress-compile)
  (let* ((debug (find :debug context))
         (uniform-assigners (mapcar #'make-arg-assigners aggregate-uniforms))
         ;; we generate the func that compiles & uploads the pipeline
         ;; and also populates the pipeline's local-vars
         (primitive (varjo.internals:primitive-name-to-instance
                     (varjo.internals:get-primitive-type-from-context context)))
         (init-func (gen-pipeline-init name primitive stage-pairs post
                                       uniform-assigners stage-keys debug)))
    ;;
    ;; update the spec immediately (macro-expansion time)
    (%update-spec name stage-pairs context)
    (multiple-value-bind (cpr-macro dispatch)
        (def-dispatch-func name (first init-func) stage-keys
                           context primitive uniform-assigners
                           (find :static context))
      `(progn
         ,cpr-macro
         (let ((prog-id nil)
               ;;
               ,(when debug `(debug-pass-prog-ids (make-array 0 :element-type 'gl-id)))
               ;;
               ;; If there are no implicit-uniforms we need a no-op
               ;; function to call
               (implicit-uniform-upload-func #'fallback-iuniform-func)
               ;;
               ;; {todo} explain
               ,@(mapcar λ`(,(first _) -1)
                         (mapcat #'let-forms uniform-assigners)))
           ;;
           ;; To help the compiler we make sure it knows it's a function :)
           (declare (type function implicit-uniform-upload-func)
                    ,(when debug `(type debug-pass-prog-ids (array gl-id (*)))))
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

(defun+ preprocess-stages-for-version (stage-pairs)
  (let ((glsl-version (compute-glsl-version-from-stage-pairs stage-pairs)))
    (swap-versions stage-pairs glsl-version)))

(defun+ %compile-link-and-upload (name draw-mode stage-pairs)
  (let* ((compiled-stages (%varjo-compile-as-pipeline
                           (mapcar λ(parsed-gpipe-args->v-translate-args
                                     draw-mode _)
                                   stage-pairs)))
         (stages-objects (mapcar #'%gl-make-shader-from-varjo compiled-stages)))
    (format t "~&; uploading (~a ...)~&" (or name "GPU-LAMBDA"))
    (let ((prog-id (request-program-id-for name)))
      (link-shaders stages-objects prog-id compiled-stages)
      (when (and name +cache-last-compile-result+)
        (add-compile-results-to-pipeline name compiled-stages))
      (mapcar #'%gl:delete-shader stages-objects)
      (values compiled-stages prog-id))))

(v-def-glsl-template-fun peek-val (val) "~a" (t) 0 :v-place-index t )

(defun+ %compile-link-and-upload-debug-stages (name compiled-stages)
  (let* ((name `(:debug ,name 0))
         ;; this is too indirect, making gpu-func-specs just to make
         ;; varjo inputs again is dump
         (v-stages (mapcar #'make-vstage-from-compiled-for-debug
                           compiled-stages))
         (compiled-stages (%varjo-compile-as-pipeline v-stages))
         (stages-objects (mapcar #'%gl-make-shader-from-varjo compiled-stages))
         (prog-id (request-program-id-for name)))
    (link-shaders stages-objects prog-id compiled-stages)
    (when (and name +cache-last-compile-result+)
      (add-compile-results-to-pipeline name compiled-stages))
    (mapcar #'%gl:delete-shader stages-objects)
    (values compiled-stages prog-id)))

(defun+ make-vstage-from-compiled-for-debug (compiled-stage)
  (let ((i 0)
        (vars nil))
    (labels ((swap-dbg-for-setf (form)
               (cond
                 ((atom form) form)
                 ((eq (first form) 'peek-val)
                  (let ((vname (intern (format nil "DBG~a" (incf i)))))
                    (push vname vars)
                    `(setf ,vname ,@(mapcar #'swap-dbg-for-setf (rest form)))))
                 (t (mapcar #'swap-dbg-for-setf form))))
             (process-code-for-debug (code)
               (let ((code (swap-dbg-for-setf code)))
                 `(let ,vars
                    ,@code))))
      (make-stage (varjo.internals::starting-stage compiled-stage)
                  (mapcar #'varjo.internals:to-arg-form
                          (input-variables compiled-stage))
                  (mapcar #'varjo.internals:to-arg-form
                          (uniform-variables compiled-stage))
                  (context compiled-stage)
                  (process-code-for-debug (ast->code compiled-stage))
                  (stemcells-allowed compiled-stage)
                  (primitive-in compiled-stage)))))


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
  (force-use-program 0)
  (when func (funcall func))
  (values))

(defun+ gen-pipeline-init (name primitive stage-pairs post uniform-assigners
                          stage-keys debug)
  (let ((uniform-names
         (mapcar #'first (aggregate-uniforms stage-keys))))
    `(,(gensym "init")
       ()
       (let (;; all image units will be >0 as 0 is used as scratch tex-unit
             (image-unit 0)
             (stg-pairs (preprocess-stages-for-version
                         (pairs-key-to-stage
                          ,(serialize-stage-pairs stage-pairs)))))
         (declare (ignorable image-unit))
         (multiple-value-bind (compiled-stages new-prog-id)
             (%compile-link-and-upload ',name ',primitive stg-pairs)
           (when ,debug
             (setf debug-pass-prog-ids
                   (%compile-link-and-upload-debug-stages
                    ',name ',primitive compiled-stages)))
           (declare (ignorable compiled-stages))
           (setf prog-id new-prog-id)
           (setf (slot-value (pipeline-spec ',name) 'prog-id) prog-id)
           (use-program prog-id)
           (setf implicit-uniform-upload-func
                 (or (%create-implicit-uniform-uploader compiled-stages
                                                        ',uniform-names)
                     #'fallback-iuniform-func)))
         ,@(let ((u-lets (mapcat #'let-forms uniform-assigners)))
                (loop for u in u-lets collect (cons 'setf u)))
         (%post-init ,post)
         prog-id))))

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
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (define-compiler-macro ,name (&whole whole ,ctx &rest rest)
          (declare (ignore rest))
          (unless (mapg-context-p ,ctx)
            (error 'dispatch-called-outside-of-map-g :name ',name))
          whole))
     `(progn
        (defun+ ,(symb :%touch- name) (&key verbose)
          #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (unless prog-id
            (setf prog-id (,init-func-name)))
          (when verbose
            (format t
                    ,(escape-tildes
                      (format nil
                              "~%----------------------------------------
~%name: ~s~%~%pipeline compile context: ~s~%~%uniform assigners:~%~s~%~%
~%----------------------------------------"
                              name
                              context
                              (mapcar λ(list (let-forms _) _1)
                                      uniform-assigners u-uploads)))))
          t)
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
                   'buffer-stream-has-invalid-primtive-for-stream
                   :name ',name
                   :pline-prim ',(varjo::lisp-name primitive)
                   :stream-prim (buffer-stream-primitive stream)))))
          (unless prog-id
            (setf prog-id (,init-func-name))
            (unless prog-id (return-from ,name)))
          (use-program prog-id)
          (profile-block (,name :uniforms)
            ,@u-uploads
            (funcall implicit-uniform-upload-func prog-id ,@uniform-names))
          (profile-block (,name :draw)
            (when stream (draw-expander stream ,primitive)))
          ;;(use-program 0)
          ,@u-cleanup
          (values))
        (register-named-pipeline ',name #',name)
        ',name))))

(defun+ escape-tildes (str)
  (cl-ppcre:regex-replace-all "~" str "~~"))

(defmacro draw-expander (stream primitive)
  "This draws the single stream provided using the currently
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  `(let* ((stream ,stream)
          (draw-type ,(if (typep primitive 'varjo::dynamic)
                          `(buffer-stream-draw-mode-val ,stream)
                          (varjo::lisp-name primitive)))
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

(defun+ program-attrib-count (program)
  "Returns the number of attributes used by the shader"
  (gl:get-program program :active-attributes))

(defun+ program-attributes (program)
  "Returns a list of details of the attributes used by
   the program. Each element in the list is a list in the
   format: (attribute-name attribute-type attribute-size)"
  (loop for i from 0 below (program-attrib-count program)
     collect (multiple-value-bind (size type name)
                 (gl:get-active-attrib program i)
               (list name type size))))

(defun+ program-uniform-count (program)
  "Returns the number of uniforms used by the shader"
  (gl:get-program program :active-uniforms))

(defun+ program-uniforms (program-id)
  "Returns a list of details of the uniforms used by
   the program. Each element in the list is a list in the
   format: (uniform-name uniform-type uniform-size)"
  (loop for i from 0 below (program-uniform-count program-id)
     collect (multiple-value-bind (size type name)
                 (gl:get-active-uniform program-id i)
               (list name type size))))

(let ((program-cache 0))
  (declare (type gl-id program-cache))
  (defn use-program ((program-id gl-id)) gl-id
    (unless (= program-id program-cache)
      (%gl:use-program program-id)
      (setf program-cache program-id))
    program-id)
  (defn force-use-program ((program-id gl-id)) gl-id
    (%gl:use-program program-id)
    (setf program-cache program-id)))

(setf (documentation 'use-program 'function)
      "Installs a program object as part of current rendering state")

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

(defun+ link-shaders (shaders program_id compiled-stages)
  "Links all the shaders provided and returns an opengl program
   object. Will recompile an existing program if ID is provided"
  (let ((program (or program_id (%gl:create-program))))
    (release-unwind-protect
         (progn (loop :for shader :in shaders :do
                   (%gl:attach-shader program shader))
                (%gl:link-program program)
                ;;check for linking errors
                (if (not (gl:get-program program :link-status))
                    (error (format nil "Error Linking Program~%~a~%~%Compiled-stages:~{~%~% ~a~}"
                                   (gl:get-program-info-log program)
                                   (mapcar #'glsl-code compiled-stages)))))
      (loop :for shader :in shaders :do
         (gl:detach-shader program shader)))
    program))

(defmethod free ((pipeline-name symbol))
  (free-pipeline pipeline-name))

(defmethod free ((pipeline-func function))
  (free-pipeline pipeline-func))

(defun+ free-pipeline (pipeline)
  (with-slots (prog-id) (pipeline-spec pipeline)
    (gl:delete-program prog-id)
    (setf prog-id nil)))
