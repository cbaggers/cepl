(in-package :jungl)
(in-readtable fn:fn-reader)

(defun %defpipeline-gfuncs (name args gpipe-args options
			    &optional suppress-compile)
  ;; {TODO} context is now options, need to parse this
  (when args (error 'shader-pipeline-non-null-args :pipeline-name name))
  (assert-valid-gpipe-form name gpipe-args :shader)
  (let ((pass-key (%gen-pass-key))) ;; used as key for memoization
    (assoc-bind ((context :context) (post :post))
        (parse-options name options :shader)
      (destructuring-bind (stage-pairs gpipe-context)
          (parse-gpipe-args gpipe-args)
        (assert (not (and gpipe-context context)))

        (let* ((context (or context gpipe-context))
               (stage-names (mapcar #'cdr stage-pairs)))
          (assert-valid-stage-specs stage-names)
          (let (;; we generate the func that compiles & uploads the pipeline
                ;; and also populates the pipeline's local-vars
                (init-func (gen-pipeline-init name stage-pairs post pass-key
                                              context)))

            ;; update the spec immediately (macro-expansion time)
            (%update-spec name stage-names context gpipe-context)
            `(progn
               ;; macro that expands to the local vars for the pipeline
               (let-pipeline-vars (,stage-names ,pass-key ,context)
                 ;; we upload the spec at compile time
                 ,(gen-update-spec name stage-names context gpipe-context)
                 (labels (,init-func
                          ,@(fallback-implicit-uniform-func context))
                   ;; generate the code that actually renders
                   ,(def-dispatch-func name (first init-func) stage-names
				       context pass-key)))
               ;; generate the function that recompiles this pipeline
               ,(gen-recompile-func name args gpipe-args options)
               ,(unless suppress-compile `(,(recompile-name name))))))))))

(defun fallback-implicit-uniform-func (context)
  (when (supports-implicit-uniformsp context)
    `((fallback-iuniform-func (prog-id)
                               (declare (ignore prog-id)
                                        (optimize (speed 3) (safety 1)))))))

(defun gen-recompile-func (name args gpipe-args options)
  `(defun ,(recompile-name name) ()
     (format t "~&; recompile cpu side of (~a ...)~&" ',name)
     (force-output)
     (let ((*standard-output* (make-string-output-stream)))
       (handler-bind ((warning #'muffle-warning))
	 (eval (%defpipeline-gfuncs
		',name ',args ',gpipe-args ',options t))))))

(defun %update-spec (name stage-names context gpipe-context)
  (update-pipeline-spec
   (make-shader-pipeline-spec
    name stage-names (make-pipeline-change-signature stage-names)
    (or gpipe-context context))))

(defun gen-update-spec (name stage-names context gpipe-context)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-pipeline-spec
      (make-shader-pipeline-spec
       ',name ',stage-names
       ,(make-pipeline-change-signature stage-names)
       ',(or gpipe-context context)))))

(defvar *all-quiet* nil)

(defun quiet-warning-handler (c)
   (when *all-quiet*
     (let ((r (find-restart 'muffle-warning c)))
       (when r
         (invoke-restart r)))))

(defun make-pipeline-change-signature (stage-names)
  (sxhash
   (format nil "~s"
           (mapcar #'make-change-signature stage-names))))

(defun make-change-signature (stage-name)
  (with-gpu-func-spec (gpu-func-spec stage-name t)
    (list in-args uniforms body
          (mapcar #'make-change-signature
                  (funcs-this-func-uses stage-name)))))

(defmacro let-pipeline-vars ((stage-names pass-key context) &body body)
  (let ((uniform-assigners (stages->uniform-assigners stage-names pass-key)))
    `(let ((prog-id nil)
           ,@(when (supports-implicit-uniformsp context)
                   `((implicit-uniform-upload-func nil)))
           ,@(let ((u-lets (mapcat #'first uniform-assigners)))
                  (mapcar (lambda (_)
                            `(,(first _) -1)) u-lets)))
       ,@body)))

(defun %gl-make-shader-from-varjo (compiled-stage)
  (make-shader (varjo->gl-stage-names (varjo:stage-type compiled-stage))
               (varjo:glsl-code compiled-stage)))

(defun %compile-link-and-upload (name stage-pairs)
  (let* ((compiled-stages (%varjo-compile-as-pipeline stage-pairs))
         (stages-objects (mapcar #'%gl-make-shader-from-varjo
                                 compiled-stages)))
    (format t "~&; uploading (~a ...)~&" name)
    (let ((prog-id (request-program-id-for name)))
      (link-shaders stages-objects prog-id compiled-stages)
      (when +cache-last-compile-result+
	(add-compile-results-to-pipeline name compiled-stages))
      (mapcar #'%gl:delete-shader stages-objects)
      (values compiled-stages prog-id))))

(defun %create-implicit-uniform-uploader (compiled-stages)
  (let ((uniforms (mapcat #'varjo:implicit-uniforms compiled-stages)))
    (when uniforms
      (let* ((assigners (mapcar #'make-arg-assigners uniforms))
             (u-lets (mapcat #'first assigners)))
        (%compile-closure
         `(let ((initd nil)
                ,@(mapcar (lambda (_) `(,(first _) -1)) u-lets))
            (lambda (prog-id)
              (unless initd
                ,@(mapcar (lambda (_) (cons 'setf _)) u-lets))
              ,@(mapcar #'second assigners))))))))

(defun supports-implicit-uniformsp (context)
  (not (member :no-iuniforms context)))

(defun %implicit-uniforms-dont-have-type-mismatches (uniforms)
  (loop :for (name type . i) :in uniforms :always
     (loop :for (n tp . i_)
        :in (remove-if-not (lambda (x) (equal (car x) name)) uniforms)
        :always (equal type tp))))

(defun %compile-closure (code)
  (funcall (compile nil `(lambda () ,code))))

(defun %post-init (func)
  (unbind-buffer)
  (force-bind-vao 0)
  (force-use-program 0)
  (when func (funcall func)))

(defun gen-pipeline-init (name stage-pairs post pass-key context)
  (let* ((stage-names (mapcar #'cdr stage-pairs))
         (uniform-assigners (stages->uniform-assigners stage-names pass-key)))
    `(,(gensym "init")
      ()
      (let ((image-unit -1))
        (declare (ignorable image-unit))
        (multiple-value-bind (compiled-stages new-prog-id)
	    (%compile-link-and-upload ',name ',stage-pairs)
	  (declare (ignorable compiled-stages))
	  (setf prog-id new-prog-id)
          ,(when (supports-implicit-uniformsp context)
                 `(setf implicit-uniform-upload-func
                        (or (%create-implicit-uniform-uploader compiled-stages)
                            #'fallback-iuniform-func))))
        ,@(let ((u-lets (mapcat #'first uniform-assigners)))
               (loop for u in u-lets collect (cons 'setf u)))
        (%post-init ,(car post))
        prog-id))))

(defun def-dispatch-func (name init-func-name stage-names context pass-key)
  (let* ((uniform-assigners (stages->uniform-assigners stage-names pass-key))
         (uniform-names
          (mapcar #'first (aggregate-uniforms stage-names)))
	 (actual-uniform-names
	  (mapcar #'first (aggregate-uniforms stage-names nil t)))
	 (uniform-transforms
	  (remove-duplicates
	   (remove nil
		   (mapcar λ(when (member (first _) actual-uniform-names)
			      _)
			   (reduce
			    (lambda (accum name)
			      (with-gpu-func-spec (gpu-func-spec name)
				(append accum uniform-transforms)))
			    stage-names
			    :initial-value nil)))
	   :test #'equal))
         (prim-type (varjo:get-primitive-type-from-context context))
         (u-uploads (mapcar #'second uniform-assigners)))
    `(progn
       (defun ,(symb :%touch- name) (&key verbose)
	 (let ((*verbose-compiles* verbose))
	   (unless prog-id
	     (setf prog-id (,init-func-name))))
	 (when verbose
	   (format t
		   ,(format nil
			    "~%----------------------------------------
~%name: ~s~%~%pipeline compile context: ~s~%~%uniform assigners:~%~s~%~%uniform transforms:~%~s
~%----------------------------------------"
			    name
			    context
			    uniform-assigners
			    uniform-transforms)))
	 t)
       (defun ,name (mapg-context stream ,@(when uniform-names `(&key ,@uniform-names)))
         (declare (ignore mapg-context) (ignorable ,@uniform-names))
         (unless prog-id
           (setf prog-id (,init-func-name))
           (unless prog-id (return-from ,name)))
         (use-program prog-id)
	 (let ,uniform-transforms
	   ,@u-uploads)
         ,(when (supports-implicit-uniformsp context)
                `(locally
                     (declare (function implicit-uniform-upload-func)
                              (optimize (speed 3) (safety 1)))
                   (funcall implicit-uniform-upload-func prog-id)))
         (when stream (draw-expander stream ,prim-type))
         (use-program 0)
         stream)
       (define-compiler-macro ,name (&whole whole mapg-context &rest rest)
         (declare (ignore rest))
         (unless (mapg-constantp mapg-context)
           (error 'dispatch-called-outside-of-map-g :name ',name))
         whole))))

(defmacro draw-expander (stream draw-type)
  "This draws the single stream provided using the currently
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  `(let ((stream ,stream)
         (draw-type ,draw-type)
         (index-type (vertex-stream-index-type stream)))
     (bind-vao (vertex-stream-vao stream))
     (if (= |*instance-count*| 0)
         (if index-type
             (%gl:draw-elements draw-type
                                (vertex-stream-length stream)
                                (gl::cffi-type-to-gl index-type)
                                (make-pointer 0))
             (%gl:draw-arrays draw-type
                              (vertex-stream-start stream)
                              (vertex-stream-length stream)))
         (if index-type
             (%gl:draw-elements-instanced
              draw-type
              (vertex-stream-length stream)
              (gl::cffi-type-to-gl index-type)
              (make-pointer 0)
              |*instance-count*|)
             (%gl:draw-arrays-instanced
              draw-type
              (vertex-stream-start stream)
              (vertex-stream-length stream)
              |*instance-count*|)))))



;;;--------------------------------------------------------------
;;; ARG ASSIGNERS ;;;
;;;---------------;;;

(defun stages->uniform-assigners (stage-names &optional pass-key)
  (mapcar (lambda (_) (make-arg-assigners _ pass-key))
          (aggregate-uniforms stage-names nil t)))

(let ((cached-data nil)
      (cached-key -1))
  (defun make-arg-assigners (uniform-arg &optional pass-key)
    ;; This function is pretty much just memoization for %make-arg-assigners
    (if (and pass-key (= cached-key pass-key)
             (assoc uniform-arg cached-data :test #'equal))
        (return-from make-arg-assigners
          (cdr (assoc uniform-arg cached-data :test #'equal)))
        ;; the call here -vvvv is the only bit of real work in this function
        (let ((result (%make-arg-assigners uniform-arg)))
          (when pass-key
            (when (not (= cached-key pass-key))
              (setf cached-data nil)
              (setf cached-key pass-key))
            (setf cached-data (acons uniform-arg result cached-data)))
          result))))

(defun %make-arg-assigners (uniform-arg)
  (varjo:with-v-arg (arg-name varjo-type~1 qualifiers glsl-name) uniform-arg
    (let* ((local-arg-name 'val)
           (glsl-name (or glsl-name (varjo:safe-glsl-name-string arg-name)))
           (assigner (dispatch-make-assigner local-arg-name varjo-type~1
                                             glsl-name qualifiers)))
      `(,(let-forms assigner)
         (when ,arg-name
           (let ((,local-arg-name ,(if (pointer-arg assigner)
                                       `(pointer ,arg-name)
                                       arg-name)))
             ,@(uploaders assigner)))))))

(defun dispatch-make-assigner (arg-name type glsl-name qualifiers)
  (assert (not (null glsl-name)))
  (let* ((varjo-type (varjo:type-spec->type type))
         (struct-arg (varjo:v-typep varjo-type 'varjo:v-user-struct))
         (array-length (when (v-typep varjo-type 'v-array)
                         (apply #'* (v-dimensions varjo-type))))
         (sampler (sampler-typep varjo-type))
         (ubo (member :ubo qualifiers)))
    (cond
      (ubo (make-ubo-assigner arg-name varjo-type glsl-name))
      ;;
      (array-length (make-array-assigners arg-name varjo-type glsl-name))
      ;;
      (struct-arg (make-struct-assigners arg-name varjo-type glsl-name))
      ;;
      (sampler (make-sampler-assigner arg-name varjo-type glsl-name))
      ;;
      (t (make-simple-assigner arg-name varjo-type glsl-name nil)))))

;; {TODO} Why does this not have a byte-offset? Very tired cant work it out :)
(defun make-sampler-assigner (arg-name type glsl-name-path)
  (let ((id-name (gensym))
        (i-unit (gensym "IMAGE-UNIT")))
    (make-assigner
     :let-forms `((,id-name (gl:get-uniform-location prog-id ,glsl-name-path))
                  (,i-unit (incf image-unit)))
     :uploaders `((when (>= ,id-name 0)
                     (unless (eq (sampler-type ,arg-name) ,(type->spec type))
                       (error "incorrect texture type passed to shader"))
                     (active-texture-num ,i-unit)
                     (bind-texture ,arg-name)
                     (gl:bind-sampler ,i-unit (slot-value ,arg-name 'sampler-object-id))
                     (uniform-sampler ,id-name ,i-unit))))))

(defun make-ubo-assigner (arg-name varjo-type glsl-name)
  (let ((id-name (gensym))
        (type-spec (varjo:type->type-spec varjo-type)))
    (make-assigner
     :let-forms
     `((,id-name (get-uniform-block-index
                  prog-id ,(format nil "_UBO_~a" glsl-name))))
     :uploaders
     `((when (>= ,id-name 0)
          (if (and (typep ,arg-name 'ubo)
                   (v-type-eq (varjo:type-spec->type ',type-spec)
                              (ubo-data-type ,arg-name)))
              (%gl:uniform-block-binding prog-id ,id-name (ubo-id ,arg-name))
              (error "Invalid type for ubo argument:~%Required:~a~%Recieved:~a~%"
                     ',type-spec (ubo-data-type ,arg-name))))))))

;; NOTE: byte-offset can be nil, this is a legit value
(defun make-simple-assigner (arg-name type glsl-name-path
                             &optional (byte-offset 0))
  (let ((id-name (gensym)))
    (make-assigner
     :let-forms `((,id-name (gl:get-uniform-location prog-id ,glsl-name-path)))
     :uploaders
     `((when (>= ,id-name 0)
	 ;; If we have a byte offset then we need index into that block of
	 ;; foreign data to upload (think structs) so we need to use
	 ;; #'get-foreign-uniform-function-name.
         ,(if byte-offset
              `(,(get-foreign-uniform-function-name (type->spec type))
                 ,id-name 1 (cffi:inc-pointer ,arg-name ,byte-offset))
              `(,(get-uniform-function-name (type->spec type)) ,id-name ,arg-name)))))))

(defun make-array-assigners (arg-name type glsl-name-path &optional (byte-offset 0))
  (let ((element-type (varjo:v-element-type type))
        (array-length (apply #'* (v-dimensions type))))
    (merge-into-assigner
     t
     (loop :for i :below array-length
        :if (varjo:v-typep element-type 'varjo:v-user-struct) :append
        (make-struct-assigners arg-name element-type
                               (format nil "~a[~a]" glsl-name-path i)
                               byte-offset)
        :else :collect
        (make-simple-assigner arg-name element-type
                              (format nil "~a[~a]" glsl-name-path i)
                              byte-offset)
        :do (incf byte-offset (gl-type-size (type->spec element-type)))))))


(defun make-struct-assigners (arg-name type glsl-name-path
                              &optional (byte-offset 0))
  (merge-into-assigner
   t
   (loop
      :for (l-slot-name v-slot-type) :in (varjo:v-slots type)
      :for (pslot-type array-length . rest) := (listify v-slot-type)
      :append
      (let* ((pslot-type (type-spec->type pslot-type))
             (glsl-name (varjo:safe-glsl-name-string l-slot-name))
             (glsl-name-path (format nil "~a.~a" glsl-name-path glsl-name)))
        (cond
          ;;
          (array-length (make-array-assigners arg-name v-slot-type
                                              glsl-name-path byte-offset))
          ;;
          ((varjo:v-typep pslot-type 'v-user-struct)
           (make-struct-assigners arg-name pslot-type glsl-name-path
                                  byte-offset))
          ;;
          (t (list (make-simple-assigner arg-name pslot-type glsl-name-path
                                         byte-offset)))))
      :do (when byte-offset
	    (incf byte-offset (* (gl-type-size pslot-type)
				 (or array-length 1)))))))


(defclass assigner ()
  ((let-forms :initform nil :initarg :let-forms :accessor let-forms)
   (uploaders :initform nil :initarg :uploaders :accessor uploaders)
   (pointer-arg :initform nil :initarg :pointer-arg :accessor pointer-arg)))

(defun make-assigner (&key let-forms uploaders pointer-arg)
  (make-instance 'assigner :let-forms let-forms :uploaders uploaders
                 :pointer-arg pointer-arg))

(defun merge-into-assigner (pointer-arg assingers)
  (make-assigner :let-forms (mapcat #'let-forms assingers)
                 :uploaders (mapcat #'uploaders assingers)
                 :pointer-arg pointer-arg))

;;;--------------------------------------------------------------
;;; GL HELPERS ;;;
;;;------------;;;

(defun program-attrib-count (program)
  "Returns the number of attributes used by the shader"
  (gl:get-program program :active-attributes))

(defun program-attributes (program)
  "Returns a list of details of the attributes used by
   the program. Each element in the list is a list in the
   format: (attribute-name attribute-type attribute-size)"
  (loop for i from 0 below (program-attrib-count program)
     collect (multiple-value-bind (size type name)
                 (gl:get-active-attrib program i)
               (list name type size))))

(defun program-uniform-count (program)
  "Returns the number of uniforms used by the shader"
  (gl:get-program program :active-uniforms))

(defun program-uniforms (program-id)
  "Returns a list of details of the uniforms used by
   the program. Each element in the list is a list in the
   format: (uniform-name uniform-type uniform-size)"
  (loop for i from 0 below (program-uniform-count program-id)
     collect (multiple-value-bind (size type name)
                 (gl:get-active-uniform program-id i)
               (list name type size))))

(let ((program-cache nil))
  (defun use-program (program-id)
    (unless (eq program-id program-cache)
      (gl:use-program program-id)
      (setf program-cache program-id)))
  (defun force-use-program (program-id)
    (gl:use-program program-id)
    (setf program-cache program-id)))
(setf (documentation 'use-program 'function)
      "Installs a program object as part of current rendering state")

;; [TODO] Expand on this and allow loading on strings/text files for making
;;        shaders
(defun shader-type-from-path (path)
  "This uses the extension to return the type of the shader.
   Currently it only recognises .vert or .frag files"
  (let* ((plen (length path))
         (exten (subseq path (- plen 5) plen)))
    (cond ((equal exten ".vert") :vertex-shader)
          ((equal exten ".frag") :fragment-shader)
          (t (error "Could not extract shader type from shader file extension (must be .vert or .frag)")))))

(defun make-shader
    (shader-type source-string &optional (shader-id (gl:create-shader
                                                     shader-type)))
  "This makes a new opengl shader object by compiling the text
   in the specified file and, unless specified, establishing the
   shader type from the file extension"
  (gl:shader-source shader-id source-string)
  (gl:compile-shader shader-id)
  ;;check for compile errors
  (when (not (gl:get-shader shader-id :compile-status))
    (error "Error compiling ~(~a~): ~%~a~%~%~a"
           shader-type
           (gl:get-shader-info-log shader-id)
           source-string))
  shader-id)

(defun load-shader (file-path
                    &optional (shader-type
                               (shader-type-from-path file-path)))
  (restart-case
      (make-shader (utils:file-to-string file-path) shader-type)
    (reload-recompile-shader () (load-shader file-path
                                             shader-type))))

(defun load-shaders (&rest shader-paths)
  (mapcar #'load-shader shader-paths))

(defun link-shaders (shaders program_id compiled-stages)
  "Links all the shaders provided and returns an opengl program
   object. Will recompile an existing program if ID is provided"
  (let ((program (or program_id (gl:create-program))))
    (unwind-protect
         (progn (loop :for shader :in shaders :do
                   (gl:attach-shader program shader))
                (gl:link-program program)
                ;;check for linking errors
                (if (not (gl:get-program program :link-status))
                    (error (format nil "Error Linking Program~%~a~%~%Compiled-stages:~{~%~% ~a~}"
                                   (gl:get-program-info-log program)
                                   (mapcar #'glsl-code compiled-stages)))))
      (loop :for shader :in shaders :do
         (gl:detach-shader program shader)))
    program))
