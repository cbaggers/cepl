(in-package :cepl.pipelines)
(in-readtable fn:fn-reader)

(defmacro g-> (context &rest forms)
  (declare (ignore context forms))
  (error "Sorry, making anonomous pipelines is not currently supported"))

(defmacro def-g-> (name context &body gpipe-args)
  (assert-valid-gpipe-form name gpipe-args)
  (%defpipeline-gfuncs name gpipe-args context))

(defun %defpipeline-gfuncs (name gpipe-args context &optional suppress-compile)
  ;;
  ;; {todo} explain
  (destructuring-bind (stage-pairs post) (parse-gpipe-args gpipe-args)
    ;;
    (let* ((stage-names (mapcar #'cdr stage-pairs))
	   (uniform-assigners (stages->uniform-assigners stage-names)))
      (assert-valid-stage-specs stage-names)
      (let (;; we generate the func that compiles & uploads the pipeline
	    ;; and also populates the pipeline's local-vars
	    (init-func (gen-pipeline-init name stage-pairs post context
					  uniform-assigners)))
	;;
	;; update the spec immediately (macro-expansion time)
	(%update-spec name stage-names context)
	`(progn
	   (let ((prog-id nil)
		 ;; If there are no implicit-uniforms we need a no-op
		 ;; function to call
		 ,@(when (supports-implicit-uniformsp context)
			 `((implicit-uniform-upload-func #'fallback-iuniform-func)))
		 ;;
		 ;; {todo} explain
		 ,@(mapcar λ`(,(first _) -1)
			   (mapcat #'first uniform-assigners)))
	     ;;
	     ;; To help the compiler we make sure it knows it's a function :)
	     ,@(when (supports-implicit-uniformsp context)
		     `((declare (type function implicit-uniform-upload-func))))
	     ;;
	     ;; we upload the spec at compile time
	     ,(gen-update-spec name stage-names context)
	     ;;
	     (labels (,init-func)
	       ;;
	       ;; generate the code that actually renders
	       ,(def-dispatch-func name (first init-func) stage-names
				   context uniform-assigners)))
	   ;;
	   ;; generate the function that recompiles this pipeline
	   ,(gen-recompile-func name gpipe-args context)
	   ,(unless suppress-compile `(,(recompile-name name))))))))

(defun fallback-iuniform-func (prog-id)
  (declare (ignore prog-id) (optimize (speed 3) (safety 1))))

(defun gen-recompile-func (name gpipe-args context)
  `(defun ,(recompile-name name) ()
     (format t "~&; recompile cpu side of (~a ...)~&" ',name)
     (force-output)
     (let ((*standard-output* (make-string-output-stream)))
       (handler-bind ((warning #'muffle-warning))
	 (eval (%defpipeline-gfuncs
		',name ',gpipe-args ',context t))))))

(defun %update-spec (name stage-names context)
  (update-pipeline-spec
   (make-pipeline-spec
    name stage-names (make-pipeline-change-signature stage-names)
    context)))

(defun gen-update-spec (name stage-names context)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-pipeline-spec
      (make-pipeline-spec
       ',name ',stage-names
       ,(make-pipeline-change-signature stage-names)
       ',context))))

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
  (loop :for (name type . i) :in uniforms
     :do (just-ignore i)
     :always
     (loop :for (n tp . i_)
        :in (remove-if-not (lambda (x) (equal (car x) name)) uniforms)
	:do (just-ignore n i_)
        :always (equal type tp))))

(defun %compile-closure (code)
  (funcall (compile nil `(lambda () ,code))))

(defun %post-init (func)
  (unbind-buffer)
  (cepl.streams::bind-vao 0)
  (force-use-program 0)
  (when func (funcall func)))

(defun gen-pipeline-init (name stage-pairs post context uniform-assigners)
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
       prog-id)))

(defun def-dispatch-func (name init-func-name stage-names context
			  uniform-assigners)
  (let* ((uniform-names
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
		   ,(escape-tildes
		     (format nil
			     "~%----------------------------------------
~%name: ~s~%~%pipeline compile context: ~s~%~%uniform assigners:~%~s~%~%uniform transforms:~%~s
~%----------------------------------------"
			     name
			     context
			     uniform-assigners
			     uniform-transforms))))
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
                     (declare (optimize (speed 3) (safety 1)))
                   (funcall implicit-uniform-upload-func prog-id)))
         (when stream (draw-expander stream ,prim-type))
         (use-program 0)
         stream)
       (define-compiler-macro ,name (&whole whole mapg-context &rest rest)
         (declare (ignore rest))
         (unless (mapg-constantp mapg-context)
           (error 'dispatch-called-outside-of-map-g :name ',name))
         whole))))

(defun escape-tildes (str)
  (cl-ppcre:regex-replace-all "~" str "~~"))

(defmacro draw-expander (stream draw-type)
  "This draws the single stream provided using the currently
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  `(let ((stream ,stream)
         (draw-type ,draw-type)
         (index-type (buffer-stream-index-type stream)))
     (bind-vao (buffer-stream-vao stream))
     (if (= |*instance-count*| 0)
         (if index-type
             (%gl:draw-elements draw-type
                                (buffer-stream-length stream)
                                (gl::cffi-type-to-gl index-type)
                                (make-pointer 0))
             (%gl:draw-arrays draw-type
                              (buffer-stream-start stream)
                              (buffer-stream-length stream)))
         (if index-type
             (%gl:draw-elements-instanced
              draw-type
              (buffer-stream-length stream)
              (gl::cffi-type-to-gl index-type)
              (make-pointer 0)
              |*instance-count*|)
             (%gl:draw-arrays-instanced
              draw-type
              (buffer-stream-start stream)
              (buffer-stream-length stream)
              |*instance-count*|)))))


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
      (make-shader (cepl-utils:file-to-string file-path) shader-type)
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
