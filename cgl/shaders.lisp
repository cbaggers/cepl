(in-package :cgl)

;;{TODO} Almost everything in here could really benefit from being optimized

(defparameter *gl-window* nil)
(defparameter *stage-names* '((:vertex . :vertex-shader)
                              (:fragment . :fragment-shader)
                              (:geometry . :geometry-shader)
                              (:compute . :compute-shader)
                              (:tesselation-evaluation . :tess-evaluation-shader)
                              (:tesselation-control . :tess-control-shader)))
(defvar |*instance-count*| 0)

;;;--------------------------------------------------------------
;;; SHADER & PROGRAMS ;;;
;;;-------------------;;;

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defvar *shader-args* (make-hash-table)))

(let ((assets (make-hash-table)))
  (defun update-shader-asset (name type compile-result recompile-function depends-on)
    (let ((prog-id (when (eq type :pipeline)
                     (or (fourth (gethash name assets))
                         (gl:create-program))))
          (visited nil))
      (loop :for (n cr rf id dp) :being :the :hash-value :of assets
         :if (and (not (member n visited)) (find name dp)) :do
         (format t "~&; recompiling (~a ...)~&" n)
         (push n visited)
         (funcall rf))
      (setf (gethash name assets) (list name compile-result recompile-function
                                        prog-id depends-on type))
      prog-id))
  (defun get-glsl-code (name)
    (let ((asset (gethash name assets)))
      (when asset
        (destructuring-bind (name compile-result recompile-function prog-id depends-on type)
            asset
          (declare (ignore name recompile-function depends-on prog-id))
          (if (eq type :pipeline)
              (mapcar #'varjo:glsl-code compile-result)
              (varjo:glsl-code compile-result))))))
  (defun get-compiled-asset (name)
    (second (gethash name assets)))
  (defun flush-assets () (setf assets nil)))

(defmacro defsmacro (name lambda-list &body body)
  `(varjo::v-defmacro ,name ,lambda-list ,@body))

(defmacro defsfun (name args &body body)
  (let ((recompile-name (symb-package :%cgl name)))
    `(progn
       (defun ,recompile-name ()
         ,(destructuring-bind (in-args context)
                              (varjo:split-arguments args '(&uniforms &context))
                              `(let ((compile-result (varjo::%v-def-external ',name ',in-args
                                                                             ',context ',body)))
                                 (update-shader-asset ',name :function compile-result
                                                      #',recompile-name (used-external-functions compile-result)))))
       (,recompile-name)
       ',name)))

(defmacro defshader (name args &body body)
  (let ((recompile-name (symb-package :%cgl name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (utils:assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context) (instancing :&instancing))
             (utils:lambda-list-split '(&uniform &context &instancing) ',args)
           (setf (gethash ',name *shader-args*) (list in-args uniforms context instancing))))
       (defun ,recompile-name ()
         (let ((compile-result (varjo::translate ',args '(progn ,@body))))
           (update-shader-asset ',name :shader compile-result
                                #',recompile-name (used-external-functions compile-result))))
       (,recompile-name)
       ',name)))

;;[TODO] add to errors.lisp
(defun varjo->gl-stage-names (stage-name)
  (or (cdr (assoc stage-name *stage-names*))
      (error "CGL: ~a is not a known type of shader stage" stage-name)))

(defun extract-args-from-stages (stages)
  (let ((stage-args
         (mapcar (lambda (x) (gethash x *shader-args*))
                 (remove-if-not #'symbolp stages))))
    (aggregate-args-from-stages stage-args t nil)))

(defun aggregate-args-from-stages (stages in-args uniforms)
  (if stages
      (let ((stage (first stages)))
        (destructuring-bind
              (stage-args stage-uniforms stage-context stage-instancing) stage
          (declare (ignore stage-context stage-instancing))
          (aggregate-args-from-stages
           (rest stages)
           (if (eql t in-args) stage-args in-args)
           (aggregate-uniforms stage-uniforms uniforms))))
      `(,in-args &uniform ,@uniforms)))

(defun aggregate-uniforms (from into)
  (if from
      (let ((u (first from)))
        (cond ((not (find (first u) into :test #'equal :key #'first))
               (aggregate-uniforms (rest from) (cons u into)))
              ((find u into :test #'equal)
               (aggregate-uniforms (rest from) into))
              (t (error "Uniforms for the stages are incompatible: ~a ~a" u into))))
      into))

(defmacro defpipeline (name args-or-stage &body stages)
  (let* ((stages (if (symbolp args-or-stage)
                     (cons args-or-stage stages)
                     stages))
         (args (if (symbolp args-or-stage)
                   (extract-args-from-stages stages)
                   args-or-stage)))
    (utils:assoc-bind ((in-args nil) (unexpanded-uniforms :&uniform) 
                       (context :&context) (instancing :&instancing))
        (utils:lambda-list-split '(&uniform &context &instancing) args)
      (declare (ignore instancing))
      (let* ((uniforms (expand-equivalent-types unexpanded-uniforms))
             (init-func-name (symb-package :cgl '%%- name))
             (invalidate-func-name (symb-package :cgl '££- name))
             (uniform-details (mapcar #'make-arg-assigners uniforms))
             (varjo-args
              `(,@in-args
                ,@(when unexpanded-uniforms
                        `(&uniform
                          ,@(loop :for (name type) :in unexpanded-uniforms :collect
                               (if (gethash type *gl-equivalent-type-data*)
                                   `(,name ,(symbol-to-bridge-symbol type))
                                   `(,name ,type)))))
                ,@(when context `(&context ,@context)))))        
        `(let ((program-id nil)
               ,@(let ((u-lets (mapcan #'first uniform-details)))
                      (loop for u in u-lets collect `(,(first u) -1))))
           ,(gen-pipeline-invalidate invalidate-func-name)
           ,(gen-pipeline-init init-func-name varjo-args name 
                               invalidate-func-name (copy-tree uniform-details) stages)
           ,(gen-pipeline-func init-func-name name context
                               unexpanded-uniforms (copy-tree uniform-details)))))))

(defun gen-pipeline-invalidate (invalidate-func-name)
  `(defun ,invalidate-func-name () (setf program-id nil)))

(defun gen-pipeline-init (init-func-name args name invalidate-func-name
                          uniform-details shaders)
  (destructuring-bind (stages post-compile)
      (loop :for shader :in shaders
         :if (and (listp shader) (eq (first shader) :post-compile))
         :collect shader :into post :else :collect shader :into main
         :finally (return (list main post)))
    `(defun ,init-func-name ()
       (let* ((stages ',stages)              
              (compiled-stages
               ,(destructuring-bind (in-args uniforms context)
                                    (varjo:split-arguments args '(&uniform &context))
                   `(varjo::rolling-translate ',in-args ',uniforms ',context
                                              (loop :for i :in stages :collect
                                                 (if (symbolp i)
                                                     (get-compiled-asset i)
                                                     i)))))
              (shaders-objects
               (loop :for compiled-stage :in compiled-stages
                  :collect (make-shader (varjo->gl-stage-names
                                         (varjo::stage-type compiled-stage))
                                        (varjo::glsl-code compiled-stage))))
              (depends-on (loop :for s :in stages :for c :in compiled-stages :append
                             (cond ((symbolp s) `(,s)) ((listp s) (used-external-functions c)))))
              (prog-id (update-shader-asset ',name :pipeline compiled-stages
                                            #',invalidate-func-name depends-on))
              (image-unit -1))
         (declare (ignorable image-unit))
         (format t ,(format nil "~&; uploading (~a ...)~&" name))
         (link-shaders shaders-objects prog-id)
         (mapcar #'%gl:delete-shader shaders-objects)
         
         ,@(let ((u-lets (mapcan #'first uniform-details)))
                (loop for u in u-lets collect (cons 'setf u)))
         (unbind-buffer) (force-bind-vao 0) (force-use-program 0)
         (setf program-id prog-id)
         ,@(loop for p in post-compile append p)
         prog-id))))

(defun gen-pipeline-func (init-func-name name context unexpanded-uniforms
                          uniform-details)
  (let ((uniform-names (mapcar #'first unexpanded-uniforms))
        (prim-type (varjo::get-primitive-type-from-context context))
        (u-uploads (mapcar #'second uniform-details)))
    `(defun ,name (stream ,@(when unexpanded-uniforms `(&key ,@uniform-names)))
       (declare (ignorable ,@uniform-names))
       (unless program-id (setf program-id (,init-func-name)))
       (use-program program-id)
       ,@u-uploads
       (when stream (draw-expander stream ,prim-type))
       (use-program 0)
       stream)))

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

(defmacro with-instances ((count) &body body)
  `(let ((|*instance-count*| ,count))
     (unless (> |*instance-count*| 0)
       (error "Instance count must be greater than 0"))
     ,@body))

;; (defun make-implicit-uniform-uploader (implicit-uniforms)
;;   (let* ((uniforms (mapcar #'(lambda (x) (list (first x) (third x)))
;;                            implicit-uniforms))
;;          (details (loop :for u :in uniforms :collect
;;                      (make-arg-assigners u)))
;;          (uploaders (loop :for u :in details :collect (second u))))
;;     (break "break" details)))

(defmacro defvshader (name args &body body)
  (let ((args (if (find '&context args :test #'symbol-name-equal)
                  (append (copy-list args) `(:vertex)) ; :stemcells
                  (append (copy-list args) `(&context :vertex))))) ; :stemcells
    `(defshader ,name ,args ,@body)))
(defmacro deffshader (name args &body body)
  (let ((args (if (find '&context args :test #'symbol-name-equal)
                  (append (copy-list args) `(:fragment)) ; :stemcells
                  (append (copy-list args) `(&context :fragment))))) ; :stemcells
    `(defshader ,name ,args ,@body)))
(defmacro defgshader (name args &body body)
  (let ((args (if (find '&context args :test #'symbol-name-equal)
                  (append (copy-list args) `(:geometry)) ; :stemcells
                  (append (copy-list args) `(&context :geometry))))) ; :stemcells
    `(defshader ,name ,args ,@body)))

(defmethod gl-pull ((asset-name symbol))
  (get-glsl-code asset-name))

(defun shader-args-to-lisp-args (args)
  (let* ((uni-pos (position '&uniform args :test #'equal :key #'symbol-name))
         (context-pos (position '&context args :test #'equal :key #'symbol-name))
         (in-vars (subseq args 0 (or uni-pos context-pos)))
         (uniforms (when uni-pos (subseq args (1+ uni-pos) context-pos))))
    `(,@(mapcar #'first in-vars)
        ,@(when uniforms (cons '&key (mapcar #'first uniforms))))))

;;---------------------------------------------------------------------

(defun make-arg-assigners (uniform-arg &aux gen-ids assigners)
  (destructuring-bind ((arg-name &optional expanded-from converter)
                       varjo-type~1) uniform-arg
    (let* ((varjo-type (varjo::type-spec->type varjo-type~1))
           (glsl-name (varjo::safe-glsl-name-string arg-name))
           (struct-arg (varjo::v-typep varjo-type 'varjo::v-user-struct))
           (array-length (when (v-typep varjo-type 'v-array)
                           (apply #'* (v-dimensions varjo-type))))
           (sampler (sampler-typep varjo-type)))
      (loop :for (gid asn multi-gid) :in
         (cond (array-length (make-array-assigners varjo-type glsl-name))
               (struct-arg (make-struct-assigners varjo-type glsl-name))
               (sampler `(,(make-sampler-assigner varjo-type glsl-name nil)))
               (t `(,(make-simple-assigner varjo-type glsl-name nil))))
         :do (if multi-gid
                 (progn (loop for g in gid :do (push g gen-ids))
                        (push asn assigners))
                 (progn (push gid gen-ids) (push asn assigners))))
      (let ((val~ (if expanded-from
                      expanded-from
                      (if (or array-length struct-arg)
                          `(pointer ,arg-name)
                          arg-name))))
        `(,(reverse gen-ids)
           (when ,(or expanded-from arg-name)
             (let ((val ,(cond ((null converter) val~) 
                               ((eq (first converter) 'function)
                                `(,(second converter) ,val~))
                               ((eq (first converter) 'lambda)
                                `(labels ((c ,@(rest converter)))
                                   (c ,val~)))
                               (t (error "invalid converter in make-arg-assigners")))))               
               ,@(reverse assigners))))))))

(defun make-sampler-assigner (type path &optional (byte-offset 0))
  (declare (ignore byte-offset))
  (let ((id-name (gensym))
        (i-unit (gensym "IMAGE-UNIT")))
    `(((,id-name (gl:get-uniform-location prog-id ,path))
       (,i-unit (incf image-unit)))
      (when (>= ,id-name 0)
        (unless (eq (sampler-type val) ,(type->spec type))
          (error "incorrect texture type passed to shader"))
        ;; (unless ,id-name
        ;;   (error "Texture uniforms must be populated")) ;; [TODO] this wont work here
        (active-texture-num ,i-unit)
        (bind-texture val)
        (uniform-sampler ,id-name ,i-unit))
      t)))

(defun make-simple-assigner (type path &optional (byte-offset 0))
  (let ((id-name (gensym)))
    `((,id-name (gl:get-uniform-location prog-id ,path))
      (when (>= ,id-name 0)
        ,(if byte-offset
             `(,(get-foreign-uniform-function-name (type->spec type))
                ,id-name 1 (cffi:inc-pointer val ,byte-offset))
             `(,(get-uniform-function-name (type->spec type)) ,id-name val)))
      nil)))

(defun make-array-assigners (type path &optional (byte-offset 0))
  (let ((element-type (varjo::v-element-type type))
        (array-length (apply #'* (v-dimensions type))))
    (loop :for i :below array-length :append
       (cond ((varjo::v-typep element-type 'varjo::v-user-struct)
              (make-struct-assigners element-type byte-offset))
             (t (list (make-simple-assigner element-type
                                            (format nil "~a[~a]" path i)
                                            byte-offset))))
       :do (incf byte-offset (gl-type-size element-type)))))

(defun make-struct-assigners (type path &optional (byte-offset 0))
  (loop :for (l-slot-name v-slot-type) :in (varjo::v-slots type)
     :for glsl-name = (varjo::safe-glsl-name-string l-slot-name) :append
     (destructuring-bind (pslot-type array-length . rest) v-slot-type
       (declare (ignore rest))
       (let ((path (format nil "~a.~a" path glsl-name)))
         (prog1
             (cond (array-length (make-array-assigners v-slot-type path
                                                       byte-offset))
                   ((varjo::v-typep pslot-type 'v-user-struct)
                    (make-struct-assigners pslot-type path byte-offset))
                   (t (list (make-simple-assigner pslot-type path
                                                  byte-offset))))
           (incf byte-offset (* (gl-type-size pslot-type)
                                (or array-length 1))))))))

;;---------------------------------------------------------------------

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

(defun link-shaders (shaders &optional program_id)
  "Links all the shaders provided and returns an opengl program
   object. Will recompile an existing program if ID is provided"
  (let ((program (or program_id (gl:create-program))))
    (unwind-protect
         (progn (loop :for shader :in shaders :do
                   (gl:attach-shader program shader))
                (gl:link-program program)
                ;;check for linking errors
                (if (not (gl:get-program program :link-status))
                    (error (format nil "Error Linking Program~%~a"
                                   (gl:get-program-info-log program)))))
      (loop :for shader :in shaders :do
         (gl:detach-shader program shader)))
    program))
