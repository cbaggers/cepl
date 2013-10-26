(in-package :cgl)

(defparameter *cached-glsl-source-code* (make-hash-table))
(defparameter *gl-context* (dvals:make-dval))
(defparameter *gl-window* nil) 
(defparameter *compile-chain* (make-hash-table))

;; [TODO] Need to be able to delete programs...How does this fit in lisp?

(defparameter *sampler-types*
  '(:isampler-1D :isampler-1d-Array :isampler-2D :isampler-2d-Array
    :isampler-2d-MS :isampler-2d-MS-Array :isampler-2d-Rect
    :isampler-3d :isampler-Buffer :isampler-Cube
    :isampler-Cube-Array :sampler-1D :sampler-1d-Array
    :sampler-1d-Array-Shadow :sampler-1d-Shadow :sampler-2D
    :sampler-2d-Array :sampler-2d-Array-Shadow :sampler-2d-MS
    :sampler-2d-MS-Array :sampler-2d-Rect :sampler-2d-Rect-Shadow
    :sampler-2d-Shadow :sampler-3d :sampler-Buffer :sampler-Cube
    :sampler-Cube-Array :sampler-Cube-Array-Shadow 
    :sampler-Cube-Shadow :usampler-1D :usampler-1d-Array
    :usampler-2D :usampler-2d-Array :usampler-2d-MS
    :usampler-2d-MS-Array :usampler-2d-Rect :usampler-3d 
    :usampler-Buffer :usampler-Cube :usampler-Cube-Array
    :isampler-1D-arb :isampler-1d-Array-arb :isampler-2D-arb 
    :isampler-2d-Array-arb
    :isampler-2d-MS-arb :isampler-2d-MS-Array-arb :isampler-2d-Rect-arb
    :isampler-3d-arb :isampler-Buffer-arb :isampler-Cube-arb
    :isampler-Cube-Array-arb :sampler-1D-arb :sampler-1d-Array-arb
    :sampler-1d-Array-Shadow-arb :sampler-1d-Shadow-arb :sampler-2D-arb
    :sampler-2d-Array-arb :sampler-2d-Array-Shadow-arb :sampler-2d-MS-arb
    :sampler-2d-MS-Array-arb :sampler-2d-Rect-arb :sampler-2d-Rect-Shadow-arb
    :sampler-2d-Shadow-arb :sampler-3d-arb :sampler-Buffer-arb :sampler-Cube-arb
    :sampler-Cube-Array-arb :sampler-Cube-Array-Shadow-arb
    :sampler-Cube-Shadow-arb :usampler-1D-arb :usampler-1d-Array-arb
    :usampler-2D-arb :usampler-2d-Array-arb :usampler-2d-MS-arb
    :usampler-2d-MS-Array-arb :usampler-2d-Rect-arb :usampler-3d-arb
    :usampler-Buffer-arb :usampler-Cube-arb :usampler-Cube-Array-arb))

;;;--------------------------------------------------------------
;;; UNIFORMS ;;;
;;;----------;;;

(defun uniform-1i (location value)
  (gl:uniformi location value))

(defun uniform-sampler (location image-unit)
  (gl:uniformi location image-unit))

(defun uniform-2i (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-2iv location 1 ptr)))

(defun uniform-3i (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-3iv location 1 ptr)))

(defun uniform-4i (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-4iv location 1 ptr)))

(defun uniform-1f (location value)
  (gl:uniformf location value))

(defun uniform-2f (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-2fv location 1 ptr)))

(defun uniform-3f (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-3fv location 1 ptr)))

(defun uniform-4f (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-4fv location 1 ptr)))

(defun uniform-matrix-2ft (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-matrix-2fv location 1 nil ptr)))

(defun uniform-matrix-3ft (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-matrix-3fv location 1 nil ptr)))

(defun uniform-matrix-4ft (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-matrix-4fv location 1 nil ptr)))

(defun uniform-matrix-2fvt (location count value)
  (%gl:uniform-matrix-2fv location count nil value))

(defun uniform-matrix-3fvt (location count value)
  (%gl:uniform-matrix-3fv location count nil value))

(defun uniform-matrix-4fvt (location count value)
  (%gl:uniform-matrix-4fv location count nil value))

;; [TODO] HANDLE DOUBLES
(defun get-foreign-uniform-function (type)
  (symbol-function (get-foreign-uniform-function-name type)))

(defun get-uniform-function (type)
  (symbol-function (get-uniform-function-name type)))

(defun get-foreign-uniform-function-name (type)
  (case type
    ((:int :int-arb :bool :bool-arb) '%gl:uniform-1iv)
    ((:float :float-arb) '%gl:uniform-1fv)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) '%gl:uniform-2iv)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) '%gl:uniform-3iv)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) '%gl:uniform-4iv)
    ((:vec2 :float-vec2 :float-vec2-arb) '%gl:uniform-2fv)
    ((:vec3 :float-vec3 :float-vec3-arb) '%gl:uniform-3fv)
    ((:vec4 :float-vec4 :float-vec4-arb) '%gl:uniform-4fv)
    ((:mat2 :float-mat2 :float-mat2-arb) 'uniform-matrix-2fvt)
    ((:mat3 :float-mat3 :float-mat3-arb) 'uniform-matrix-3fvt)
    ((:mat4 :float-mat4 :float-mat4-arb) 'uniform-matrix-4fvt)
    (t (if (sampler-typep type) nil
           (error "Sorry cepl doesnt handle that type yet")))))

(defun get-uniform-function-name (type)
  (case type
    ((:int :int-arb :bool :bool-arb) 'uniform-1i)
    ((:float :float-arb) 'uniform-1f)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) 'uniform-2i)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) 'uniform-3i)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) 'uniform-4i)
    ((:vec2 :float-vec2 :float-vec2-arb) 'uniform-2f)
    ((:vec3 :float-vec3 :float-vec3-arb) 'uniform-3f)
    ((:vec4 :float-vec4 :float-vec4-arb) 'uniform-4f)
    ((:mat2 :float-mat2 :float-mat2-arb) 'uniform-matrix-2ft)
    ((:mat3 :float-mat3 :float-mat3-arb) 'uniform-matrix-3ft)
    ((:mat4 :float-mat4 :float-mat4-arb) 'uniform-matrix-4ft)    
    (t (if (sampler-typep type) 'uniform-sampler
           (error "Sorry cepl doesnt handle that type yet")))))

;;;--------------------------------------------------------------
;;; SHADER & PROGRAMS ;;;
;;;-------------------;;;

(let ((programs (make-hash-table)))
  (defun program-manager (name)
    (let ((prog-id (gethash name programs)))
      (if prog-id prog-id
          (setf (gethash name programs) (gl:create-program)))))
  ;;[TODO] Implement delete
  (defun program-manager-delete (name)
    (declare (ignore name))
    (print "delete not yet implemented")))

(defun valid-shader-typep (shader)
  (find (first shader) '(:vertex :fragment :geometry)))

(defmacro defsmacro (name lambda-list &body body)
  `(varjo::vdefmacro ,name ,lambda-list ,@body))

(defmacro defsfun (name args &body body)
  (let ((l-args (shader-args-to-list-args args)))
    `(progn
       (when (and (fboundp ',name) (gethash #',name *cached-glsl-source-code*))
         (remhash (symbol-function ',name) *cached-glsl-source-code*))
       (defun ,name ,l-args
         (declare (ignore ,@(loop :for i :in args :for l :in l-args
                               :if (listp args) :collect l)))
         (error "This is an sfun and can only be called from inside a pipeline..for now"))
       (setf (gethash #',name *cached-glsl-source-code*) '(:sfun ,name ,args ,body))
       (when *gl-context* (compile-the-chain ',name))
       ',name)))

(defmethod gl-pull ((object function))
  (let* ((code (gethash object *cached-glsl-source-code*))
         (s-type (first code)))
    (if code
        (case s-type
          (:pipeline (format nil "~{~a~^~%-----------~%~}"
                             (loop :for (s-type src) :in (rest code)
                                :collect (format nil "#~a~%~a" s-type src))))
          (:shader (let ((code-chunk (third code)))
                     (format nil "~&#~a~%~a" (first code-chunk) (second code-chunk))))
          (:sfun `(defsfun ,(second code) ,(third code)
                    ,@(fourth code))))
        (format t "Could not find any source code for ~a" object))))

(defun shader-args-to-list-args (args)
  (loop :for a :in args :collect
     (if (listp a)
         (first a)
         (if (and (symbolp a) (equal (symbol-name a) "&UNIFORM"))
             '&key
             (error "Invalid atom ~s in shader args" a)))))

(defun subst-sfuns (code)
  (let ((seen nil)
        (sfuncs nil)               
        (sfuns (mapcar #'rest (utils:hash-values *cached-glsl-source-code*))))
    (labels ((wsf (code)
               (when (consp code)
                 (let* ((func (first code))
                        (fdef (and (not (find func seen)) (assoc func sfuns))))
                   (when fdef (push func seen) (push fdef sfuncs))
                   (or (every 'identity (loop :for i :in code :collect (wsf i)))
                       fdef)))))
      (wsf code)
      (loop :until (not (wsf sfuncs)))
      (values (if sfuncs
                  `((labels (,@(loop :for (name args body) :in sfuncs
                                  :collect `(,name ,args ,@body)))
                      ,@code))
                  code)
              seen))))

(defun %defshader (name type s-args body)
  (let* ((args (shader-args-to-list-args s-args))
         (s-args-with-type (if (find '&context s-args :test #'utils:symbol-name-equal)
                               (append s-args `(:type ,type))
                               (append s-args `(&context :type ,type))))
         (invalidate-func-name (symbolicate-package :cgl '££- name)))
    `(progn
       (defun ,name ,args
         (declare (ignore ,@(remove '&key args)))
         (error "This is a shader stage and can only be called from inside a pipeline..for now"))
       ;; [TODO] how do we handle first shader?
       (defun ,invalidate-func-name ()
         (multiple-value-bind (new-code sfuns-found) (subst-sfuns ',body)
           (setf (gethash #',name *cached-glsl-source-code*)
                 (append (list :shader ',s-args)
                         (varjo:translate ',s-args-with-type
                                          new-code nil)))
           (loop :for c :in sfuns-found :do 
              (add-compile-chain c ',invalidate-func-name)))
         (when *gl-context* (compile-the-chain ',name)))
       (,invalidate-func-name)            
       ',name)))

(defmacro defvshader (name args &body body)
  (%defshader name :vertex args body))
(defmacro deffshader (name args &body body)
  (%defshader name :fragment args body))
(defmacro defgshader (name args &body body)
  (%defshader name :geometry args body))

(defmacro defshader (name shader-type args &body body)
  (case shader-type
    (:vertex (%defshader name :vertex args body))
    (:fragment (%defshader name :fragment args body))
    (:geometry (%defshader name :geometry args body))))

;; [TODO] If a shader-func or sfun is redefined as macro or regular func the 
;;        source code still remains in the cache
;; [TODO] if this called before deftype has made it's varjo stuff not cool
;; [TODO] If we load shaders from files the names will clash
(defun shader-args-compatible (pipe-args shader-args)
  (destructuring-bind (p-shader-type p-version p-in-args p-in-arg-decs
                                     p-uniform-args p-struct-functions
                                     p-struct-defs p-types)
      (varjo::parse-shader-args pipe-args)
    (declare (ignore p-shader-type p-version p-in-arg-decs p-struct-functions
                     p-struct-defs p-types))
    (destructuring-bind (s-shader-type s-version s-in-args s-in-arg-decs
                                       s-uniform-args s-struct-functions
                                       s-struct-defs s-types)
        (varjo::parse-shader-args shader-args)
      (declare (ignore s-shader-type s-version s-in-arg-decs s-struct-functions
                       s-struct-defs s-types))
      (and (loop :for s-in-arg :in s-in-args :always 
              (find s-in-arg p-in-args :test #'equal))
           (loop :for s-uniform-arg :in s-uniform-args :always 
              (find s-uniform-arg p-uniform-args :test #'equal))))))

(defun rolling-translate (in-args to-compile &optional accum (first-shader t))  
  (if to-compile
      (let ((shader (first to-compile)))
        (if (functionp shader)
            (destructuring-bind (s-in-args type-n-code out-args)
                (rest (gethash shader *cached-glsl-source-code*))
              (if (shader-args-compatible in-args s-in-args)
                  (rolling-translate out-args (rest to-compile) 
                                     (cons type-n-code accum) nil)
                  (error "Shader args not compatible with previous stage~%~s~%~s" 
                         in-args s-in-args)))
            (let ((type (first shader)))
              (destructuring-bind (type-n-code out-args)
                  (varjo:translate (if (find '&context in-args 
                                             :test #'utils:symbol-name-equal)
                                       (append in-args `(:type ,type))
                                       (append in-args `(&context :type ,type)))
                                   (rest shader) first-shader)
                (rolling-translate out-args (rest to-compile)
                                   (cons type-n-code accum) nil)))))
      (progn (reverse accum))))

(defun add-compile-chain (child parent)
  (let ((current-chain (gethash child *compile-chain*)))
    (when (not (find parent current-chain))
      (setf (gethash child *compile-chain*) (cons parent current-chain)))))

(defun compile-the-chain (child)
  (let ((chain (gethash child *compile-chain*)))
    (loop :for p :in chain :if (fboundp p) :do 
       (funcall (symbol-function p)))))

;; [TODO] &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& SUBSTFUN HERE &&&&&&&!!!1lol
;; [TODO] Add textures back properly
(defmacro defpipeline (name (&rest args) &body shaders)
  (let* ((uniforms (varjo:extract-uniforms args))
         (uniform-names (mapcar #'first uniforms))
         (uniform-details (loop :for u :in uniforms :collect 
                             (make-arg-assigners u)))
         (u-lets (loop :for u :in uniform-details :append (first u)))
         (u-uploads (loop :for u :in uniform-details :collect (second u)))
         (init-func-name (symbolicate-package :cgl '%%- name))
         (invalidate-func-name (symbolicate-package :cgl '££- name))
         (shaders-no-post nil)
         (subscribe nil)
         (post-compile nil))
    (loop :for shader :in shaders :do
       (cond ((symbolp shader) (error "Cannot compose symbol '~s' into pipeline stage" shader))
             ((eq (first shader) :post-compile) (push (rest shader) post-compile))
             ((and (eq (first shader) 'cl:quote) (symbolp (second shader))) 
              (push (cons 'cl:function (rest shader)) shaders-no-post)
              (push (second shader) subscribe))
             ((eq (first shader) 'cl:function) (push shader shaders-no-post) 
              (push (second shader) subscribe))
             ((valid-shader-typep shader)
              (push `(multiple-value-bind (new-code sfuns-found) 
                         (subst-sfuns '(,@(rest shader)))
                       (loop :for c :in sfuns-found :do 
                          (add-compile-chain c ',invalidate-func-name))
                       (cons ,(first shader) new-code))
                    shaders-no-post))
             (t (error "unknown pipeline element: ~s" shader))))
    `(let ((program-id nil)
           ,@(loop for u in u-lets collect `(,(first u) -1)))
       (defun ,invalidate-func-name () (setf program-id nil))
       ;; func that will create all resources for pipeline
       (defun ,init-func-name ()
         (let* ((glsl-src (cgl::rolling-translate 
                           ',args (list ,@(reverse shaders-no-post))))
                (shaders-objects (loop :for (type code) :in glsl-src
                                    :collect (make-shader type code)))
                (prog-id (link-shaders shaders-objects
                                       ,(if name `(program-manager ',name)
                                            `(gl:create-program))))
                (image-unit -1))
           (declare (ignorable image-unit))
           (loop :for stage :in ',subscribe :do 
              (add-compile-chain stage ',invalidate-func-name))
           (mapcar #'%gl:delete-shader shaders-objects)
           ,@(loop for u in u-lets collect (cons 'setf u))
           (setf (gethash #',name *cached-glsl-source-code*) 
                 (cons :pipeline glsl-src))
           (unbind-buffer)
           (force-bind-vao 0)
           (force-use-program 0)
           (setf program-id prog-id)
           ,@(loop for p in post-compile append p)
           prog-id))
       ;; if we are creating once context exists then just run the init func,
       ;; otherwise bind the init func to the creation of the context
       ;; which doesnt work yet
       ;; (dvals:brittle-bind program-id *gl-context* (,init-func-name))
       ;; And finally the pipeline function itself
       (defun ,name (stream ,@(when uniforms `(&key ,@uniform-names)))
         (declare (ignorable ,@uniform-names))
         (unless program-id (setf program-id (,init-func-name)))
         (use-program program-id)
         ,@u-uploads
         (when stream (no-bind-draw-one stream))
         (use-program 0)
         stream))))

;;---------------------------------------------------------------------

(defun sampler-typep (type)
  (let ((type (varjo:flesh-out-type type)))
    (find (first type) *sampler-types*)))

(defun make-arg-assigners (uniform-arg &aux gen-ids assigners)
  (destructuring-bind (arg-name varjo-type glsl-name &rest ignore-args) 
      (varjo::flesh-out-arg uniform-arg)
    (declare (ignore ignore-args))
    (let ((struct-arg (varjo:type-struct-p varjo-type))
          (array-length (second varjo-type))
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
      `(,(reverse gen-ids)
         (when ,arg-name
           (let ((val ,(if (or array-length struct-arg) 
                           `(pointer ,arg-name)
                           arg-name)))
             ,@(reverse assigners)))))))

(defun make-sampler-assigner (type path &optional (byte-offset 0))
  (declare (ignore byte-offset))
  (destructuring-bind (principle-type &rest ignore-args) 
      (varjo:flesh-out-type type)
    (declare (ignore ignore-args))
    (let ((id-name (gensym))
          (i-unit (gensym "IMAGE-UNIT")))
      `(((,id-name (gl:get-uniform-location prog-id ,path))
         (,i-unit (incf image-unit)))
        (when (>= ,id-name 0)
          (unless (eq (sampler-type val) ,principle-type) 
            (error "incorrect texture type passed to shader"))
          ;; (unless ,id-name 
          ;;   (error "Texture uniforms must be populated")) ;; [TODO] this wont work here
          (active-texture-num ,i-unit)
          (bind-texture val)
          (uniform-sampler ,id-name ,i-unit))
        t))))

(defun make-simple-assigner (type path &optional (byte-offset 0))
  (destructuring-bind (principle-type &rest ignore-args) 
      (varjo:flesh-out-type type)
    (declare (ignore ignore-args))
    (let ((id-name (gensym)))
      `((,id-name (gl:get-uniform-location prog-id ,path))
        (when (>= ,id-name 0)
          ,(if byte-offset
               `(,(get-foreign-uniform-function-name principle-type) 
                  ,id-name 1 (cffi:inc-pointer val ,byte-offset))
               `(,(get-uniform-function-name principle-type) ,id-name val)))
        nil))))

(defun make-array-assigners (type path &optional (byte-offset 0))
  (destructuring-bind 
        (principle-type array-length &rest rest) (varjo:flesh-out-type type)
    (declare (ignore rest))
    (loop :for i :below array-length :append
       (cond ((varjo:type-struct-p principle-type) 
              (make-struct-assigners principle-type byte-offset))
             (t (list (make-simple-assigner principle-type 
                                            (format nil "~a[~a]" path i)
                                            byte-offset))))
       :do (incf byte-offset (cffi:foreign-type-size principle-type)))))

(defun make-struct-assigners (type path &optional (byte-offset 0))
  (destructuring-bind (principle-type &rest rest) (varjo:flesh-out-type type)
    (declare (ignore rest))
    (loop :for (l-slot-name v-slot-type glsl-name) 
       :in (varjo:struct-definition principle-type) :append
       (destructuring-bind (pslot-type array-length . rest) v-slot-type
         (declare (ignore rest)) 
         (let ((path (format nil "~a.~a" path glsl-name)))
           (prog1
               (cond (array-length (make-array-assigners v-slot-type path
                                                         byte-offset))
                     ((varjo:type-struct-p pslot-type) (make-struct-assigners
                                                        pslot-type path
                                                        byte-offset))
                     (t (list (make-simple-assigner pslot-type path
                                                    byte-offset))))
             (incf byte-offset (* (cffi:foreign-type-size pslot-type)
                                  (or array-length 1)))))))))

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

;; [TODO] Need to sort gpustream indicies thing
(defun no-bind-draw-one (stream)
  "This draws the single stream provided using the currently 
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  (let ((index-type (vertex-stream-index-type stream)))
    (bind-vao (vertex-stream-vao stream))
    (if index-type
        (%gl:draw-elements (vertex-stream-draw-type stream)
                           (vertex-stream-length stream)
                           (gl::cffi-type-to-gl index-type)
                           (make-pointer 0))
        (%gl:draw-arrays (vertex-stream-draw-type stream)
                         (vertex-stream-start stream)
                         (vertex-stream-length stream)))))
