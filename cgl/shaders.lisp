(in-package :cgl)

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
  (case type
    ((:int :int-arb :bool :bool-arb) #'%gl:uniform-1iv)
    ((:float :float-arb) #'%gl:uniform-1fv)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) #'%gl:uniform-2iv)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) #'%gl:uniform-3iv)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) #'%gl:uniform-4iv)
    ((:float-vec2 :float-vec2-arb) #'%gl:uniform-2fv)
    ((:float-vec3 :float-vec3-arb) #'%gl:uniform-3fv)
    ((:float-vec4 :float-vec4-arb) #'%gl:uniform-4fv)
    ((:float-mat2 :float-mat2-arb) #'uniform-matrix-2fvt)
    ((:float-mat3 :float-mat3-arb) #'uniform-matrix-3fvt)
    ((:float-mat4 :float-mat4-arb) #'uniform-matrix-4fvt)
    (t (if (sampler-typep type) nil
           (error "Sorry cepl doesnt handle that type yet")))))

(defun get-uniform-function (type)
  (case type
    ((:int :int-arb :bool :bool-arb) #'uniform-1i)
    ((:float :float-arb) #'uniform-1f)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) #'uniform-2i)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) #'uniform-3i)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) #'uniform-4i)
    ((:float-vec2 :float-vec2-arb) #'uniform-2f)
    ((:float-vec3 :float-vec3-arb) #'uniform-3f)
    ((:float-vec4 :float-vec4-arb) #'uniform-4f)
    ((:float-mat2 :float-mat2-arb) #'uniform-matrix-2ft)
    ((:float-mat3 :float-mat3-arb) #'uniform-matrix-3ft)
    ((:float-mat4 :float-mat4-arb) #'uniform-matrix-4ft)    
    (t (if (sampler-typep type) #'uniform-sampler
           (error "Sorry cepl doesnt handle that type yet")))))

;;;--------------------------------------------------------------
;;; SHADER & PROGRAMS ;;;
;;;-------------------;;;

(defun sampler-typep (type)
  (find type *sampler-types*))

(let ((programs (make-hash-table)))
  (defun program-manager (name)
    (let ((prog-id (gethash name programs)))
      (if prog-id prog-id
          (setf (gethash name programs) (gl:create-program)))))
  (defun program-manager-delete (name)
    (declare (ignore name))
    (print "delete not yet implemented")))

(defun valid-shader-typep (shader)
  (find (first shader) '(:vertex :fragment :geometry)))

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

;; where do we check signature?
(defun %defshader (name type s-args body)
  (let ((args (loop :for a :in s-args :collect
                 (if (listp a) 
                     (first a)
                     (if (and (symbolp a) (equal (symbol-name a) "&UNIFORM"))
                         '&key
                         (error "Invalid atom ~s in shader args" a))))))
    `(defun ,name ,args
       (if (eq ,(first args) '--dump--)
           (list (cons ,type ',body) ',s-args)
           (error "This is a shader function and can only be called from inside a pipeline..for now")))))

(defun process-shaders (shaders args)
  (let ((post-compile nil) (result nil))
    (loop :for shader :in shaders :doing
       (if (listp shader)
           (if (eq (first shader) :post-compile)
               (set post-compile (cons shader post-compile))
               (if (valid-shader-typep shaders)
                   (set result `(quote ,(cons shader result)))
                   (error "Invalid shader type ~a" (first shader))))
           (setf result (cons `(handle-external-shader #',shader ',args) result))))
    (list (reverse result) (reverse post-compile))))

(defun handle-external-shader (sfun pipeline-args)
  (destructuring-bind (code shader-args) (funcall sfun '--dump--)
    (let ((s-uniforms (varjo:extract-uniforms shader-args))
          (p-uniforms (varjo:extract-uniforms pipeline-args)))
      
      (if (and (if (eq (first code) :vertex)
                   (equal (first shader-args) (first pipeline-args))
                   t)
               (loop :for s-uniform :in s-uniforms :always
                  (find s-uniform p-uniforms :test #'equal)))
          code
          (error "Shader arguments are not fully compatible with those of the pipeline:~%shader args: ~s~%pipeline args: ~s" shader-args pipeline-args)))))

;; [TODO] We need to make this fast, this 'if not prog' won't do
(defmacro defpipeline (name (&rest args) &body shaders)
  (destructuring-bind (shaders post) (process-shaders shaders args)
    (let* ((uniform-names (mapcar #'first (varjo:extract-uniforms args))))
      `(let ((program nil))
         (defun ,name (stream ,@(when uniform-names `(&key ,@uniform-names)))
           (when (not program)
             (setf program (make-program ,name ,args ,(cons 'list shaders)))
             ,@post)
           (funcall program stream ,@(loop for name in uniform-names 
                                        :append `(,(utils:kwd name)
                                                   ,name))))))))

(defmacro defpipeline? (name (&rest args) &body shaders)
  (declare (ignore name))
  (let ((shaders (remove :post-compile shaders :key #'first)))
    (if (every #'valid-shader-typep shaders)
        `(let* ((shaders (varjo:rolling-translate ',args ',shaders)))
           (format t "~&~{~{~(#~a~)~%~a~}~^-----------~^~%~^~%~}~&" shaders)
           nil)
        (error "Some shaders have invalid types ~a" (mapcar #'first shaders)))))

(defun extract-textures (uniforms)
  (loop for (name type) in uniforms 
       :if (sampler-typep type)
       :collect name))

(defmacro make-program (name args shaders)  
  (let* ((uniforms (varjo:extract-uniforms args))
         (textures (extract-textures uniforms))
         (uniform-names (mapcar #'first uniforms))
         (image-unit -1))
    `(let* ((shaders (loop for (type code) in (varjo:rolling-translate 
                                               ',args ,shaders)
                        :collect (make-shader type code)))
            (program-id (link-shaders 
                         shaders
                         ,(if name
                              `(program-manager ',name)
                              `(gl:create-program))))
            (assigners (create-uniform-assigners 
                        program-id ',uniforms 
                        ,(utils:kwd (package-name (symbol-package name)))))
            ,@(loop :for name :in uniform-names :for i :from 0
                 :collect `(,(utils:symb name '-assigner)
                             (nth ,i assigners))))
       (declare (ignorable assigners))
       (mapcar #'%gl:delete-shader shaders)
       (unbind-buffer)
       (force-bind-vao 0)
       (force-use-program 0)
       (lambda (stream ,@(when uniforms `(&key ,@uniform-names)))
         (use-program program-id)
         ,@(loop :for uniform-name :in uniform-names
              :for uspec :in uniforms
              :collect
              (if (find uniform-name textures)
                  (progn 
                    (incf image-unit)
                    `(if ,uniform-name
                         (if (eq (sampler-type ,uniform-name)
                                 ,(second uspec))
                             (progn
                               (active-texture-num ,image-unit)
                               (bind-texture ,uniform-name)
                               (dolist (fun ,(utils:symb uniform-name '-assigner))
                                 (funcall fun ,image-unit)))
                             (error "incorrect texture type passed to shader"))
                         (error "Texture uniforms must be populated"))) ;really? - for now yeah
                  `(when ,uniform-name
                     (dolist (fun ,(utils:symb uniform-name
                                               '-assigner))
                       (funcall fun ,uniform-name)))))
         (when stream (no-bind-draw-one stream))))))

;; make this return list of funcs or nil for each uni-var
(defun create-uniform-assigners (program-id uniform-vars package)
  (let* ((uniform-details (program-uniforms program-id))
         (active-uniform-details (process-uniform-details uniform-details
                                                          uniform-vars
                                                          package)))
    (loop for a-uniform in active-uniform-details
       :collect
         (when a-uniform
           (let ((location (gl:get-uniform-location program-id 
                                                    (second a-uniform))))
             (if (< location 0)
                 (error "uniform ~a not found, this is a bug in cepl"
                        (second a-uniform))
                 (loop for part in (subseq a-uniform 2)
                    :collect 
                      (destructuring-bind (offset type length) part
                        (let ((uni-fun (get-foreign-uniform-function type))
                              (uni-fun2 (get-uniform-function type)))
                          (if (or (> length 1) (varjo:type-struct-p type))
                              (lambda (pointer)
                                (funcall uni-fun location length
                                         (cffi-sys:inc-pointer pointer offset)))
                              (lambda (value) (funcall uni-fun2 location value))))))))))))

;; [TODO] Got to be a quicker and tidier way
(defun process-uniform-details (uniform-details uniform-vars package)
  ;; returns '(byte-offset principle-type length)
  (let ((result nil)
        (paths (loop for det in uniform-details
                  collect (parse-uniform-path det package))))
    (loop for detail in uniform-details
       for path in paths
       :do (setf result 
                 (acons (caar path) 
                        (cons (first detail)
                              (cons (list (get-path-offset path uniform-vars)
                                          (second detail)
                                          (third detail))
                                    (rest (rest 
                                           (assoc (caar path)
                                                  result)))))
                        result)))
    (loop for var in uniform-vars
       :collect (assoc (first var) result))))

;; [TODO] If we load shaders from files the names will clash
(defun parse-uniform-path (uniform-detail package)
  (labels ((s-dot (x) (split-sequence:split-sequence #\. x))
           (s-square (x) (split-sequence:split-sequence #\[ x)))
    (loop for path in (s-dot (first uniform-detail))
       :collect (let ((part (s-square (remove #\] path))))
                  (list (symbol-munger:camel-case->lisp-symbol (first part) 
                                                               package)
                        (if (second part)
                            (parse-integer (second part))
                            0))))))

(defun get-slot-type (parent-type slot-name)
  (second (assoc slot-name (varjo:struct-definition parent-type))))

;;path: ((TEX 0)) uniform-vars: ((JAM VEC4) (TEX SAMPLER-2D))
(defun get-path-offset (path uniform-vars)
  (format t "path: ~a uniform-vars: ~a" path uniform-vars)
  (labels ((path-offset (type path &optional (sum 0))
             (if path
                 (let* ((path-part (first path))
                        (slot-name (first path-part))
                        (child-type (varjo:type-principle
                                     (get-slot-type type
                                                    slot-name))))
                   (path-offset 
                    child-type
                    (rest path)
                    (+ sum
                       (+ (cffi:foreign-slot-offset type slot-name) 
                          (* (cffi:foreign-type-size child-type)
                             (second path-part))))))
                 sum)))
    (let* ((first-part (first path))
           (type (second (assoc (symbol-name (first first-part)) uniform-vars
                                :key #'symbol-name :test #'equal)))
           (index (second first-part)))
      (if type
          ;; this bit was taken from tw olevels up to kill the chance of 
          ;; cffi being asked the size of a sampler..it's an ugly hack though
          (if (or (> index 1) (varjo:type-struct-p type))
              (+ (* (cffi:foreign-type-size type) index) ;; [TODO] What if type non 
                 ;; cffi
                 (path-offset type (rest path)))
              0)
          (error "Could not find the uniform variable named '~a'" 
                 (first first-part))))))

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
    (loop for shader in shaders
       do (gl:attach-shader program shader))
    (gl:link-program program)
    ;;check for linking errors
    (if (not (gl:get-program program :link-status))
        (error (format nil "Error Linking Program~%~a" 
                       (gl:get-program-info-log program))))
    (loop :for shader :in shaders :do
       (gl:detach-shader program shader))
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
