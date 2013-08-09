(in-package :cgl)

;;;--------------------------------------------------------------
;;; UNIFORMS ;;;
;;;----------;;;

(defun uniform-1i (location value)
  (gl:uniformi location value))

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
    ((:int :int-arb :bool :bool-arb :sampler_1d :sampler_1d_shadow 
           :sampler_2d :sampler_3d :sampler_cube 
           :sampler_2d_shadow) #'%gl:uniform-1iv)
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
    (t (error "Sorry cepl doesnt handle that type yet"))))

(defun get-uniform-function (type)
  (case type
    ((:int :int-arb :bool :bool-arb :sampler_1d :sampler_1d_shadow 
           :sampler_2d :sampler_3d :sampler_cube 
           :sampler_2d_shadow) #'uniform-1i)
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
    (t (error "Sorry cepl doesnt handle that type yet"))))

;;;--------------------------------------------------------------
;;; SHADER & PROGRAMS ;;;
;;;-------------------;;;

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

;; [TODO] We need to make this fast, this 'if not prog' won't do
(defmacro defpipeline (name (&rest args) &body shaders)
  (if (> (count :post-compile shaders :key #'first) 1)
      (error "Cannot not have more than one :post-compile section")
      (let ((post (rest (find :post-compile shaders :key #'first)))
            (shaders (remove :post-compile shaders :key #'first)))
        (if (every #'valid-shader-typep shaders)
            (let* ((uniform-names (mapcar #'first (varjo:extract-uniforms args))))
              `(let ((program nil))
                 (defun ,name (stream ,@(when uniform-names `(&key ,@uniform-names)))
                   (when (not program) 
                     (setf program (make-program ,name ,args ,shaders))
                     ,@post)
                   (funcall program stream ,@(loop for name in uniform-names 
                                                :append `(,(utils:kwd name)
                                                           ,name))))))
            (error "Some shaders have invalid types ~a" (mapcar #'first shaders))))))

(defmacro defpipeline? (name (&rest args) &body shaders)
  (declare (ignore name))
  (let ((shaders (remove :post-compile shaders :key #'first)))
    (if (every #'valid-shader-typep shaders)
        `(let* ((shaders (varjo:rolling-translate ',args ',shaders)))
           (format t "~&~{~{~(#~a~)~%~a~}~^-----------~^~%~^~%~}~&" shaders)
           nil)
        (error "Some shaders have invalid types ~a" (mapcar #'first shaders)))))

(defmacro glambda ((&rest args) &body shaders)
  `(make-program nil ,args ,shaders))

;; [TODO] Make glambda handle strings
(defmacro make-program (name args shaders)  

  (let* ((uniforms (varjo:extract-uniforms args))
         (uniform-names (mapcar #'first uniforms)))
    
    `(let* ((shaders (loop for (type code) in (varjo:rolling-translate 
                                               ',args ',shaders)
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
              :collect `(when ,uniform-name
                          (dolist (fun ,(utils:symb uniform-name
                                                    '-assigner))
                            (funcall fun ,uniform-name))))
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

(defun get-path-offset (path uniform-vars)
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
          (+ (* (cffi:foreign-type-size type) index)
             (path-offset type (rest path)))
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
    (loop for shader in shaders
       do (gl:detach-shader program shader))
    program))

;; [TODO] Need to sort gpustream indicies thing
(defun no-bind-draw-one (stream)
  "This draws the single stream provided using the currently 
   bound program. Please note: It Does Not bind the program so
   this function should only be used from another function which
   is handling the binding."
  (let ((index-type (gpu-stream-index-type stream)))
    (bind-vao (gpu-stream-vao stream))
    (if index-type
        (%gl:draw-elements (gpu-stream-draw-type stream)
                           (gpu-stream-length stream)
                           (gl::cffi-type-to-gl index-type)
                           (make-pointer 0))
        (%gl:draw-arrays (gpu-stream-draw-type stream)
                         (gpu-stream-start stream)
                         (gpu-stream-length stream)))))
