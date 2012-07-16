;; This is to shadow and tidy up the cl-opengl feature set.
;; We will define our own abstractions here also so if the user
;; wants to interact with opengl they dont have to think 'Is it
;; in cl-opengl or cepl-gl?' it will just be cepl-gl.
;; Hopefully this will help with cognitive flow.

(in-package :cepl-gl)

(define-condition cgl-compile-shader-error (error)
  ((text :initarg :text :reader text)))

;;;--------------------------------------------------------------
;;; BUFFERS ;;;
;;;---------;;;

;; This pattern could be abstracted into a macro
;; That would be pretty badass

;; These lexicaly bound functions provide memoization around
;; the binding of buffers. 
;; The effect is that it only calls the gl:bind-buffer if 
;; that buffer isn't already bound to the current target
;; it also allows nesting.
(let ((current-buffer nil)
      (current-target nil)
      (cached-buffers nil)
      (cached-targets nil))
  (defun bind-buffer (target buffer-id)
    (unless (and (eq current-buffer buffer-id)
		 (eq current-target target))
      (gl:bind-buffer target buffer-id))
    (cons current-buffer cached-buffers)
    (cons current-target cached-targets)
    (setf buffer-id current-buffer)
    (setf target current-target))

  (defun unbind-buffer ()
    (let ((cached-buffer (car cached-buffers))
	  (cached-target (car cached-targets)))
      (unless (and (eq cached-buffer current-buffer)
		   (eq cached-target current-target))
	(gl:bind-buffer target buffer-id))
      (setf current-buffer cached-buffer)
      (setf current-target cached-target)
      (setf cached-buffers (cdr cached-buffers))
      (setf cached-targets (cdr cached-targets)))))


(defmacro with-bind-buffer (target buffer-id &body body)
  `(unwind-protect
        (prog2 (bind-buffer ,target ,buffer-id)
            (progn ,@body))
     (unbind-buffer)))

(defun gen-buffer ()
  (first (gen-buffers 1)))

(defun populate-buffer (buffer-id gl-array &key (buffer-type :array-buffer) (draw-type :static-draw))
  (with-bind-buffer buffer-type buffer-id
    (gl:buffer-data buffer-type draw-type gl-array))
  buffer-id)

;;;--------------------------------------------------------------
;;; GL-ARRAYS ;;;
;;;-----------;;;

;; (setf (gl:glaref array index 'x) 1.675)

(defmacro define-gl-array-format (name &body clauses)
  "Defines a vertex array format spcification.
   We have taken out the array binding stuff, I think
   it has been deprecated. I could always add it back 
   later but I prefer this for now."
  `(progn
     (defcstruct ,name
       ,@(mapcan #'emit-gl-array-struct-clause clauses))
     ',name))

(defun emit-gl-array-struct-clause (clause)
  (destructuring-bind (&key type components &allow-other-keys)
      clause
    (loop for c in components
          collect `(,c ,type))))

(defun alloc-array-gl (format count)
  (alloc-gl-array (data-format-c-format format) count))

;; Grr, wanted to use this but coudl work out how to do the 
;; setf version
;;(setf (symbol-function 'aref-gl) (symbol-function 'glaref))
(declaim (inline aref-gl))
(defun aref-gl (array index &optional (component nil c-p))
  "Returns the INDEX-th component of ARRAY. If COMPONENT is
supplied and ARRAY is of a compound type the component named
COMPONENT is returned."
  (if c-p
      (foreign-slot-value (mem-aref (gl::gl-array-pointer array
				     )
                                    (gl::gl-array-type array)
                                    index)
                          (gl::gl-array-type array)
                          component)
      (mem-aref (gl::gl-array-pointer array) (gl::gl-array-type array) index)))


;;[TODO] Check length of data and warn if too short
;;[TODO] Start offset could break if (> (+ offset length) 
;;                                      array-length)
;;[TODO] Specify component names?
(defun populate-gl-array (array format data &optional (offset 0))
  (loop for entry in data
     for index = offset then (1+ index)
     do (loop for attr-data in entry
	   for components in (data-format-data-layout format)
	   do (loop for component in (second components)
		   for datum in attr-data
		   do (setf (aref-gl array index component) 
			    datum)))))


(declaim (inline (setf aref-gl)))
(defun (setf aref-gl) (value array index &optional (component nil c-p))
  "Sets the place (GLAREF ARRAY INDEX [COMPONENT]) to VALUE."
  (if c-p
      (setf (foreign-slot-value (mem-aref (gl::gl-array-pointer array)
                                          (gl::gl-array-type array)
                                          index)
                                (gl::gl-array-type array)
                                component)
            value)
      (setf (mem-aref (gl::gl-array-pointer array) (gl::gl-array-type array) index)
            value)))

;;;--------------------------------------------------------------
;;; DATA-FORMAT ;;;
;;;-------------;;;

(defstruct data-format 
  (data-layout)
  (c-format))

(defmacro def-data-format (&body clauses)
  "This extends the gl-arrays of cl-opengl so we can use 
   it to generate VAOs 
   This is in a basic form at the mo, this will eventually
   lay out the data in a way that will be faster for the gpu.
   For now this is more of a 'shell' we will fill in later."
  `(make-data-format 
    :data-layout ',(mapcar 
		    #'(lambda (clause) 
			(destructuring-bind 
			      (&key type components 
				    &allow-other-keys)
			    clause
			  (list type components))) clauses)
    :c-format (define-gl-array-format ,(gensym) ,@clauses)))

;;;--------------------------------------------------------------
;;; VAO ;;;
;;;-----;;;

(defmacro with-bind-vao (vao-id &body body)
  `(unwind-protect
        (prog2 (bind-vertex-array ,vao-id)
            (progn ,@body))
     (bind-vertex-array 0)))

;;;--------------------------------------------------------------
;;; STREAMS ;;;
;;;---------;;;

;;[TODO] Needs to handle normalized
;;[TODO] Needs to handle non-interleaved data
(defun make-stream (buffer-id format 
		    &optional (buffer-type :array-buffer)
		      (element-buffer nil)
		      (buffer-offset 0))
  (labels ((clause-type (clause) 
	     (second (member :type clause)))
	   (clause-length (clause)
	     (length (second (member :components clause))))
	   (clause-size (clause)
	     (* (foreign-type-size (clause-type clause))
		(clause-length clause))))
    (let* ((vao-id (gen-vertex-array))
	   (clauses (data-format-data-layout format))
	   (stride (apply #'+ (mapcar #'clause-size clauses))))
      (with-bind-vao vao-id
	(with-bind-buffer buffer-type buffer-id
	  (loop for clause in clauses
	     for index from 0 to (1- (length clauses))
	     do (progn
		  (enable-vertex-attrib-array index)
		  (vertex-attrib-pointer 
		   index
		   (clause-length clause)
		   (clause-type clause)
		   :false
		   stride
		   (cffi:make-pointer buffer-offset)))))
	(when element-buffer
	  (bind-buffer :element-array-buffer element-buffer)))
      vao-id)))


;;;--------------------------------------------------------------
;;; RENDERABLES ;;;
;;;-------------;;;

(defstruct renderable 
  (stream)
  (start)
  (end))


;;;--------------------------------------------------------------
;;; PROGRAMS ;;;
;;;----------;;;

(defun program-attrib-count (program)
  (get-program program :active-attributes))

(defun program-attributes (program)
  (loop for i from 0 below (program-attrib-count program)
     collect (multiple-value-bind (size type name)
		 (get-active-attrib program i)
	       (list name type size))))

(defun program-uniform-count (program)
  (get-program program :active-uniforms))

(defun program-uniforms (program)
  (loop for i from 0 below (program-uniform-count program)
     collect (multiple-value-bind (size type name)
		 (get-active-uniform program i)
	       (list name type size))))

(defmacro with-use-program (program-id &body body)
  `(unwind-protect
        (prog2 (use-program ,program-id)
            (progn ,@body))
     (use-program 0)))

;; This is one DUMB function...but its serving my needs for now
(defun shader-type-from-path (path)
  "This uses the extension to return the type of the shader"
  (let* ((plen (length path))
	 (exten (subseq path (- plen 5) plen)))
    (cond ((equal exten ".vert") :vertex-shader)
	  ((equal exten ".frag") :fragment-shader)
	  (t (error "Could not extract shader type from shader"))
	  )))

(defun make-shader (file-path &optional 
				(shader-type 
			      (shader-type-from-path file-path))
				(shader-id 
				 (gl:create-shader shader-type)))
  (restart-case  
      (let ((source-string (utils:file-to-string file-path)))
	(gl:shader-source shader-id source-string)
	(gl:compile-shader shader-id)
	;;check for compile errors
	(if (not (gl:get-shader shader-id :compile-status))
	    (error `cgl-compile-shader-error
		   :text (format nil 
				 "Error compiling shader ~a~%~a" 
				 file-path
				 (gl:get-shader-info-log 
				  shader-id)))))
    (reload-recompile-shader () (make-shader file-path
					     shader-type
					     shader-id)))
  shader-id)

(defun make-program (shaders)
  (let ((program (gl:create-program)))
    (loop for shader in shaders
       do (gl:attach-shader program shader))
    (gl:link-program program)
    ;;check for linking errors
    (if (not (gl:get-program program :link-status))
	(error (format nil "Error Linking Program~%~a" 
		       (gl:get-program-info-log program))))
    (loop for shader in shaders
       do (gl:detach-shader program shader)
	  (gl:delete-shader shader))
    program))


;; Treat pipeline as struct with unfirms being setf'able values
;; belonging to the pipline. How do we handle bidning the program
;; (pipeline) when the setf occurs outside of a with pipeline? 
(defun make-pipeline (shaders)
  (let ((program (make-program shaders))
	(uniforms (make-hash-table)))
    (loop for unif-name in (program-uniforms program)
       do (setf (gethash (utils:make-keyword 
			  (string-upcase unif-name))
			 uniforms)
		(gl:get-uniform-location program unif-name)))))



(defun draw (uniforms renderables)
  )

;; (setf (model->cam-uniform win) 
;;       (gl:get-uniform-location (program win)
;; 			       "modelToCameraMatrix"))

;; (gl:uniform-matrix (cam->clip-uniform win)
;; 		   4 (vector (cam->clip win))))


;; This was no good as my thinking was that we called pipelines
;; but the problem with this is that the uniforms can potentially
;; change with each draw call...it will be crap if we have to 
;; bind the pipeline each time (with-use-program)...need a better
;; abstraction that this

;; (defmacro make-pipeline (uniform-names shaders)
;;   `(let ((program-id (make-program ,shaders)))
;;      (lambda (,@uniform-names renderables)
;;        (with-use-program program-id
;; 	 (mapc #' draw renderables)))))

;; CL-USER> (macroexpand `(make-pipeline (position color) ("shader.frag" "shader.vert")))

;; (LET ((PROGRAM-ID (MAKE-PROGRAM ("shader.frag" "shader.vert"))))
;;   (LAMBDA (POSITION COLOR RENDERABLES)
;;     (WITH-USE-PROGRAM PROGRAM-ID (MAPC #'DRAW RENDERABLES))))
