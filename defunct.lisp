(in-package :defunct)

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case 
       (progn ,@body)
     (continue () :report "Continue")))

;;;--------------------------------------------------------------

(defun draw-elements-base-vertex (mode array indices base-vertex
			    &key (count (gl::gl-array-size array)))
  (%gl:draw-elements-base-vertex mode count
                     (gl::cffi-type-to-gl (gl::gl-array-type array))
                     (gl::gl-array-pointer-offset array indices)
		     base-vertex))


(defun calculate-frustrum-scale (field-of-view-degrees)
  (/ 1.0 (tan (/ (* field-of-view-degrees base-maths:+one-degree-in-radians+) 2.0))))

(defmacro with-bind-buffer (target buffer-id &body body)
  `(unwind-protect
        (prog2 (gl:bind-buffer ,target ,buffer-id)
            (progn ,@body))
     (gl:bind-buffer ,target 0)))

(defmacro with-bind-vao (vao-id &body body)
  `(unwind-protect
        (prog2 (gl:bind-vertex-array ,vao-id)
            (progn ,@body))
     (gl:bind-vertex-array 0)))

(defmacro with-use-program (program-id &body body)
  `(unwind-protect
        (prog2 (gl:use-program ,program-id)
            (progn ,@body))
     (gl:use-program 0)))

;;;--------------------------------------------------------------

(defun setup-buffer (buf-type gl-array 
		     &optional (draw-type :static-draw))
  (let* ((buffer (car (gl:gen-buffers 1))))
    (with-bind-buffer buf-type buffer
      (gl:buffer-data buf-type draw-type gl-array))
    buffer))

;;;--------------------------------------------------------------

(defun file-to-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated 
   string, returning two values: the string and the number of 
   bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

;;;--------------------------------------------------------------

(defun make-shader (file-path shader-type)
  (let* ((source-string (file-to-string file-path))
	 (shader (gl:create-shader shader-type)))
    (gl:shader-source shader source-string)
    (gl:compile-shader shader)
    ;;check for compile errors
    (if (not (gl:get-shader shader :compile-status))
	(let ((error-string (write-to-string 
			     (gl:get-shader-info-log shader))))
	  (error (format nil "Error compiling shader ~a~%~a" 
			 file-path
			 error-string))))
    shader))

;;;--------------------------------------------------------------

(defun shader-type-from-path (path)
  "This uses the extension to return the type of the shader"
  (let* ((plen (length path))
	 (exten (subseq path (- plen 5) plen)))
    (cond ((equal exten ".vert") :vertex-shader)
	  ((equal exten ".frag") :fragment-shader)
	  (t (error "Could not extract shader type from shader"))
	  )))

;;;--------------------------------------------------------------

(defun make-program (shader-paths)
  (let* ((shaders (mapcar (lambda (path) 
			    (make-shader 
			     path
			     (shader-type-from-path path))) 
			  shader-paths))
	 (program (gl:create-program)))
    (loop for shader in shaders
       do (gl:attach-shader program shader))
    (gl:link-program program)
    ;;check for linking errors
    (if (not (gl:get-program program :link-status))
	(let ((error-string (write-to-string
			     (gl:get-program-info-log program))))
	  (error (format nil "Error Linking Program~%~a" 
			 error-string))))
    (loop for shader in shaders
       do (gl:detach-shader program shader)
	  (gl:delete-shader shader))
    program))


(defun make-gl-array-from-array (data-type data)
  (let* ((data-length (length data))
	 (arr (gl:alloc-gl-array data-type data-length)))
    (dotimes (i data-length)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defun sub-buffer (buffer-type buffer new-gl-array 
		   &optional (offset 0) 
		     (size (gl:gl-array-byte-size new-gl-array)))
  (with-bind-buffer buffer-type buffer
    (gl:buffer-sub-data buffer-type new-gl-array
			:offset offset :size size)))
