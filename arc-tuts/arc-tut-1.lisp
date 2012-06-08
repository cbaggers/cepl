;;; working through the arcsynthesis tutorials to make sure
;;; our libraries work well enough.
;;; Many comments may be taken from the tutorials to spare 
;;; my terrible memory
(in-package :cepl)

;;;--------------------------------------------------------------

(defun file-to-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated 
   string, returning two values: the string and the number of 
   bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

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

(defun make-program (shaders)
  (let ((program (gl:create-program)))
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
       do (gl:detach-shader program shader))
    program))

(defun shader-type-from-path (path)
  "This uses the extension to return the type of the shader"
  (let* ((plen (length path))
	 (exten (subseq path (- plen 5) plen)))
    (cond ((equal exten ".vert") :vertex-shader)
	  ((equal exten ".frag") :fragment-shader)
	  (t (error "Could not extract shader type from shader"))
	  )))

(defun initialize-program ()
  (let* ((shader-paths `("./tut1.vert" "./tut1.frag"))
	 (shaders (mapcar (lambda (path) 
			    (make-shader 
			     path
			     (shader-type-from-path path))) 
			  shader-paths))
	 (program (make-program shaders)))
    (loop for shader in shaders
	 do (gl:delete-shader shader))
    program))

;;;--------------------------------------------------------------

;; A vertex is a collection of arbitrary data. For the sake of
;; simplicity (we will expand upon this later), let us say
;; that this data must contain a point in three dimensional 
;; space.
(defstruct vertex
  position)

;; note that these co-ords are already in clip space
(defparameter *vertex-positions* #(-0.75  0.75  0.0  1.0
  				     0.0   0.0  0.0  1.0
				   -0.75 -0.75  0.0  1.0))

(defparameter *program-id* nil)
(defparameter *position-buffer-object* nil)

;;;--------------------------------------------------------------

(defun setup-buffer (buf-type data-type data)
  (let* ((data-length (length data))
	 (arr (gl:alloc-gl-array data-type data-length))
	 (buffer (car (gl:gen-buffers 1))))
    ;; need to pass data as a c-array
    (dotimes (i data-length)
      (setf (gl:glaref arr i) (aref data i)))
    (gl:bind-buffer buf-type buffer)
    (gl:buffer-data buf-type :static-draw arr)
    (gl:free-gl-array arr)
    (gl:bind-buffer buf-type 0)
    buffer))

;;;--------------------------------------------------------------

(defun init ()
  (setf *program-id* (initialize-program))
  (setf *position-buffer-object* (initialize-vertex-buffer)))

(defun initialize-vertex-buffer ()
  ;; * creates a buffer
  ;; * binds the buffer to array-buffer binding target, this 
  ;;   gives the buffer context in opengl
  ;; * gl:buffer-data does a few things
  ;;   - state that we are pushing to the currently bound 
  ;;     array-buffer
  ;;   - allocate and push the data from *vertex-positions*
  ;;   - set as :static-draw.... this we come to in future 
  ;;     tutorials
  ;; * cleanup the data we have used
  ;; so we have populated (as far as opengl is concerned) a 
  ;; load of random binary data, we tell opengl how to use it
  ;; in the rendering code
  (setup-buffer :array-buffer
		:float 
		*vertex-positions*))

;;;--------------------------------------------------------------

(cffi:defcallback display :void ()
  ;; this is a state setting function, its sets the color used
  ;; when clearing the screen
  (gl:clear-color 0.0 0.0 0.0 0.0)
  ;; here we are using clearing the color buffer
  (gl:clear :color-buffer-bit)
  ;; sets the shader program to be used in all the following 
  ;; commands we will need to wrap this up in a 'with-program' 
  ;; macro.
  (gl:use-program *program-id*)
  ;; bind position-buffer-object to the array-buffer
  ;; tell opengl where the co-ords of the triangle we want to 
  ;; draw are
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  ;; this enables the attribute index we will use for the 
  ;; position attribute.
  (gl:enable-vertex-attrib-array 0)
  ;; the following is not abount pointers, its abount gl-objects
  ;; it's laid out as follows 
  ;; * attribute index
  ;; * num of vals per piece of data
  ;; * val type
  ;; * ?
  ;; * spacing beween each set of values
  ;; * specifies the offset to the first piece of data
  ;; remeber this is acting on the currently bound buffer
  (gl:vertex-attrib-pointer 0 4 :float :false 
			    0 (cffi:null-pointer))
  ;; The glDrawArrays function is, as the name suggests, a 
  ;; rendering function. It uses the current state to generate 
  ;; a stream of vertices that will form triangles.
  ;; the 0 specifies the first index (refering to data specified
  ;; in vertex-attrib-pointer) and the number of vertices to
  ;; read (3).
  (gl:draw-arrays :triangles 0 3)
  ;; unbind things
  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)
  ;; swap the buffer to make what we have drawn visible
  (glut:swap-buffers))

(cffi:defcallback reshape :void ((width :int) (height :int))
  (let ((dim (min width height)))
    (gl:viewport 0 0 width height)))

;; The only reason the print statements are there is so I dont 
;; get style warnings
(cffi:defcallback keyboard :void ((key :char) (x :int) (y :int))
  (declare (ignore x y))
  (if (eql key 27)
      (glut:leave-main-loop)))

;;;--------------------------------------------------------------

(defun run-demo ()
  (let ((width 500)
	(height 500))
    (glut:init)
    (glut:init-display-mode :double :alpha :depth :stencil)
    (glut:init-window-size width height)
    (glut:create-window "ArcSynthesis Tutorial 1")
    (init) ; to initialise the data
    (glut:display-func (cffi:callback display))
    (glut:reshape-func (cffi:callback reshape))
    (glut:keyboard-func (cffi:callback keyboard))
    (glut:main-loop)))


;; could be useful later, who knows.
;; (if debug-arb
;; 	(progn (gl:enable :debug-output-synchronous-arb)
;; 	       (gl:debug-message-callback-arb (cffi:callback debug) 15)))



