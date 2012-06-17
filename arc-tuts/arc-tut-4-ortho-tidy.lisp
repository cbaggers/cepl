;; A moving triangle, makes use of restarable and slime 
;; communication while running.

;;; this version is going to be tidied using the lispy 
;;; abstractions provided by the cl-eopgl and glut libraries
(in-package :arc-tuts)

;;;--------------------------------------------------------------

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


;;;--------------------------------------------------------------

(defmacro with-bind-buffer (target buffer-id &body body)
  `(unwind-protect
        (prog2 (gl:bind-buffer ,target ,buffer-id)
            (progn ,@body))
     (gl:bind-buffer ,target 0)))

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


(defun initialize-program (shader-paths)
  (let* ((shaders (mapcar (lambda (path) 
			    (make-shader 
			     path
			     (shader-type-from-path path))) 
			  shader-paths))
	 (program (make-program shaders)))
    (loop for shader in shaders
	 do (gl:delete-shader shader))
    program))

 
(defun setup-buffer (buf-type data-type data)
  (let* ((data-length (length data))
	 (arr (gl:alloc-gl-array data-type data-length))
	 (buffer (car (gl:gen-buffers 1))))
    (dotimes (i data-length)
      (setf (gl:glaref arr i) (aref data i)))
    (with-bind-buffer buf-type buffer
      (gl:buffer-data buf-type :stream-draw arr))
    (gl:free-gl-array arr)
    buffer))

(defun sub-buffer (buffer-type data-type buffer new-data)
  (let* ((data-length (length new-data))
	 (arr (gl:alloc-gl-array data-type data-length)))
    (dotimes (i data-length)
      (setf (gl:glaref arr i) (aref new-data i)))
    (gl:bind-buffer buffer-type buffer)
    (gl:buffer-sub-data buffer-type arr)
    (gl:bind-buffer buffer-type 0)))

;;;--------------------------------------------------------------

(defclass arc-tut-window (glut:window)
  ((vbuff :accessor vertex-buffer)
   (va :accessor vertex-array)
   (program :accessor program)
   (offset :accessor offset-uniform)
   (v-array-ob :accessor vao))
  (:default-initargs :width 500 :height 500 :pos-x 100
		     :pos-y 100 
		     :mode `(:double :alpha :depth :stencil)
		     :title "ArcSynthesis Tut 4"))


(defmethod glut:display-window :before ((win arc-tut-window))
  (setf (program win) (initialize-program 
		       `("./tut4-ortho.vert" 
			 "./tut4-ortho.frag"))
	(vertex-array win) #(0.25   0.25  0.75  1.0 
			     0.25  -0.25  0.75  1.0 
			     -0.25   0.25  0.75  1.0 

			     0.25  -0.25  0.75  1.0 
			     -0.25  -0.25  0.75  1.0 
			     -0.25   0.25  0.75  1.0 

			     0.25   0.25  -0.75  1.0 
			     -0.25   0.25  -0.75  1.0 
			     0.25  -0.25  -0.75  1.0 

			     0.25  -0.25  -0.75  1.0 
			     -0.25   0.25  -0.75  1.0 
			     -0.25  -0.25  -0.75  1.0 

			     -0.25   0.25   0.75  1.0 
			     -0.25  -0.25   0.75  1.0 
			     -0.25  -0.25  -0.75  1.0 

			     -0.25   0.25   0.75  1.0 
			     -0.25  -0.25  -0.75  1.0 
			     -0.25   0.25  -0.75  1.0 

			     0.25   0.25   0.75  1.0 
			     0.25  -0.25  -0.75  1.0 
			     0.25  -0.25   0.75  1.0 

			     0.25   0.25   0.75  1.0 
			     0.25   0.25  -0.75  1.0 
			     0.25  -0.25  -0.75  1.0 

			     0.25   0.25  -0.75  1.0 
			     0.25   0.25   0.75  1.0 
			     -0.25   0.25   0.75  1.0 

			     0.25   0.25  -0.75  1.0 
			     -0.25   0.25   0.75  1.0 
			     -0.25   0.25  -0.75  1.0 

			     0.25  -0.25  -0.75  1.0 
			     -0.25  -0.25   0.75  1.0 
			     0.25  -0.25   0.75  1.0 

			     0.25  -0.25  -0.75  1.0 
			     -0.25  -0.25  -0.75  1.0 
			     -0.25  -0.25   0.75  1.0 

			     0.0  0.0  1.0  1.0 
			     0.0  0.0  1.0  1.0 
			     0.0  0.0  1.0  1.0 

			     0.0  0.0  1.0  1.0 
			     0.0  0.0  1.0  1.0 
			     0.0  0.0  1.0  1.0 

			     0.8  0.8  0.8  1.0 
			     0.8  0.8  0.8  1.0 
			     0.8  0.8  0.8  1.0 

			     0.8  0.8  0.8  1.0 
			     0.8  0.8  0.8  1.0 
			     0.8  0.8  0.8  1.0 

			     0.0  1.0  0.0  1.0 
			     0.0  1.0  0.0  1.0 
			     0.0  1.0  0.0  1.0 

			     0.0  1.0  0.0  1.0 
			     0.0  1.0  0.0  1.0 
			     0.0  1.0  0.0  1.0 

			     0.5  0.5  0.0  1.0  
			     0.5  0.5  0.0  1.0 
			     0.5  0.5  0.0  1.0 

			     0.5  0.5  0.0  1.0 
			     0.5  0.5  0.0  1.0 
			     0.5  0.5  0.0  1.0 

			     1.0  0.0  0.0  1.0 
			     1.0  0.0  0.0  1.0 
			     1.0  0.0  0.0  1.0 

			     1.0  0.0  0.0  1.0 
			     1.0  0.0  0.0  1.0 
			     1.0  0.0  0.0  1.0 

			     0.0  1.0  1.0  1.0 
			     0.0  1.0  1.0  1.0 
			     0.0  1.0  1.0  1.0 

			     0.0  1.0  1.0  1.0 
			     0.0  1.0  1.0  1.0 
			     0.0  1.0  1.0  1.0)
	(vertex-buffer win) (setup-buffer :array-buffer
					  :float 
					  (vertex-array win))
	(offset-uniform win) (gl:get-uniform-location 
			      (program win) "offset")
	(vao win) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vao win))
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw))

(defmethod glut:display ((win arc-tut-window))
  (restartable
    (let ((color-data (/ (length (vertex-array win)) 2)))
      (gl:clear-color 0.0 0.0 0.0 0.0)
      (gl:clear :color-buffer-bit)

      (gl:use-program (program win))
      (gl:uniformf (offset-uniform win) 0.5 0.25)

      (gl:bind-buffer :array-buffer (vertex-buffer win))
      (gl:enable-vertex-attrib-array 0)
      (gl:enable-vertex-attrib-array 1)
      
      (gl:vertex-attrib-pointer 0 4 :float :false 
				0 (cffi:null-pointer))
      (gl:vertex-attrib-pointer 1 4 :float :false 
				0 (cffi:make-pointer color-data))
      (gl:draw-arrays :triangles 0 36)
      (gl:disable-vertex-attrib-array 0)
      (gl:disable-vertex-attrib-array 1)
      (gl:use-program 0)
      (glut:swap-buffers)
      (glut:post-redisplay))))

(defmethod glut:reshape ((win arc-tut-window) width height)
  (gl:viewport 0 0 width height))

(defmethod glut:keyboard ((win arc-tut-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defmethod glut:idle ((win arc-tut-window))
  (restartable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))

(defun compute-position-offsets ()
  (let* ((loop-duration 5.0)
	 (scale (/ ( * base:+pi+ 2.0) loop-duration))
	 (elapsed-time (/ (glut:get :elapsed-time) 1000.0))
	 (curr-time-through-loop (mod elapsed-time 
				      loop-duration))
	 (x (* 0.5 (sin (* scale curr-time-through-loop))))
	 (y (* 0.5 (cos (* scale curr-time-through-loop)))))
    (make-vector2 x y)))

(defmethod adjust-vertex-data ((win arc-tut-window) offset)
  (let ((v-data (vertex-array win)))
    (loop for i from 0 below (length v-data) by 4
       for va = (aref v-data i)
       for vb = (aref v-data (+ i 1))
       do (setf (aref v-data i) (+ va (v-x offset)))
	  (setf (aref v-data (+ i 1)) (+ vb (v-y offset))))
    (sub-buffer :array-buffer :float (vertex-buffer win) v-data)))

(defun run-demo ()
  (glut:display-window (make-instance 'arc-tut-window)))
