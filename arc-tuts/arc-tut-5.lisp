;; this has two objects with no depth
(in-package :arc-tuts)

;;;--------------------------------------------------------------

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

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

(defun make-gl-array-from-array (data-type data)
  (let* ((data-length (length data))
	 (arr (gl:alloc-gl-array data-type data-length)))
    (dotimes (i data-length)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defun setup-buffer (buf-type gl-array 
		     &optional (draw-type :static-draw))
  (let* ((buffer (car (gl:gen-buffers 1))))
    (with-bind-buffer buf-type buffer
      (gl:buffer-data buf-type draw-type gl-array))
    buffer))

(defun sub-buffer (buffer-type buffer new-gl-array 
		   &optional (offset 0) 
		     (size (gl:gl-array-byte-size new-gl-array)))
  (with-bind-buffer buffer-type buffer
    (gl:buffer-sub-data buffer-type new-gl-array
			:offset offset :size size)))


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

;;;--------------------------------------------------------------

(defclass arc-tut-window (glut:window)
  ((vbuff :accessor vertex-buffer)
   (vdt :accessor vertex-data)
   (ibuff :accessor index-buffer)
   (idt :accessor index-data)
   (program-id :accessor program)
   (offset :accessor offset-uniform)
   (perspec-matrix :accessor perspective-matrix-uniform)
   (persp-matrix :accessor perspective-matrix)
   (v-array-ob-1 :accessor vao-1)
   (v-array-ob-2 :accessor vao-2)
   (frus-scale :accessor frustrum-scale))
  (:default-initargs :width 500 :height 500 :pos-x 100
		     :pos-y 100 
		     :mode `(:double :alpha :depth :stencil)
		     :title "ArcSynthesis Tut 5"))

(defun init-prog (win)
  (setf (program win) (initialize-program `("tut5-nodepth.vert"
					   "tut5-nodepth.frag")))
  (setf (offset-uniform win) (gl:get-uniform-location 
			      (program win) "offset"))
  (setf (perspective-matrix-uniform win) 
	(gl:get-uniform-location (program win) 
				 "perspectiveMatrix"))
  (setf (frustrum-scale win) 1.0)
  (let* ((f-near 1.0)
	 (f-far 3.0)
	 (f-scale (frustrum-scale win))
	 (persp-mat 
	  (matrix4:make-matrix4 f-scale 0.0 0.0 0.0
				0.0 f-scale 0.0 0.0
				0.0 0.0 (/ (+ f-far f-near)
					   (- f-near f-far)) -1.0
			        0.0 0.0 (/ (* 2 f-far f-near)
					   (- f-near f-far)) 0.0)))
    (setf (perspective-matrix win) persp-mat)
    (with-use-program (program win)
      (gl:uniform-matrix (perspective-matrix-uniform win)
			 4 (vector (perspective-matrix win))))))

(defun init-vb (win)
  (setf (vertex-data win) (make-gl-array-from-array :float
			     #(;Object 1 positions
				 -0.8  0.2  -1.75
				 -0.8  0.0  -1.25
				 0.8  0.0  -1.25
				 0.8  0.2  -1.75

				 -0.8  -0.2  -1.75
				 -0.8  0.0  -1.25
				 0.8  0.0  -1.25
				 0.8  -0.2  -1.75

				 -0.8  0.2    -1.75
				 -0.8  0.0  -1.25
				 -0.8  -0.2  -1.75

				 0.8  0.2    -1.75
				 0.8  0.0  -1.25
				 0.8  -0.2  -1.75

				 -0.8  -0.2  -1.75
				 -0.8  0.2    -1.75
				 0.8  0.2    -1.75
				 0.8  -0.2  -1.75

					;Object 2 positions
				 0.2    0.8  -1.75
				 0.0  0.8  -1.25
				 0.0  -0.8  -1.25
				 0.2    -0.8  -1.75

				 -0.2  0.8  -1.75
				 0.0  0.8  -1.25
				 0.0  -0.8  -1.25
				 -0.2  -0.8  -1.75

				 0.2    0.8  -1.75
				 0.0  0.8  -1.25
				 -0.2  0.8  -1.75

				 0.2    -0.8  -1.75
				 0.0  -0.8  -1.25
				 -0.2  -0.8  -1.75

				 -0.2  0.8  -1.75
				 0.2    0.8  -1.75
				 0.2    -0.8  -1.75
				 -0.2  -0.8  -1.75

					;Object 1 colors
				 0.75 0.75 1.0 1.0
				 0.75 0.75 1.0 1.0
				 0.75 0.75 1.0 1.0
				 0.75 0.75 1.0 1.0

				 0.0 0.5 0.0 1.0
				 0.0 0.5 0.0 1.0
				 0.0 0.5 0.0 1.0
				 0.0 0.5 0.0 1.0

				 1.0 0.0 0.0 1.0
				 1.0 0.0 0.0 1.0
				 1.0 0.0 0.0 1.0

				 0.8 0.8 0.8 1.0
				 0.8 0.8 0.8 1.0
				 0.8 0.8 0.8 1.0

				 0.5 0.5 0.0 1.0
				 0.5 0.5 0.0 1.0
				 0.5 0.5 0.0 1.0
				 0.5 0.5 0.0 1.0

					;Object 2 colors
				 1.0 0.0 0.0 1.0
				 1.0 0.0 0.0 1.0
				 1.0 0.0 0.0 1.0
				 1.0 0.0 0.0 1.0

				 0.5 0.5 0.0 1.0
				 0.5 0.5 0.0 1.0
				 0.5 0.5 0.0 1.0
				 0.5 0.5 0.0 1.0

				 0.0 0.5 0.0 1.0
				 0.0 0.5 0.0 1.0
				 0.0 0.5 0.0 1.0

				 0.75 0.75 1.0 1.0
				 0.75 0.75 1.0 1.0
				 0.75 0.75 1.0 1.0

				 0.8 0.8 0.8 1.0
				 0.8 0.8 0.8 1.0
				 0.8 0.8 0.8 1.0
				 0.8 0.8 0.8 1.0)))
  (setf (index-data win) (make-gl-array-from-array :short
			  #(0 2 1
			    3 2 0
			    
			    4 5 6
			    6 7 4
			    
			    8 9 10
			    11 13 12
			    
			    14 16 15
			    17 16 14)))
  (setf (vertex-buffer win) 
	(setup-buffer :array-buffer (vertex-data win)))
  (setf (index-buffer win)
	(setup-buffer :element-array-buffer (index-data win))))

(defun init-vaos (win)
  ;; Sorry about the num-of-verts magic number crap
  ;; the tutorial is very C and messy. grrr.
  (let* ((num-of-verts 36)
	 (ob-1-color-offset (* 4 3 num-of-verts))
	 (ob-2-color-offset (+ ob-1-color-offset
			       (* 4 4 (/ num-of-verts 2))))
	 (ob-2-vert-offset (* 4 3 (/ num-of-verts 2))))
    (setf (vao-1 win) (gl:gen-vertex-array))
    (gl:bind-vertex-array (vao-1 win))
    (gl:bind-buffer :array-buffer (vertex-buffer win))
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 0 3 :float :false
			      0 (cffi:null-pointer))
    (gl:vertex-attrib-pointer 1 4 :float :false 
			      0 (cffi:make-pointer 
				 ob-1-color-offset))

    (gl:bind-buffer :element-array-buffer (index-buffer win))
    (gl:bind-vertex-array 0)
    
    (setf (vao-2 win) (gl:gen-vertex-array))
    (gl:bind-vertex-array (vao-2 win))

    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 0 3 :float :false 0
    			      (cffi:make-pointer 
    			       ob-2-vert-offset))
    (gl:vertex-attrib-pointer 1 4 :float :false 0
    			      (cffi:make-pointer 
    			       ob-2-color-offset))
    (gl:bind-buffer :element-array-buffer (index-buffer win))
    (gl:bind-vertex-array 0)))

(defmethod glut:display-window :before ((win arc-tut-window))
  ;; equivilent to init
  (init-prog win)
  (init-vb win)
  (init-vaos win)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw))
  

(defmethod glut:display ((win arc-tut-window))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  (with-use-program (program win)
    (with-bind-vao (vao-1 win)
      (gl:uniformf (offset-uniform win) 0.0 0.0 0.0)
      (gl:draw-elements :triangles
			(gl:make-null-gl-array :unsigned-short)
			:count 12))
    (with-bind-vao (vao-2 win) 
      (gl:uniformf (offset-uniform win) 0.0 0.0 -1.0)
      (gl:draw-elements :triangles 
			(gl:make-null-gl-array :unsigned-short)
			:count 12)))
  (glut:swap-buffers)
  (glut:post-redisplay))

(defmethod glut:reshape ((win arc-tut-window) width height)
  (setf (matrix4:melm (perspective-matrix win) 0 0)
	(* (frustrum-scale win) (/ height width)))
  (setf (matrix4:melm (perspective-matrix win) 1 1)
	(frustrum-scale win))
  (with-use-program (program win)
    (gl:uniform-matrix (perspective-matrix-uniform win)
		       4 (vector (perspective-matrix win))))
  (gl:viewport 0 0 width height))

(defmethod glut:keyboard ((w arc-tut-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defun run-demo ()
  (glut:display-window (make-instance 'arc-tut-window)))

(defmethod glut:idle ((win arc-tut-window))
  (restartable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))
