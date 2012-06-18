;; this has two objects with no depth
(in-package :arc-tuts)

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
		     :title "ArcSynthesis Tut 6"))

(defun init-prog (win)
  (setf (program win) (cepl:make-program `("tut6-1.vert"
					   "tut6-1.frag")))
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
    (cepl:with-use-program (program win)
      (gl:uniform-matrix (perspective-matrix-uniform win)
			 4 (vector (perspective-matrix win))))))

(defun init-vb (win)
  (setf (vertex-data win) (cepl:make-gl-array-from-array :float
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
  (setf (index-data win) (cepl:make-gl-array-from-array :short
			  #(0 2 1
			    3 2 0
			    
			    4 5 6
			    6 7 4
			    
			    8 9 10
			    11 13 12
			    
			    14 16 15
			    17 16 14)))
  (setf (vertex-buffer win) 
	(cepl:setup-buffer :array-buffer (vertex-data win)))
  (setf (index-buffer win)
	(cepl:setup-buffer :element-array-buffer (index-data win))))

(defun init-vaos (win)
  ;; Sorry about the num-of-verts magic number crap
  ;; the tutorial is very C and messy. grrr.
  (let* ((num-of-verts 36)
	 (ob-1-color-offset (* 4 3 num-of-verts)))
    (setf (vao-1 win) (gl:gen-vertex-array))
    (cepl:with-bind-vao (vao-1 win)
      (gl:bind-buffer :array-buffer (vertex-buffer win))
      (gl:enable-vertex-attrib-array 0)
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 0 3 :float :false
				0 (cffi:null-pointer))
      (gl:vertex-attrib-pointer 1 4 :float :false 
				0 (cffi:make-pointer 
				   ob-1-color-offset))
      (gl:bind-buffer :element-array-buffer (index-buffer win)))))

(defmethod glut:display-window :before ((win arc-tut-window))
  ;; equivilent to init
  (init-prog win)
  (init-vb win)
  (init-vaos win)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0))
  
(defmethod glut:display ((win arc-tut-window))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (cepl:with-use-program (program win)
    (cepl:with-bind-vao (vao-1 win)
      (gl:uniformf (offset-uniform win) 0.0 0.0 0.0)
      (gl:draw-elements :triangles
			(gl:make-null-gl-array :unsigned-short)
			:count 12)
      (gl:uniformf (offset-uniform win) 0.0 0.0 0.0)
      (cepl:draw-elements-base-vertex :triangles
			   (gl:make-null-gl-array :unsigned-short)
			   0 (/ 36 2)
			   :count 12 )))
  (glut:swap-buffers)
  (glut:post-redisplay))



(defmethod glut:reshape ((win arc-tut-window) width height)
  (setf (matrix4:melm (perspective-matrix win) 0 0)
	(* (frustrum-scale win) (/ height width)))
  (setf (matrix4:melm (perspective-matrix win) 1 1)
	(frustrum-scale win))
  (cepl:with-use-program (program win)
    (gl:uniform-matrix (perspective-matrix-uniform win)
		       4 (vector (perspective-matrix win))))
  (gl:viewport 0 0 width height))

(defmethod glut:keyboard ((w arc-tut-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defun run-demo ()
  (glut:display-window 
   (make-instance 'arc-tut-window
		  :)))

(defmethod glut:idle ((win arc-tut-window))
  (cepl:restartable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))
