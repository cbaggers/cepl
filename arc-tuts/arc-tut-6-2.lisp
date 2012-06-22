;; this has two objects with no depth
(in-package :arc-tuts)

;;;--------------------------------------------------------------

(defstruct entity 
  (pos (make-vector3 0.0 0.0 -20.0))
  (loop-angle 0.0)
  (scale 1.0))

(defclass arc-tut-window (glut:window)
  ((vbuff :accessor vertex-buffer)
   (vdt :accessor vertex-data)
   (ibuff :accessor index-buffer)
   (idt :accessor index-data)
   (program-id :accessor program)
   (cam->clip-m-u :accessor cam->clip-uniform)
   (cam->clip-m :accessor cam->clip)
   (model->cam-m-u :accessor model->cam-uniform)
   (v-array-ob-1 :accessor vao-1)
   (v-array-ob-2 :accessor vao-2)
   (frus-scale :accessor frustrum-scale)
   (entities-list :accessor entities))
  (:default-initargs :width 500 :height 500 :pos-x 100
		     :pos-y 100 
		     :mode `(:double :alpha :depth :stencil)
		     :title "ArcSynthesis Tut 6 - Translation"))

(defun make-cam-clip-matrix (win)
  (let* ((f-near 1.0)
	 (f-far 45.0)
	 (f-scale (frustrum-scale win)))
    (matrix4:make-matrix4 f-scale 0.0 0.0 0.0
			  0.0 f-scale 0.0 0.0
			  0.0 0.0 (/ (+ f-far f-near)
				     (- f-near f-far)) -1.0
			  0.0 0.0 (/ (* 2 f-far f-near)
				     (- f-near f-far)) 0.0)))

(defun init-prog (win)
  (setf (program win) (cepl:make-program `("tut6-1.vert"
					   "tut6-1.frag")))
  (setf (cam->clip-uniform win) 
	(gl:get-uniform-location (program win) 
				 "cameraToClipMatrix"))
  (setf (model->cam-uniform win) 
	(gl:get-uniform-location (program win)
				 "modelToCameraMatrix"))
  (setf (frustrum-scale win) 
	(cepl:calculate-frustrum-scale 45.0))
  (setf (cam->clip win) (make-cam-clip-matrix win))
  (cepl:with-use-program (program win)
    (gl:uniform-matrix (cam->clip-uniform win)
		       4 (vector (cam->clip win)))))

(defun init-vb (win)
  (setf (vertex-data win) (cepl:make-gl-array-from-array :float
			   #(+1.0  +1.0  +1.0 
			     -1.0  -1.0  +1.0 
			     -1.0  +1.0  -1.0 
			     +1.0  -1.0  -1.0 
			     
			     -1.0  -1.0  -1.0 
			     +1.0  +1.0  -1.0 
			     +1.0  -1.0  +1.0 
			     -1.0  +1.0  +1.0 

			     0.0  1.0  0.0  1.0 
			     0.0  0.0  1.0  1.0
			     1.0  0.0  0.0  1.0
			     0.5  0.5  0.0  1.0
			     
			     0.0  1.0  0.0  1.0 
			     0.0  0.0  1.0  1.0
			     1.0  0.0  0.0  1.0
			     0.5  0.5  0.0  1.0)))
  (setf (index-data win) (cepl:make-gl-array-from-array :short
			  #(0  1  2 
			    1  0  3 
			    2  3  0 
			    3  2  1 
			    
			    5  4  6 
			    4  5  7 
			    7  6  4 
			    6  7  5)))
  (setf (vertex-buffer win) 
	(cepl:setup-buffer :array-buffer (vertex-data win)))
  (setf (index-buffer win)
	(cepl:setup-buffer :element-array-buffer (index-data win))))

(defun init-vaos (win)
  ;; Sorry about the num-of-verts magic number crap
  ;; the tutorial is very C and messy. grrr.
  (let* ((num-of-verts 8)
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
  (setf (entities win) (list (make-entity :loop-angle 0.0)))
  ;;set options
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0))

(defun move-entity (ent)
  (let* ((new-loop (mod (+ (entity-loop-angle ent) 0.01) 360.0))
	 (new-pos (make-vector3 (* 4.0 (sin new-loop)) 
				(* 4.0 (cos new-loop)) 
				-20.0))
	 (new-scale (make-vector3 (+ 1.0 (sin new-loop))
				  (+ 1.0 (cos new-loop))
				  1.0)))
    (make-entity :pos new-pos
		 :loop-angle new-loop
		 :scale new-scale)))

(defmethod glut:display ((win arc-tut-window))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (setf (entities win) (mapcar #'move-entity (entities win)))
  (cepl:with-use-program (program win)
    (cepl:with-bind-vao (vao-1 win)
      (labels ((draw-entity (ent) 
		 (gl:uniform-matrix 
		    (model->cam-uniform win) 4
		    (vector (matrix4:m*
			     (matrix4:translation
			      (entity-pos ent))
			     (matrix4:rotation-from-euler
			      (make-vector3
			       (entity-loop-angle ent)
			       (entity-loop-angle ent)
			       (entity-loop-angle ent)))))
		    nil)
		 (gl:draw-elements :triangles
				   (gl:make-null-gl-array 
				      :unsigned-short)
				   :count 24)))
	(mapcar #'draw-entity (entities win)))))
  (glut:swap-buffers)
  (glut:post-redisplay))


(defmethod glut:reshape ((win arc-tut-window) width height)
  (setf (matrix4:melm (cam->clip win) 0 0)
	(* (frustrum-scale win) (/ height width)))
  (setf (matrix4:melm (cam->clip win) 1 1)
	(frustrum-scale win))
  (cepl:with-use-program (program win)
    (gl:uniform-matrix (cam->clip-uniform win)
		       4 (vector (cam->clip win))))
  (gl:viewport 0 0 width height))

(defmethod glut:keyboard ((w arc-tut-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defun run-demo ()
  (glut:display-window 
   (make-instance 'arc-tut-window)))

(defmethod glut:idle ((win arc-tut-window))
  (cepl:restartable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))
