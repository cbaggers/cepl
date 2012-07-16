;;; working through the arcsynthesis tutorials to make sure
;;; our libraries work well enough.

;;; This uses the formats and streams mechanism I have written
;;; after going through tuts 1-6 keep things very close to the 
;;; c++ source. Hopefully this will be much tidier!

(in-package :arc-tuts)

(defclass arc-tut-window (glut:window)
  ((vbuff :accessor vertex-buffer)
   (program :accessor program))
  (:default-initargs :width 500 :height 500 :pos-x 100
		     :pos-y 100 
		     :mode `(:double :alpha :depth :stencil)
		     :title "ArcSynthesis Tut 1"))


(defmethod glut:display-window :before ((win arc-tut-window))
  (let* ((data `(-0.75  0.75  0.0  1.0
		  0.0   0.0  0.0  1.0
		 -0.75 -0.75  0.0  1.0))
	 (format (cgl:def-data-format 
		   (:type :float :components (x))))
	 (gl-array (cgl:alloc-array-gl format (length data)))
	 (buffer-id (cgl:gen-buffer)))
    (loop for item in data
	 for index from 0 to (1- (length data))
	 do (setf (cgl:aref-gl gl-array index 'x) item))
    (cgl:populate-buffer buffer-id gl-array)
    (setf (program win) (cgl:dumb-make-program 
			 (mapcar #'cgl:make-shader	
	 '("/home/baggers/Code/lisp/cepl/arc-tuts/tut1.vert"
	  "/home/baggers/Code/lisp/cepl/arc-tuts/tut1.frag")))
	  (vertex-buffer win) buffer-id)
    (print (cgl:program-uniforms (program win)))))


(defmethod glut:display ((win arc-tut-window))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  (gl:use-program (program win))
  (gl:bind-buffer :array-buffer (vertex-buffer win))

  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 4 :float :false 
			    0 (cffi:null-pointer))
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)
  (glut:swap-buffers))

(defmethod glut:reshape ((win arc-tut-window) width height)
  (gl:viewport 0 0 width height))

(defmethod glut:keyboard ((win arc-tut-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defun run-demo ()
  (glut:display-window (make-instance 'arc-tut-window)))
