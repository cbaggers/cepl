;; This is to expand on using uniforms. Testing translation,
;; rotation and scaling in a 3D scene. It is also a better 
;; test of the vao generation functions

(defparameter *prog-1* nil)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *shaders* nil)
(defparameter *entities* nil)

;; Define data formats 
(cgl:defglstruct vert-data 
  (position :type :float :length 3)
  (color :type :float :length 4))

;; The entities used in this demo
(defstruct entity 
  (stream nil)
  (pos (v! 0.0 0.0 -20.0))
  (loop-angle 0.0)
  (scale 1.0)
  (matrix nil))

(defun init () 
  (setf *shaders* (cgl:load-shaders "3.vert" "3.frag"))
  (setf *prog-1* (cgl:make-program *shaders*))
  (setf *frustrum-scale* 
	(cepl-camera:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix 
			   *frustrum-scale*))
  (cgl:set-program-uniforms *prog-1* :cameratoclipmatrix *cam-clip-matrix*)

  ;;create entities
  (let* ((verts '((#(+1.0  +1.0  +1.0)  #(0.0  1.0  0.0  1.0)) 
		  (#(-1.0  -1.0  +1.0)  #(0.0  0.0  1.0  1.0))
		  (#(-1.0  +1.0  -1.0)  #(1.0  0.0  0.0  1.0))
		  (#(+1.0  -1.0  -1.0)  #(0.5  0.5  0.0  1.0))
		  (#(-1.0  -1.0  -1.0)  #(0.0  1.0  0.0  1.0)) 
		  (#(+1.0  +1.0  -1.0)  #(0.0  0.0  1.0  1.0))
		  (#(+1.0  -1.0  +1.0)  #(1.0  0.0  0.0  1.0))
		  (#(-1.0  +1.0  +1.0)  #(0.5  0.5  0.0  1.0))))
	 (indicies '(0  1  2    1  0  3    2  3  0    3  2  1 
		     5  4  6    4  5  7    7  6  4    6  7  5))
	 (stream (cgl:make-gpu-stream-from-gpu-arrays
		  :length (length indicies)
		  :gpu-arrays (cgl:make-gpu-array 
			       verts :element-type 'vert-data)
		  :indicies-array (cgl:make-gpu-array 
				   indicies 
				   :element-type :unsigned-short
				   :index-array t))))
    (setf *entities* (list (make-entity :stream stream)
			   (make-entity :loop-angle 3.14
					:stream stream))))
  
  ;;set options
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0))  

;----------------------------------------------

(defun move-entity (ent)
  (let* ((new-loop (mod (+ (entity-loop-angle ent) 0.05) 6.284))
	 (new-pos (v! (* 8.0 (sin new-loop)) 
		      0.0
		      (+ -20.0 (* 8.0 (cos new-loop)))))
	 (new-scale (v3:make-vector3 (+ 2.0 (sin new-loop))
				     (+ 2.0 (cos new-loop))
				     1.0)))
    (setf (entity-matrix ent) (matrix4:m*
    			       (matrix4:translation new-pos)
    				(matrix4:scale new-scale)))

    (setf (entity-scale ent) new-scale)
    (setf (entity-loop-angle ent) new-loop)
    (setf (entity-pos ent) new-pos)))

(defun draw ()
  (cgl:clear-depth 1.0)
  (cgl:clear :color-buffer-bit :depth-buffer-bit)

  (loop for entity in *entities*
       do (move-entity entity)
          (funcall *prog-1* 
		   `(,(entity-stream entity)) 
		   :modeltocameramatrix (entity-matrix entity)))
  (gl:flush)
  (sdl:update-display))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
  	(* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1)
  	*frustrum-scale*)
  (cgl:set-program-uniforms *prog-1* 
			    :cameratoclipmatrix *cam-clip-matrix*)
  (cgl:viewport 0 0 width height))

(defun run-demo () 
  (init)
  (reshape 640 480)
  (sdl:with-events () 
    (:quit-event () t)
    (:VIDEO-RESIZE-EVENT (:w width :h height) 
			 (reshape width height))
    (:idle ()
	   (cepl-utils:update-swank)
	   (base-macros:continuable (draw)))))
