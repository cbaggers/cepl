;; This is the same as 3-a but I have made an effort to use
;; less lines of code. I was testing this as I want to be able
;; to have all the features and flexibility of standard opengl
;; without having the ugly interface. To that end I would prefer
;; that the replacement not only performs well but also is tidier
;; and more concise.
;; Right now I'm still making use of global variables which is 
;; rubbish but it wouldn't be hard to get rid of this. The reason
;; I didnt was that it would be too different from 3-a and so a 
;; comparison would be a bit moot.
;; I also added a fps count into this one which gave me a great
;; chance to try out cepl-time.


(in-package :cepl-examples)

;; Globals - Too damn many of them, but its in keeping with
;;           the tutorials online
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *entities* nil)

;; Define data formats 
(cgl:define-interleaved-attribute-format vert-data 
  (:type :float :components (x y z))
  (:type :float :components (r g b a)))

;; The entities used in this demo
(defstruct entity 
  (stream nil)
  (pos (make-vector3 0.0 0.0 -20.0))
  (loop-angle 0.0)
  (scale 1.0)
  (matrix nil))

;----------------------------------------------

(defun init (program) 
  (let* ((index-list '(0  1  2 
		       1  0  3 
		       2  3  0 
		       3  2  1 
		       5  4  6 
		       4  5  7 
		       7  6  4 
		       6  7  5))
	 (vert-array (cgl:destructuring-allocate 
		      'vert-data 
		      '(((+1.0  +1.0  +1.0) (0.0  1.0  0.0  1.0))
			((-1.0  -1.0  +1.0) (0.0  0.0  1.0  1.0))
			((-1.0  +1.0  -1.0) (1.0  0.0  0.0  1.0))
			((+1.0  -1.0  -1.0) (0.5  0.5  0.0  1.0))
			((-1.0  -1.0  -1.0) (0.0  1.0  0.0  1.0))
			((+1.0  +1.0  -1.0) (0.0  0.0  1.0  1.0))
			((+1.0  -1.0  +1.0) (1.0  0.0  0.0  1.0))
			((-1.0  +1.0  +1.0) (0.5  0.5  0.0  1.0)))))
	(index-array (cgl:destructuring-allocate :short 
						 index-list))
	(vert-buffer (cgl:gen-buffer))
	(index-buffer (cgl:gen-buffer)))

    (setf *frustrum-scale* 
	  (cepl-camera:calculate-frustrum-scale 45.0))
    (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix 
			     *frustrum-scale*))
    (cgl:set-program-uniforms program :cameratoclipmatrix 
			      *cam-clip-matrix*)

    ;;setup data 
    (cgl:buffer-data index-buffer index-array
		     :buffer-type :element-array-buffer)

    ;;setup vaos & create entities
    (let ((vao-1
	   (cgl:make-vao (cgl:buffer-data vert-buffer vert-array)
			 index-buffer)))
      (setf *entities* 
	    (list 
	     (make-entity :stream 
			  (cgl:make-gl-stream 
			   :vao vao-1
			   :length (length index-list)
			   :element-type :unsigned-short))
	     (make-entity :loop-angle 180.0
			  :stream 
			  (cgl:make-gl-stream 
			   :vao vao-1
			   :length (length index-list)
			   :element-type :unsigned-short))))))
  
  ;;set options
  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0))  

;----------------------------------------------

(defun move-entity (ent)
  (let* ((new-loop (mod (+ (entity-loop-angle ent) 0.25)
			(* base-maths:+pi+ 2)))
	 (new-pos (make-vector3 (* 8.0 (sin new-loop)) 
				0.0
				(+ -20.0 (* 8.0 (cos new-loop))) ))
	 (new-scale (make-vector3 (+ 2.0 (sin new-loop))
				  (+ 2.0 (cos new-loop))
				  1.0)))
    (setf (entity-matrix ent) (matrix4:m*
    			       (matrix4:translation new-pos)
    				(matrix4:scale new-scale)))

    ;; (setf (entity-matrix ent) (matrix4:translation new-pos))

    (setf (entity-scale ent) new-scale)
    (setf (entity-loop-angle ent) new-loop)
    (setf (entity-pos ent) new-pos)
    ent))

(defun draw (program)
  (cgl::clear-depth 1.0)
  (cgl::clear :color-buffer-bit :depth-buffer-bit)

  ;;move the entities
  (setf *entities* (mapcar #'move-entity *entities*))

  (loop for entity in *entities*
       do (cgl::draw-streams program (list (entity-stream entity)) 
  		   :modeltocameramatrix (entity-matrix entity)))
  (gl:flush)
  (sdl:update-display))

(defun reshape (program width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
  	(* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1)
  	*frustrum-scale*)
  (cgl:set-program-uniforms program
			    :cameratoclipmatrix
			    *cam-clip-matrix*)
  (cgl::viewport 0 0 width height))

(defun update-swank ()
  (let ((connection (or swank::*emacs-connection*
			(swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

;----------------------------------------------

;; currently anything changed in here is going to need a restart
;; this is obviously unacceptable and will be fixed when I can
;; extract the sdl event handling from their loop system.

;; (defparameter program nil)
;; (defparameter *frustrum-scale* nil)
;; (defparameter *cam-clip-matrix* nil)
;; (defparameter *entities* nil)

(defun run-demo () 
  (sdl:with-init ()
    (sdl:window 
     640 480 :opengl t
     :resizable t
     :opengl-attributes '((:sdl-gl-doublebuffer 1)
			  (:sdl-gl-alpha-size 0)
			  (:sdl-gl-depth-size 16) 
			  (:sdl-gl-stencil-size 8)
			  (:sdl-gl-red-size 8)
			  (:sdl-gl-green-size 8)
			  (:sdl-gl-blue-size 8)))
    (let ((prog-1 (cgl:make-program 
		   (mapcar #'cgl:make-shader 
			   '("3.vert" "3.frag"))))
	  (loops 0)
	  (timer (make-time-buffer))
	  (stepper (make-stepper 1000)))
      (init prog-1)
      (reshape prog-1 640 480)
      (setf cl-opengl-bindings:*gl-get-proc-address* 
	    #'sdl-cffi::sdl-gl-get-proc-address)
      (sdl:with-events () 
	(:quit-event () t)
	(:VIDEO-RESIZE-EVENT (:w width :h height) 
			     (reshape prog-1 width height))
	(:idle ()
	       (base-macros:continuable (update-swank))
	       (base-macros:continuable (draw prog-1))
	       (setf loops (1+ loops))
	       (on-step-call (stepper
			      (funcall timer))
		 (print loops)
		 (setf loops 0)))))))
