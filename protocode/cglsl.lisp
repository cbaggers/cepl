;; lets play around with glsl!
(in-package :cglsl)


;; #version 330

;; layout(location = 0) in vec4 position;
;; layout(location = 1) in vec4 color;

;; smooth out vec4 interpColor;

;; uniform mat4 cameraToClipMatrix;
;; uniform mat4 worldToCameraMatrix;
;; uniform mat4 modelToWorldMatrix;

;; void main()
;; {
;; 	vec4 temp = modelToWorldMatrix * position;
;; 	temp = worldToCameraMatrix * temp;
;; 	gl_Position = cameraToClipMatrix * temp;
;; 	interpColor = color;
;; }

(defshader test-vert (:version 330 (vec4 position) (vec4 color)
			       &uniforms (mat4 camera-to-clip-matrix)
			       (mat4 world-to-camera-matrix)
			       (mat4 model-to-world-matrix))
  (let (((vec4 temp) (m* world-to-camera-matrix (m*v model-to-world-matrix position))))
      (out ((:gl gl-position) (* camera-to-clip-matrix temp))
	   ((vec4 interpColor :smooth) color))))

(defparameter *code*
  '((:version 330 (vec4 position) (vec4 color) &uniforms (mat4 camera-to-clip-matrix)
     (mat4 world-to-camera-matrix) (mat4 model-to-world-matrix))
    (let (((vec4 temp) (m* world-to-camera-matrix (m*v model-to-world-matrix position))))
      (out ((:gl gl-position) (* camera-to-clip-matrix temp))
	   ((vec4 interpColor :smooth) color)))))

(defparameter *glsl-spec* nil)

(defun compile-shader (raw)
  (destructuring-bind (args main-body)
      raw
    (let ((header (process-shader-args args))
	  (functions (process-shader-body main-body *glsl-spec*)))
      (print header)
      (print functions))))

(defun process-shader-args (args)
  (let* ((header (list (if (eq (first args) :version)
			   (format nil "#version ~s" (second args))
			   "#version 330")))
	 (uniforms-pos (position '&uniforms args))
	 (in-vals (subseq args (position-if #'consp args) uniforms-pos))
	 (uniforms (when uniforms-pos (subseq args (1+ uniforms-pos)))))
    (append 
     header
     (loop for in-spec in in-vals
	for i from 0
	collect (format nil "layout(location = ~s) in ~s ~s;" i (first in-spec) (second in-spec)))
     (loop for unif-spec in uniforms
	collect (format nil "uniform ~s ~s" (first unif-spec) (second unif-spec))))))

(defun quoted-list-p (x)
  (and (listp x) (eq 'quote (car x))))


;; the result of the code processing (the code for that line)
;; any functions that have been defined as a side effect of this line

;; if the its a list and the first item is a function then funcall 
;; the handler and let it handle all the processing of the block
;; all vars are cons pairs of type and name

;; vars cannot be defined inside an if

;; how do we handle this?
(setf a (+ 1 2 (let ((x 3)) (+ 1 x))))
;; should be 
;; int x = 3;
;; a = 1 + 2 + (1 + x);
;; note the defintion of x perculates up to before the setf
;; actually it goes up to the previous blocks...we need to 
;; track this somehow

;; so must return: the type of the value, the code, any code that needs to perculate up to the next block

(defun process-s-exp (sexp glsl-spec local-functions local-variables in-vars uniforms out-vars)
  (cond 
    ((null sexp) nil)
	((quoted-list-p sexp) 'array-or-something)
	((listp sexp) 'should-be-a-function-call)
	((atom sexp) 'better-be-a-var-of-some-kind)
	(t (error "What the hell is a bong key?"))))

(defun sl-array (sexp)
  (append '({) (mapcar ) '(})))

(defun sl-aref (sexp)
  '(a [] ]))

;; CL-USER> (format nil "~{~s~^~};" '(a [ 0 ] = 5))
;; "A[0]=5;"

;; NEED TO PASS VARS AND TYPES FROM HEADER SO WE CAN USE THEM HERE
(defun process-shader-body (body glsl-spec in-vars uniforms out-vars)
  (loop for sexp in body
     append (process-s-exp sexp glsl-spec nil nil in-vars uniforms out-vars)))

(defclass code-blob ()
  type
  code
  percolate)



;; (let (((vec4 temp) (m* world-to-camera-matrix (m*v model-to-world-matrix position))))
;;   (out ((:gl gl-position) (* camera-to-clip-matrix temp))
;;        ((vec4 interpColor :smooth) color)))

;; #version 330

;; layout(location = 0) in vec4 position;
;; layout(location = 1) in vec4 color;

;; smooth out vec4 interpColor;

;; uniform mat4 cameraToClipMatrix;
;; uniform mat4 worldToCameraMatrix;
;; uniform mat4 modelToWorldMatrix;

;; void main()
;; {
;; 	vec4 temp = modelToWorldMatrix * position;
;; 	temp = worldToCameraMatrix * temp;
;; 	gl_Position = cameraToClipMatrix * temp;
;; 	interpColor = color;
;; }

