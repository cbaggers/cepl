;; lets play around with glsl!
(in-package :cglsl)

(defparameter *symbol-map* `((cl:+ . gl+)
			     (cl:- . gl-)
			     (cl:let . gl-let*)
			     (cl:setf . gl-setf)))

(defmacro defshader (name (&rest args) &body body)
  `(defun ,name ,args
     (let ((result (list ,@(walk-replace-numbers 
			    (sublis *symbol-map* body)))))
       (loop for chunk in result
	     do (loop for line in (append (percolate-to-block chunk)
					  (list (code chunk)))
		      do (format t "~{~a~^ ~};~%" line))))))

(defshader test ()
  (let (((a gl-int) 1)
	((b gl-int) 2))
    (+ a b (let (((c gl-int) (+ 3 b))) c))))

;; do we put the type in code? is this a type changing 
;; command, and if so - shouldn't it percolate to the 
;; next block?
(defun gl-setf (var val)
  (make-instance 
   (gl-type var)
   :code `(,(code var) = ,(code val))
   :percolate-to-block (append (percolate-to-block var)
			       (percolate-to-block val))
   :percolate-to-top (append (percolate-to-top var)
			     (percolate-to-top val))))

(defun gl+ (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (intersperse '+ (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl- (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (intersperse '+ (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot subtract them together")))

(defun class-name-sym (obj)
  (class-name (class-of obj)))

(defun types-match (args)
  (let ((gtype (class-name (class-of (first args)))))
    (every #'(lambda (x) (typep x gtype)) args)))

(defun intersperse (symb sequence)
  (rest (mapcan #'(lambda (x) (list symb x)) sequence)))

(defun gl-percolate-to-block (&rest args)
  (let ((last-index (- (length args) 1)))
    (make-instance 
     (gl-type (elt args last-index))
     :code (code (elt args last-index))
     :percolate-to-block (append 
			  (mapcar #'code (subseq args 0 last-index))
			  (mapcan #'percolate-to-block args))
     :percolate-to-top (mapcan #'percolate-to-top args))))

(defmacro gl-let* (bindings &body body)
  (sublis 
   *symbol-map*
   (let ((name-map (loop for binding in bindings
			 collect (cons (caar binding)
				       `(make-instance 
					 ',(cadar binding)
					 :code ',(glsym))))))
     (sublis
      name-map
      `(gl-percolate-to-block
	,@(loop for binding in bindings
		collect `(instan ,(list 'setf 
					(caar binding) 
					(cadr binding))))
	,@body)))))

(defun instan (x)
  (make-instance 
   (class-name-sym x)
   :code `(,(class-name-sym x) ,@(code x))
   :percolate-to-block (percolate-to-block x)
   :percolate-to-top (percolate-to-top x)))

;;--------------------------------------------------

(let ((count 0))
  (defun glsym ()
    (setf count (1+ count))
    (cepl-utils:symb (format nil "_SL_~s_~s" 'var count))))

(defun walk-replace-numbers (form)
  (cond ((null form) nil)
	((atom form) (if (numberp form)
			 `(make-instance ',(quick-num-type form)
					 :code ,form)
			 form))
	(t (cons (walk-replace-numbers (car form)) 
		 (walk-replace-numbers (cdr form))))))

(defun quick-num-type (x)
  (cond ((integerp x) 'gl-int)
	((floatp x) 'gl-float)
	(t 'gl-number)))

(defmacro def-gl-types (types)
  `(progn
     ,@(loop for type-name in types
	     collect (let ((name (cepl-utils:symb 'gl- type-name)))
		       `(defclass ,name (gl-code)
			  ())))))

(def-gl-types (bool int uint float vec2 vec3 vec4 ivec2 ivec3 ivec4 uvec2 uvec3 uvec4 bvec2 bvec3 bvec4 mat2 mat3 mat4 number))

(defun gl-type (obj)
  (class-name (class-of obj)))

(defclass gl-code ()
  ((dimen
    :initarg :len
    :initform nil
    :reader len
    :writer (setf len))
   (code
    :initarg :code
    :initform nil
    :reader code
    :writer (setf code))
   (percolate-to-block
    :initarg :percolate-to-block
    :initform nil
    :reader percolate-to-block
    :writer (setf percolate-to-block))
   (percolate-to-top
    :initarg :percolate-to-top
    :initform nil
    :reader percolate-to-top
    :writer (setf percolate-to-top))))
