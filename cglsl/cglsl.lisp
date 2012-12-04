;; [TODO]

;; - need to modify intersperse in some way so - can be negate
;; - in vars and uniforms need to be part of scope
;; - types need to be converted
;; - how are implicit conversions handled?
;; - types are actually a bigger problem in cepl. We need to
;;   look at them in uniforms, shaders, lisp and foreign code.
;;   there will be a way to unify these approaches

(in-package :cglsl)

(defparameter *symbol-map* `((cl:+ . gl+)
			     (cl:- . gl-)
			     (cl:let . gl-let*)
			     (cl:setf . gl-setf)))

;;----------------------------------------------------------
;; DEFSHADER -
;;------------

;; defun ,name (,@args ,@(when uniforms-args '(&key)) ,@uniforms-args)

(defmacro defshader (name (&rest args) &body body)
  ;; The argument layout is pretty ugly so the handling of that
  ;; if farmed off the the process-shader-args function
  (destructuring-bind (args uniforms-args compiled-initialisers)
      (process-shader-args args)
    (declare (ignore args uniforms-args))
    ;; we are creating a function which will return the shader
    `(defun ,name (&rest ignored-args)
       ;; This next bit is what creates the actual code
       (let ((result (list ,@(walk-replace-numbers 
			      (sublis *symbol-map* body)))))
	 ;; the rest of this function pretty printing the result
	 ;; into the correct layout
	 ;; write header - for now we are only targeting 330
	 (format t "#version 330~2%")
	 (format t "~{~a~^~%~}~2%"',compiled-initialisers)
	 ;; write anything that percolated to the top
	 (format t "")
	 ;; write main function
	 (format t "void main()~%{~%")
	 (loop for chunk in result
	       do (loop for line in (append 
				     (percolate-to-block chunk)
				     (list (code chunk)))
			do (format t "    ~{~a~^ ~};~%" line)))
	 (format t "}~%")))))

(defun process-shader-args (args)
  (when args
    (let* ((uniforms-pos (position '&uniforms args))
	   (in-vals (subseq args 0 uniforms-pos))
	   (uniforms (when uniforms-pos 
		       (subseq args (1+ uniforms-pos)))))
      (list
       (mapcar #'first in-vals)
       (mapcar #'first uniforms)
       (append
	(loop for in-spec in in-vals
	      for i from 0
	      collect (format nil "layout(location = ~s) in ~s ~s;" i (second in-spec) (first in-spec)))
	(loop for unif-spec in uniforms
	      collect (format nil "uniform ~s ~s" (second unif-spec) (first unif-spec))))))))


;;----------------------------------------------------------
;; TYPES -
;;--------

(defun gen-type-pairs (type-tree &optional (parent 'gl-code))
  (labels ((make-name (name) 
             (cepl-utils:symb 'gl- name))
           (gen-type (type-name &optional (parent 'gl-code))
	     (let ((name (make-name type-name)))
	       `(defclass ,name (,parent) ()))))
    (cond ((null type-tree) nil)
          ((atom type-tree) (list
                             (gen-type type-tree parent)))
          ((listp (first type-tree))
           (loop for x in type-tree
		 append (gen-type-pairs x parent)))
          ((atom (first type-tree))
           (cons (gen-type (first type-tree) parent)
                 (let ((new-parent (make-name 
                                    (first type-tree)))) 
                   (loop for x in (rest type-tree)
			 append (gen-type-pairs 
				 x
				 new-parent))))))))

(defmacro def-gl-types (types)
  `(progn
     ,@(gen-type-pairs types)))

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


;;----------------------------------------------------------
;; HELPER FUNCTIONS -
;;-------------------

(defun class-name-sym (obj)
  (class-name (class-of obj)))

(defun types-match (args)
  (let ((gtype (class-name (class-of (first args)))))
    (every #'(lambda (x) (typep x gtype)) args)))

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

(defun comma (&rest things)
  (cepl-utils:intersperse #\, things))

(defun glify-name (name)
  (cepl-utils:symb 'gl- name))

(defun list-permutations (lists &optional accum)
  (if lists
      (loop for item in (first lists)
	    append (list-permutations (rest lists)
				      (cons item accum)))
      (list (reverse accum))))

(defun gen-gl-form (name string-name args out-type)
  (let ((type-perms (list-permutations
		     (mapcar #'rest args))))
    (loop for types in type-perms
	  collect 
	  `(defmethod ,name ,(mapcar #'list
			      (mapcar #'first args)
			      types)
	     (make-instance 
	      ,(if out-type
		   out-type
		   `(class-name-sym ,(caar args)))
	      :code (list ,string-name (comma (code ,(caar args))))
	      :percolate-to-block 
	      (append ,@(loop for arg in args
			      collect `(percolate-to-block 
					,(car arg))))
	      :percolate-to-top 
	      (append ,@(loop for arg in args
			      collect `(percolate-to-top 
					,(car arg)))))))))

(defmacro slquickdef (name args &key out-type 
                                  (documentation "") 
                                  shadow
                                  multi-type-arg
				  dont-write-generic)
  (let ((string-name (if (stringp name) 
			 name
			 (string-downcase (utils:mkstr name))))
	(name (glify-name (string-upcase name))))
    `(progn       
       ,(when (not dont-write-generic)
	  `(defgeneric ,name ,(mapcar #'first (if multi-type-arg
						  (first args)
						  args))
	     (:documentation ,documentation)))
       
       ,@(loop for type-template in (if multi-type-arg
					args
					(list args))
	       append (gen-gl-form name string-name 
				   type-template out-type))
       ,(when shadow `(setf *symbol-map* (acons ,name 
                                                ,shadow
                                                *symbol-map*))))))
