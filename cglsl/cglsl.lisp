;; [TODO]
;; - Add the last few operators
;; - modify slquickdef to use the multi-type-arg key
;; - modify definitions to use new slquickdef
;; - add vector and matrix constructors

(in-package :cglsl)

(defparameter *symbol-map* `((cl:+ . gl+)
			     (cl:- . gl-)
			     (cl:let . gl-let*)
			     (cl:setf . gl-setf)))

;;----------------------------------------------------------
;; DEFSHADER -
;;------------

(defmacro defshader (name (&rest args) &body body)
  ;; The argument layout is pretty ugly so the handling of that
  ;; if farmed off the the process-shader-args function
  (destructuring-bind (args uniforms-args compiled-initialisers)
      (process-shader-args args)
    ;; we are creating a function which will return the shader
    `(defun ,name (,@args &key ,@uniforms-args)
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

(def-gl-types ((gen float (vec vec2 vec3 vec4)) 
               (igen int (ivec ivec2 ivec3 ivec4))
               (ugen uint (uvec uvec2 uvec3 uvec4)) 
               (bgen bool (bvec bvec2 bvec3 bvec4))))
;; (def-gl-types ((gen float (vec vec2 vec3 vec4)) 
;;                (igen int (ivec ivec2 ivec3 ivec4))
;;                (ugen uint (uvec uvec2 uvec3 uvec4)) 
;;                (bgen bool (bvec bvec2 bvec3 bvec4))
;;                (mgen (mat2 mat2x2) (mat3 mat3x3) 
;;                      (mat4 mat4x4) mat2x3 mat2x4
;;                      mat3x2 mat3x4 mat4x2 mat4x3) 
;;                (sampler
;;                 (fsampler sampler1D sampler2D sampler3D
;;                           samplerCube sampler2DRect
;;                           sampler1DShadow sampler2DShadow 
;;                           sampler2DRectShadow
;;                           sampler1DArray sampler2DArray
;;                           sampler1DArrayShadow 
;;                           sampler2DArrayShadow
;;                           samplerBuffer sampler2DMS
;;                           sampler2DMSArray) 
;;                 (isampler isampler1D isampler2D isampler3D
;;                           isamplerCube isampler2DRect
;;                           isampler1DArray isampler2DArray
;;                           isamplerBuffer isampler2DMS
;;                           isampler2DMSArray) 
;;                 (usampler usampler1D usampler2D usampler3D
;;                           usamplerCube usampler2DRect
;;                           usampler1DArray usampler2DArray
;;                           usamplerBuffer usampler2DMS
;;                           usampler2DMSArray) )
;;                number))


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

(defun gen-gl-form (name args out-type)
  (let ((gl-name (glify-name name))
	(type-perms (list-permutations
		     (mapcar #'rest args))))
    (loop for types in type-perms
	  collect 
	  `(defmethod ,gl-name ,(mapcar #'list
				 (mapcar #'first args)
				 types)
	     (make-instance 
	      ,(if out-type
		   out-type
		   `(class-name-sym ,(caar args)))
	      :code (list ',name (comma ( code ,(caar args))))
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
  (let ((gl-name (glify-name name)))
    `(progn       
       ,(when dont-write-generic
	  `(defgeneric ,gl-name ,(mapcar #'first args)
	     (:documentation ,documentation)))
       
       ,@(loop for type-template in (if multi-type-arg
					args
					(list args))
	       append (gen-gl-form gl-name type-template out-type))
       ,(when shadow `(setf *symbol-map* (acons ,gl-name 
                                                ,shadow
                                                *symbol-map*))))))


