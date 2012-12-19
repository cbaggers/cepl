;; [TODO]

;; [TODO] need to modify intersperse in some way so - can be negate
;; [TODO] in vars and uniforms need to be part of scope
;; [TODO] types need to be converted
;; [TODO] how are implicit conversions handled?
;; [TODO] types are actually a bigger problem in cepl. We need to
;;        look at them in uniforms, shaders, lisp and foreign code.
;;        there will be a way to unify these approaches
;; [TODO] add aref
;; [TODO] add melm

(in-package :cglsl)

(defparameter *symbol-map* `((cl:+ . gl+)
			     (cl:- . gl-)
			     (cl:let . gl-let*)
			     (cl:setf . gl-setf)))

;;----------------------------------------------------------
;; DEFSHADER -
;;------------


(defun lisp-to-glsl (version shader-type stream-spec uniform-specs 
		     &rest lisp-shader-source)
  (declare (ignore shader-type))
  (let ((out-vals (pull-glsl-out-vals lisp-shader-source)))
    (list (format nil "~A~%~A~%~A~%~A"
		  (parse-version version)
		  (if (stringp stream-spec)
                      stream-spec
                      (parse-stream-to-glsl stream-spec))
                  (if (stringp uniform-specs)
                      uniform-specs
                      (parse-uniforms-to-glsl uniform-specs))
                  (out-vals-to-glsl out-vals))
          out-vals)))

;;       ;; write anything that percolated to the top
;; 	 (format t "")
;; 	 ;; write main function
;; 	 (format t "void main()~%{~%")
;; 	 (loop for chunk in result
;; 	       do (loop for line in (append 
;; 				     (percolate-to-block chunk)
;; 				     (list (code chunk)))
;; 			do (format t "    ~{~a~^ ~};~%" line)))
;; 	 (format t "}~%")

(defun parse-version (version)
  (cond ((find version '(:330)) (format nil "#version ~A" version))
	((null version) (error "A GLSL version is required to produce shader"))
	(t (error (format nil "Conversion to version ~A of glsl is not currently supported." version)))))

;; Stream
;;  - part-1 vec3-gpu-array
;;              -  float length 3
;;  - part-2 jim-gpu-array
;;              -  vec3
;;              -  vec4
;;  - part-3 float-gpu-array
;;              -  float 
 
;; Our lisp shader should then have 3 input vars with accessors for 
;; Our generated shader should have 4 input vars.
;; Note that part-1 should be vec3 in generated shader NOT float array


;; [TODO] valid-gpu-stream-type could have a better pred name and also
;;        allows :mat4 grrr
;; [TODO] Find all symbols outside of scope: grep -nH -e "cgl::\(\w\|-\)\+" *
;; [TODO] ARGH, needs to support arrays - only arrays of these types, no structs
;;        these can be arrays of vectors - bummer!
;; [TODO] Centroid in

(defun parse-stream-to-glsl (stream-specs)  
  (let ((types (mapcar #'second stream-specs)))    
    (if (notany #'null (mapcar #'valid-gpu-stream-type types))
        (format 
         nil "~{layout(location = ~s) in ~A ~s~^~%~};" 
         (loop for (name type) in stream-specs
            :with layout-pos = 0
            :if (cgl::gl-struct-p type)
            append (loop for format in (cgl::glsl-type-format type)
                      collect (list layout-pos 
                                    (cgl::format-slot-type format)
                                    (cgl::format-slot-name format))
                      do (setf layout-pos 
                               (+ (cgl::glsl-size (cgl::format-slot-type format))
                                  layout-pos)))
            :else 
            collect (list layout-pos type name)
            do (setf layout-pos (+ (cgl::glsl-size type)
                                   layout-pos))))
        (error "Invalid type for stream"))))

;; Need to find any struct types recursivly
;; Need to write struct definitions
;; need to order them so no dependency issues
;; then write uniform definitions, should be easy then.
;; hmmm arrays
(defun parse-uniforms-to-glsl (uniforms)
  "")

(defun pull-glsl-out-vals (glsl-code)
  (when (listp glsl-code) 
    (if (eq (car glsl-code) 'out)
        (mapcar #'first (utils:group (rest glsl-code) 2))
        (mapcan #'pull-glsl-out-vals glsl-code))))

(defun out-vals-to-glsl (out-vals)
  (let ((vals (remove-if #'keywordp out-vals)))
    (format nil "~%~{~{~A~^ ~}~%~}" (mapcar #'reverse vals))))



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
