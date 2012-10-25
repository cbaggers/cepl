;; lets play around with glsl!
(in-package :cglsl)

;;--------------------------------------------------

(defun quoted-list-p (x)
  (and (listp x) (eq 'quote (car x))))

;;--------------------------------------------------

(defclass code-blob ()
  ((sl-type
    :initarg :sl-type
    :initform nil
    :reader sl-type
    :writer (setf sl-type))
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
   (percolate-to-top-level
    :initarg :percolate-to-top-level
    :initform nil
    :reader percolate-to-top-level
    :writer (setf percolate-to-top-level))))

;;--------------------------------------------------

;; function spec should be assoc list of function name e.g. aref
;; to list of pairs (arg-name arg-type) with the last item in list
;; being the type of the data returned by the function

(defparameter *glsl-functions* '((aref ((array (t array)) (index integer)) 0)))
;; [TODO] What are the lispy version of these ^ ^^ \| \& && \|\| ?\:
(defparameter *glsl-operators* '(+ - / % > < <= >= == !=)) 
(defparameter *glsl-special-forms* nil)
(defparameter *reserved-characters* nil)

;;--------------------------------------------------

(defmacro defspecial ( func-name (&rest args) &body body)
  (let ((safe-name (cepl-utils:symb 'glsl- func-name)))
    `(progn
       (defun ,safe-name ,args
         ,@body)
       (setf *glsl-special-forms* (acons ',func-name #',safe-name *glsl-functions*)))))

(defspecial block (sexp state)
  (let ((processed (process-multiple-sexp (cdr sexp) state)))
    (make-instance 'code-blob 
                   :code (mapcar #'percolate-to-block processed)
                   :percolate-to-block nil
                   :percolate-to-top-level (percolate-to-top-level processed)
                   :sl-type (sl-type (car (last processed))))))
(let ((a 1))
  (let ((a 2))
    (print a))
  (print a))

;; (let ((wam 1)
;;       (bam nil)
;;       (thankye (+ 1 2))
;;       (mam (make-array)))
;;   (print "do stuff"))

;; (destructuring-bind (local-vars local-functions uniforms in-vars))

(defspecial let (bindings &rest body)
  (let ((processed )))
  (make-instance 'code-blob
		 :code nil
		 :percolate-to-block (loop for dec in bindings
					collect ())))

;; thing the above is on the wrong track, will be useful later for
;; special forms though.

;;--------------------------------------------------

(defparameter *code*
  '((:version 330 (vec4 position) (vec4 color) &uniforms (mat4 camera-to-clip-matrix)
     (mat4 world-to-camera-matrix) (mat4 model-to-world-matrix))
    (let (((vec4 temp) (m* world-to-camera-matrix (m*v model-to-world-matrix position))))
      (out ((:gl gl-position) (* camera-to-clip-matrix temp))
	   ((vec4 interpColor :smooth) color)))))

(defshader test-vert (:version 330 (vec4 position) (vec4 color)
			       &uniforms (mat4 camera-to-clip-matrix)
			       (mat4 world-to-camera-matrix)
			       (mat4 model-to-world-matrix))
  (let (((vec4 temp) (m* world-to-camera-matrix (m*v model-to-world-matrix position))))
      (out ((:gl gl-position) (* camera-to-clip-matrix temp))
	   ((vec4 interpColor :smooth) color))))

(defmacro defshader (name (&rest args) &body body)
  `(defun ,name ()
     (print ',body)
     (compile-shader ',args ',body)))

(defun compile-shader (args main-body)
  (let ((header (process-shader-args args))
	(functions (process-shader-body main-body)))
    (print header)
    (print functions)))

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


(defun process-shader-body (body)
  (let* ((body (if (every #'listp body) body (list body)))
         (code-blobs (process-multiple-sexp body '(nil nil nil nil)))
         (code-for-blocks (mapcar #'percolate-to-block code-blobs)))
    (declare (ignore code-for-blocks))
    (loop for i in code-blobs
       do (print (code i)))))
                                        ;        (error "Some kind of crazy error...involving bats: ~s" code-for-blocks)

(defun process-multiple-sexp (sexps state)

  (loop for sexp in sexps
     collect (process-sexp sexp state)))

(defun process-sexp (sexp state)

  (cond ((null sexp) nil)
        ((or (numberp sexp) (stringp sexp)) (make-instance 'code-blob :code sexp))
        ((quoted-list-p sexp) (array-formatter sexp state))
        ((listp sexp) (expression-handler sexp state))
        (t (error (format nil "I dont know what this is: ~s" sexp)))))

(defun expression-handler (sexp state)
  (destructuring-bind (local-vars local-functions uniforms in-vars)
      state
    (declare (ignore local-vars uniforms in-vars))
    (let ((fun-name (car sexp)))
      (cond ((member fun-name *glsl-special-forms*) (lookup-and-call-special-form sexp state))
            ((member fun-name *glsl-operators*) (operator-formatter sexp state))
            ((member fun-name (append *glsl-functions* local-functions)) (function-formatter sexp state))
            (t (error (format nil "Cannot find a function called '~s' in scope" fun-name)))))))

(defun lookup-and-call-special-form (sexp state)
  (funcall (cadr (assoc (car sexp) *glsl-special-forms*)) (cdr sexp) state)) 

(defun operator-formatter (sexp state)

  ;; this is stub code
  (let* ((processed (process-multiple-sexp (cdr sexp) state))
         (types (mapcar #'sl-type processed)))

    (make-instance 
     'code-blob 
     :code (append `( \( ) 
                   (cdr (mapcan #'(lambda (x) (list (car sexp) x)) 
                                (mapcar #'code processed))) 
                   '( \) \; ))
     :percolate-to-block (mapcar #'percolate-to-block processed)
     :percolate-to-top-level (mapcar #'percolate-to-top-level 
                                     processed)
     :sl-type `(,(car types) array))))

(defun valid-types-p (actual-args arg-spec)
  "Used to check that a list of glsl types match a spec"
  (and (eq (length actual-args) (length arg-spec))
       (notany #'null
	      (loop for spec-arg in arg-spec
   		    for actual-arg in actual-args
		 collect (and (eq (length actual-arg) (length (second spec-arg)))
			      (notany #'null 
				      (loop for spec in (second spec-arg)
					 for actual in actual-arg
					 do (print (format nil "~s : ~s" actual spec))
					 collect (or (eq spec t)
						     (eq spec actual)))))))))

(defun function-formatter (sexp state)
  (let* ((func-name (first sexp))
	 (spec (cdr (assoc func-name *glsl-functions*)))
	 (processed (process-multiple-sexp func-name state))
         (arg-types (mapcar #'sl-type processed))
	 (type-spec (first spec)))
    (if (valid-types-p arg-types type-spec) 
     (make-instance
      'code-blob 
      :code (append `( \( ,func-name) (mapcar #'code processed) '( \) \; ))
      :percolate-to-block (mapcar #'percolate-to-block processed)
      :percolate-to-top-level (mapcar #'percolate-to-top-level processed)
      :sl-type (if (integerp (second spec))
		   (second (nth (second spec) type-spec)))))))

(defun array-formatter (sexp state)
  (let* ((processed (process-multiple-sexp (cadr sexp) state))
         (types (mapcar #'sl-type processed)))
    (if (every #'(lambda (x) (equal x (car types))) (cdr types))
        (make-instance 
         'code-blob 
         :code (append '({) (mapcar #'code processed) '(} \;))
         :percolate-to-block (mapcar #'percolate-to-block processed)
         :percolate-to-top-level (mapcar #'percolate-to-top-level processed)
         :sl-type `(,(car types) array))
        (error "All types must be the same to make an array: ~s" sexp))))

;;--------------------------------------------------

(defun valid-glsl-sym-name (symb)
  (and symb (not (cl-ppcre:scan "[^a-zA-Z_0-9\-]+" (cepl-utils:mkstr symb)))))

(let ((count 0))
  (defun glsym (&optional name)
    (setf count (1+ count))
    (cepl-utils:symb 
     (format nil "_SL_~s_~s" (or (gl-ify-name name) 'var) count))))


(defun gl-ify-name (symb)
  "take a string and changes it to uppercase and replaces
   all underscores _ with minus symbols -"
  (if (valid-glsl-sym-name symb)
      (cepl-utils:symb (string-upcase (substitute #\_ #\- (cepl-utils:mkstr symb))))
      (error (format nil "The name '~s' is not valid for use in glsl" symb))))

