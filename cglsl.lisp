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

(defparameter *glsl-functions* nil)
(defparameter *glsl-operators* nil)
(defparameter *glsl-special-forms* nil)
(defparameter *reserved-characters* nil)

(defun process-shader-body (body)
  (let ((code-blobs (process-multi-sexp body '(nil nil nil nil))))
    (if (every #'null (mapcar #'percolate-to-block code-blobs))
	(append (mapcan #'percolate-to-top-level code-blobs)
		(mapcan #'code code-blobs))
	(error "Some kind of crazy error...involving bats"))))

(defun process-multi-sexp (sexps state)
  (loop for sexp in sexps
     collect (process-sexp sexp state)))

(defun process-sexp (sexp state)
  (cond ((null sexp) nil)
	((or (numberp sexp) (stringp sexp)) sexp)
	((quoted-list-p sexp) (array-formatter sexp state))
	((listp sexp) (expression-handler sexp state))
	(t (error (format nil "I dont know what this is: ~s" sexp)))))

(defun expression-handler (sexp state)
  (destructuring-bind (local-vars local-functions uniforms in-vars)
      state
    (let ((fun-name (car sexp)))
      (cond ((member fun-name *glsl-functions*) (do-thing))
	    ((member fun-name local-functions) (do-other))
	    (t (error "AAAAAAAAHHHHHH"))))))

(defun array-formatter (sexp state)
  (let* ((processed (process-multi-sexp (cadr sexp) state))
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
  (and (atom symb)
       (not (cl-ppcre:scan "[^a-zA-Z_0-9\-]+" (cepl-utils:mkstr symb)))))

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

