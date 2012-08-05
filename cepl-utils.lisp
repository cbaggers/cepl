;; This is a stack of useful functions not really thought of as
;; tools for writing games specifically, but rather for writing
;; cepl. 
;; Saying that though, any use is wonderful so enjoy.

(in-package :cepl-utils)

;;this will be pretty inefficient, but shoudl be fine for code trees
(defun walk-replace (to-replace replace-with form)
  (cond ((null form) nil)
	((atom form) (if (eql form to-replace)
			 replace-with
			 form))
	(t (cons (walk-replace to-replace 
			       replace-with 
			       (car form)) 
		 (walk-replace to-replace 
			       replace-with 
			       (cdr form))))))

(defun file-to-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated 
   string, returning two values: the string and the number of 
   bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

;; output one string from all the items passed in
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; output one symbol
;; if input is symbols then output regualr symbol
;; if input strings, then output |symbol like this|
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun make-keyword (&rest args)
  (values (intern (apply #'mkstr args) "KEYWORD")))

;; take flat list and emit a list of lists, each n long
;; containing the elements of the original list
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n)
				   acc))
		   (nreverse (cons source acc))))))
    (if source 
	(rec source nil) 
	nil)))
