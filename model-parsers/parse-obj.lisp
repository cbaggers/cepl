;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package model-parsers)

(defun get-stuff (filename)
  (with-open-file (my-stream filename)
    (loop for line = (read-line my-stream nil 
				'my-eof-sym)
       until (eq line 'my-eof-sym)
       collect line)))

(defun parse-obj-file (filename)
  (proc-obj-lines  (with-open-file (my-stream filename)
		     (loop for line = (read-line my-stream nil 
						 'my-eof-sym)
			until (eq line 'my-eof-sym)
			collect line))))


(defun proc-obj-lines (obj-lines &optional objects groups 
				   smoothing-group
				   merging-group vertices normals
				   tex-coords points lines faces)
  ;; the first item in the line idicates the type of the data 
  ;; in the line
  (if obj-lines
   (let* ((line (cl-utilities:split-sequence
		 #\space (first obj-lines) :remove-empty-subseqs t))
	  (native-line (mapcar #'cepl-utils:safe-read-from-string line))
	  (type (first native-line))
	  (body (rest native-line)))
     ;; first item in the list will specify the type of data in the line
     (case type
       (o  (proc-obj-lines (rest obj-lines) 
			   (when vertices
			     (cons (list (reverse vertices) 
					 (reverse normals)
					 (reverse tex-coords)
					 (reverse points)
					 (reverse lines)
					 (reverse faces)) objects))
			   groups smoothing-group merging-group nil 
			   nil nil nil nil nil))
       (v  (proc-obj-lines (rest obj-lines) objects groups 
			  smoothing-group merging-group
			  (cons (apply #'v:swizzle body) vertices)
			  normals tex-coords points lines faces))
       (vt (proc-obj-lines (rest obj-lines) objects groups
			  smoothing-group merging-group vertices
			  normals (cons (apply #'v:swizzle body) 
					tex-coords) 
			  points lines faces))
       (vn (proc-obj-lines (rest obj-lines) objects groups 
			  smoothing-group merging-group vertices 
			  (cons (apply #'v:swizzle body) normals)
			  tex-coords points lines faces))
       (g  (proc-obj-lines (rest obj-lines) objects body
			  smoothing-group merging-group vertices 
			  normals tex-coords points lines faces))
       (s  (proc-obj-lines (rest obj-lines) objects groups 
			  (if (equal (car body) "off") 
			      0 (car body))
			  merging-group vertices normals tex-coords
			  points lines faces))
       (mg (proc-obj-lines (rest obj-lines) objects groups 
			  smoothing-group
			  (if (equal (car body) "off")
			      0 (car body))
			  vertices normals tex-coords points lines
			  faces))
       (p (proc-obj-lines (rest obj-lines) objects groups
			 smoothing-group merging-group vertices
			 normals
			 (cons (list (relative-indices 
				      body 
				      (length vertices)) 
				     groups smoothing-group 
				     merging-group) points)
			 lines faces))
       (l (proc-obj-lines (rest obj-lines) objects groups 
			  smoothing-group merging-group vertices
			  normals tex-coords points
			  (cons (list (relative-indices 
				       body 
				       (length vertices)) 
				      groups smoothing-group 
				      merging-group) lines) faces))
       (f (proc-obj-lines (rest obj-lines) objects groups 
			  smoothing-group merging-group vertices
			  normals tex-coords points lines
			  (cons (list (relative-indices 
				       (handle-obj-face (rest line))
				       (length vertices)) 
				      groups smoothing-group 
				      merging-group) faces)))
       (otherwise (proc-obj-lines (rest obj-lines) objects groups 
				  smoothing-group merging-group
				  vertices normals tex-coords 
				  points lines faces))))
   (cons (list vertices 
	       normals
	       tex-coords
	       points
	       lines
	       faces) objects)))


(defun relative-indices (line current-len)
  (mapcar #'(lambda (x) (cond ((null x) nil)
			      ((numberp x) (if (>= x 0)
					       (1- x)
					       (+ current-len x)))
			      (t (relative-indices x current-len)))) line))

(defun handle-obj-face (split-line)
  "splits each item in list on the forward slash and convert each of those to native"
  (mapcar #'(lambda (x) 
	      (mapcar #'cepl-utils:safe-read-from-string
		      (cl-utilities:split-sequence #\/ x))) 
	  split-line))
