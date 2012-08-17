(in-package model-parsers)

(defun parse-obj-file (filename)
  (let ((objects nil)
	(current (make-hash-table))
	(groups '(:group nil :smoothing-group nil :merging-group nil)))
    (setf (gethash :name current) "cepl:no-name")
    (with-open-file (my-stream filename)
      (loop for line = (read-line my-stream nil 'my-eof-sym)
	   until (eq line 'my-eof-sym)
	 do (multiple-value-bind (obj new-obj new-groups)
		(process-obj-line current groups line)
	      (setf current obj)
	      (setf groups new-groups)
	      (when new-obj
		(when (> (hash-table-count current) 1)
		  (setf objects (cons current objects)))
		(setf current new-obj)))))
    (cons current objects)))

(defun process-obj-line (object groups raw-line)
  (let* ((split-raw (cl-utilities:split-sequence 
		    #\space raw-line :remove-empty-subseqs t))
	 (type (first split-raw))
	 (new-object nil))
    (cond ((equal type "o") 
	   (progn (setf new-object (make-hash-table))
		  (setf (gethash :name new-object) 
			(second split-raw))))
	  ((equal type "v") 
	   (setf (gethash :vertices object) 
		 (append (gethash :vertices object)
			 (list (string-list-to-native (cdr split-raw))))))
	  ((equal type "vt") 
	   (setf (gethash :vertex-tex-coords object) 
		 (append (gethash :vertex-tex-coords object)
			 (list (string-list-to-native (cdr split-raw))))))
	  ((equal type "vn") 
	   (setf (gethash :vertex-normals object) 
		 (append (gethash :vertex-normals object)
			 (list (string-list-to-native (cdr split-raw))))))
	  ((equal type "g") 
	   (setf groups
		 (cepl-utils:sub-at-index groups 1 (string-list-to-native (cdr split-raw)))))
	  ((equal type "s") 
	   (setf groups
		 (cepl-utils:sub-at-index groups 3 
			       (let ((grp (string-list-to-native (cdr split-raw))))
				 (if (equal grp "off")
				     0
				     (car grp))))))
	  ((equal type "mg") 
	   (setf groups
		 (cepl-utils:sub-at-index groups 5
			       (let ((grp (string-list-to-native (cdr split-raw))))
				 (if (equal grp "off")
				     0
				     (car grp))))))
	  ((equal type "p") 
	   (setf 
	    (gethash :points object) 
	    (append (gethash :points object)
		    (list 
		     (append (relative-indices
			      (string-list-to-native (cdr split-raw))
			      (length (gethash :vertices object)))
			     groups)))))
	  ((equal type "l") 
	   (setf 
	    (gethash :lines object) 
	    (append (gethash :lines object)
		    (list 
		     (append (relative-indices 
			      (string-list-to-native (cdr split-raw))
			      (length (gethash :vertices object)))
			     groups)))))
	  ((equal type "f") 
	   (setf 
	    (gethash :faces object) 
	    (append (gethash :faces object)
		    (list 
		     (append (relative-indices 
			      (handle-obj-face (cdr split-raw))
			      (length (gethash :vertices object)))
			     groups))))))
    (values object new-object groups)))

(defun string-list-to-native (split-line)
  (mapcar #'cepl-utils:safe-read-from-string split-line))

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
