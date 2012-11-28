(in-package :cglsl)

;;------------------------------------------------------------------
;; GLSL Core -
;;------------

;; These are core bits of the language. For the sake of tidyness
;; the bulk of the language is moved into other files.
;; Actually plenty of this is homeless code that will get sorted 
;; into another file later but hey ho!

(defun gl-percolate-to-block (&rest args)
  (let ((last-index (- (length args) 1)))
    (make-instance 
     (gl-type (elt args last-index))
     :code (code (elt args last-index))
     :percolate-to-block (append 
			  (mapcar #'code (subseq args 0 last-index))
			  (mapcan #'percolate-to-block args))
     :percolate-to-top (mapcan #'percolate-to-top args))))

(defun gl-setf (var val)
  (make-instance 
   (gl-type var)
   :code `(,(code var) = ,(code val))
   :percolate-to-block (append (percolate-to-block var)
			       (percolate-to-block val))
   :percolate-to-top (append (percolate-to-top var)
			     (percolate-to-top val))))

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
		collect `(gl-instan ,(list 'setf 
					(caar binding) 
					(cadr binding))))
	,@body)))))

(defun gl-instan (x)
  (make-instance 
   (class-name-sym x)
   :code `(,(class-name-sym x) ,@(code x))
   :percolate-to-block (percolate-to-block x)
   :percolate-to-top (percolate-to-top x)))

