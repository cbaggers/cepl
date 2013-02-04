
(defparameter test-unifs
  '(("ambientintensity" :FLOAT 1)
    ("jam[1].intensity[0]" :FLOAT 20)
    ("jam[4].position" :FLOAT 1)
    ("lightIntensity" :FLOAT-VEC4-ARB 1)))

;; (process-uniform-details test-unifs '((jam vert) 
;; 				      (ambientintensity :float)
;; 				      (lightIntensity cgl-vec4)))


;; [TODO] Got to be a quicker and tidier way
(defun process-uniform-details (uniform-details uniform-vars)
  (let ((result nil)
	(paths (mapcar #'parse-uniform-path uniform-details)))
    (loop for detail in uniform-details
	  for path in paths
	  :do (setf result 
		    (acons (caar path) 
			   (cons (list (get-path-offset 
					path
					uniform-vars)
				       (second detail)
				       (third detail))
				 (rest (assoc (caar path)
					      result)))
			   result)))
    (loop for var in uniform-vars
	  :collect (assoc (first var) result))))

;; [TODO] If we load shaders from files the names will clash
(defun parse-uniform-path (uniform-detail)
  (labels ((s-dot (x) (split-sequence:split-sequence #\. x))
	   (s-square (x) (split-sequence:split-sequence #\[ x)))
    (loop for path in (s-dot (first uniform-detail))
	  :collect (let ((part (s-square (remove #\] path))))
		     (list (intern (string-upcase 
				    (first part)))
			   (if (second part)
			       (parse-integer (second part))
			       0))))))

(defun get-slot-type (parent-type slot-name)
  (second (assoc slot-name (varjo:struct-definition parent-type))))

(defun get-path-offset (path uniform-vars)
  (labels ((path-offset (type path &optional (sum 0))
	     (if path
		 (let* ((path-part (first path))
			(slot-name (first path-part))
			(child-type (varjo:type-principle
				     (get-slot-type type
						    slot-name))))
		   (path-offset 
		    child-type
		    (rest path)
		    (+ sum
		       (+ (cffi:foreign-slot-offset type slot-name) 
			  (* (cffi:foreign-type-size child-type)
			     (second path-part))))))
		 sum)))
    (let* ((first-part (first path))
	   (type (second (assoc (first first-part) uniform-vars)))
	   (index (second first-part)))
      (+ (* (cffi:foreign-type-size type) index)
	 (path-offset type (rest path))))))


(defun symbol-name-equal (a b)
  (equal (symbol-name a) (symbol-name b)))




