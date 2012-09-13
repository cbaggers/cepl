;; This is for my experiment on how to abstract away as much of 
;; opengls buffers, vaos etc without losing the power they 
;; provide.

(in-package :cepl-gl)

;;;--------------------------------------------------------------
;;; GLSTRUCTS ;;;
;;;-----------;;;

(defmacro defglstruct (name &body slot-descriptions)
  (let ((clauses (glstruct-slot-descriptions-to-clauses
		  slot-descriptions)))
    `(progn 
       ;; make the c-struct that will be used by the glarrays
       (defcstruct ,name 
	 ,@(loop for clause in clauses append (rest clause)))
       ;; make the getters for the foreign array
       ;; [TODO] make this generic for gl-arrays and gpu-arrays
       ,@(loop for clause in clauses
	    collect 
	      (gen-component-getters name clause))
       ;; make the setters for the foreign array
       ;; [TODO] make this generic for gl-arrays and gpu-arrays
       ,@(loop for clause in clauses
	    collect 
	      (gen-component-setters name clause)))))

(defun gen-component-setters (type-name clause)
  (let ((comp-name (first clause))
	(slots (rest clause)))
    (if (eq (length slots) 1)
	(let ((slot (first slots)))
	  `(defun 
	       (setf ,(cepl-utils:symb type-name '- comp-name)) 
	       (value instance)
	      (setf (foreign-slot-value instance 
					',type-name
					',(first slot))
		    value)))
	`(defun 
	     (setf ,(cepl-utils:symb type-name '- comp-name))
	     (value instance)
	   ,@(loop for slot in slots
		  for i from 0
		collect 
		  `(setf (foreign-slot-value instance
					     ',type-name
					     ',(first slot))
			 (aref value ,i)))))))

;; [TODO] single float for returned array
(defun gen-component-getters (type-name clause)
  (let ((comp-name (first clause))
	(slots (rest clause)))
    `(defun ,(cepl-utils:symb type-name '- comp-name) (instance)
       ,(if (eq (length slots) 1)
	    (let ((slot (first slots)))
	      `(foreign-slot-value instance 
				   ',type-name
				   ',(first slot)))
	    `(make-array ,(length slots)
			 :element-type ,(second (first slots))
			 :initial-contents
			 (list ,@(loop for slot in slots
				    collect
				      `(foreign-slot-value 
					instance 
					',type-name
					',(first slot)))))))))

(defun glstruct-slot-descriptions-to-clauses (slot-descriptions)
  (labels ((type-length (type) 
	     (or (position type 
			   '(nil nil :vector2 :vector3 :vector4))
		 1)))
    (loop for clause in slot-descriptions
       collect 
	 (destructuring-bind (name &key type &allow-other-keys)
	     clause
	   (let* ((tlength (type-length type)) 
		  (type (if (> tlength 1) 
			    :float 
			    type)))
	     (cons name (loop for i below tlength
			   collect `(,(cepl-utils:symb name i)
				      ,type))))))))

;;;--------------------------------------------------------------
;;; GLARRAYS ;;;
;;;----------;;;

;; the struct containing details about our gl-arrays
(defstruct (glarray (:copier nil))
  "Pointer to C array with size and type information attached."
  (pointer (null-pointer))
  (size 0 :type unsigned-byte)
  (type nil :type symbol))

;; check for glstruct type existance
(defun make-gl-array (&key length element-type 
			(initial-contents nil))
  (declare (ignore initial-contents))
  (make-glarray :pointer (foreign-alloc type :count length)
		:size length 
		:type type))

(declaim (inline aref-gl))
(defun aref-gl (array index)
  "Returns the INDEX-th component of gl-array."
  (mem-aref (glarray-pointer array) 
	    (glarray-type array) 
	    index))

;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(let ((buffer-pool ()))
  (defun add-buffer-to-pool (buffer)
    (setf buffer-pool (cons buffer buffer-pool))
    buffer)

  (defun kill-all-buffers-in-pool ()
    (mapcar #'(lambda (x) (print "killing a buffer")) 
	    buffer-pool)))

(defstruct gpuarray 
  (buffer_id)
  (format_index))

(defun make-gpu-array (&key length element-type 
			 (location nil)
			 (initial-contents nil))
  (let ((buffer (add-buffer-to-pool (or location (gen-buffer))))
	(buffer-data-position 
	 (1+ (length glbuffer-format buffer))))
    ))

()

;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

;; (make-gpu-stream 
;;  :arrays (((glsubseq my-glarray-1 10) :enabled t)
;; 	  (my-glarray-1 :enabled nil))
;;  :length 3
;;  :indicies-gpu-array my-indicies-gpu-array)

(let ((vao-pool (make-hash-table)))
  (defun add-vao-to-pool (vao key)
    (setf (gethash key vao-pool) vao)
    vao)

  (defun kill-all-vaos-in-pool ()
    (mapcar #'(lambda (x) (print "killing a vao")) 
	    vao-pool)))

;;;--------------------------------------------------------------
;;; HELPERS ;;;
;;;---------;;;

(defun kill-managed-resources ()
  (kill-all-vaos-in-pool)
  (kill-all-buffers-in-pool))
