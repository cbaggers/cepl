(in-package :cepl.ubos)

;;---------------------------------------------------

(defvar *lowest-unused-ubo-id* 0)
(defvar *freed-ubo-id* nil)

(defun get-free-ubo-id ()
  (if *freed-ubo-id*
      (pop *freed-ubo-id*)
      (incf *lowest-unused-ubo-id*)))

;;---------------------------------------------------

(defmethod print-object ((ubo ubo) stream)
  (format stream "#<UBO (~s) ~s>" (ubo-id ubo)
	  (element-type (ubo-data ubo))))

;;---------------------------------------------------

(defun make-ubo (data &optional element-type)
  (let ((ubo (%make-ubo :id (get-free-ubo-id)
			:data (make-gpu-array
			       (when data (vector data))
			       :dimensions 1
			       :element-type element-type)
			:index 0)))
    (%bind-ubo ubo)))


(defun make-ubo-from-array (data &optional (index 0) element-type)
  (assert (>= index 0))
  (assert (or (null element-type) (symbolp element-type)))
  (let* ((id (get-free-ubo-id))
	 (ubo
	  (etypecase data
	    (gpu-array
	     (when element-type
	       (assert (equal element-type (element-type data)) ()
                       'make-ubo-from-array-bad-type
                       :data data :element-type element-type))
	     (%make-ubo :id id
			:data data
			:index index
			:owns-gpu-array nil))
	    (c-array
	     (assert (equal element-type (element-type data)) ()
                     'make-ubo-from-array-bad-type
                     :data data :element-type element-type)
	     (%make-ubo :id id
			:data (make-gpu-array
			       (vector (aref-c data index))
			       :dimensions 1
			       :element-type (element-type data))
			:index 0))
	    (uploadable-lisp-seq
	     (make-ubo (elt data index) element-type)))))
    (%bind-ubo ubo)))

(defun make-ubo-from-buffer (&rest not-yet-implemented)
  (declare (ignore not-yet-implemented))
  (error "make-ubo-from-buffer is not yet implemented"))

(deferror make-ubo-from-array-bad-type () (data element-type)
    "CEPL: Invalid element-type for make-ubo-from-array

Attemped to make a ubo from ~s which but also specified that the element-type
should be ~s" data element-type)

;;---------------------------------------------------

(defun ubo-data-type (ubo)
  (gpu-array-bb-element-type (ubo-data ubo)))

;;---------------------------------------------------

;; {TODO} using the id as the binding point is crazy town as it doesnt
;;        take :max-uniform-buffer-bindings into account.
;;        (For example it's only 84 on my desktop)
(defun %bind-ubo (ubo)
  (let* ((data (ubo-data ubo))
	 (type (ubo-data-type ubo))
         (offset (+ (gpu-array-bb-offset-in-bytes-into-buffer data)
		    (cepl.c-arrays::gl-calc-byte-size
		     type (list (ubo-index ubo)))))
         (size (gl-type-size type))
         (gpu-buffer (gpu-array-buffer data)))
    (cepl.context::ubo-bind-buffer-id-range
     *cepl-context*
     (gpu-buffer-id gpu-buffer)
     (ubo-id ubo)
     offset
     size))
  ubo)

;;---------------------------------------------------

(defmethod push-g ((object c-array) (destination ubo))
  (push-g object (subseq-g (ubo-data destination) 0 1)))

(defmethod push-g ((object list) (destination ubo))
  (let ((g-array (ubo-data destination)))
    (with-c-array (arr (make-c-array
			(list object) :dimensions 1
			:element-type (element-type g-array)))
      (push-g arr destination))))

(defmethod pull1-g ((object ubo))
  (let* ((data (ubo-data object))
	 (x (cepl.gpu-arrays::gpu-array-pull-1
	     (subseq-g data 0 1)))
	 (r (aref-c x 0)))
    (if (typep r 'autowrap:wrapper)
	r
	(progn (free-c-array x) r))))

(defmethod pull-g ((object ubo))
  (elt (pull-g (subseq-g (ubo-data object) 0 1)) 0))

;;---------------------------------------------------

(defmethod free ((object ubo))
  (let ((data (ubo-data object)))
    (when (and data (ubo-owns-gpu-array object))
      (cepl.gpu-arrays.buffer-backed::free-gpu-array-bb data))
    (setf (ubo-data object) +null-buffer-backed-gpu-array+))
  +null-buffer-backed-gpu-array+
  t)
