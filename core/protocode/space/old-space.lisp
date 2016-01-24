(in-package :jungl.space)

;;----------------------------------------------------------------------
;; spatial relationship

(defstruct (spatial-relationship (:constructor %make-sr) (:conc-name sr-))
  (source-id 0 :type fixnum :read-only t)
  (target-id 0 :type fixnum :read-only t)
  (to nil :type (or null mat4) :read-only nil)
  (from nil :type (or null mat4) :read-only nil))

(defmethod print-object ((r spatial-relationship) stream)
  (cond
    ((and (sr-to r) (sr-from r))
     (format stream "#< ~s ↔ ~s >"
	     (%space-ref (sr-target-id r)) (%space-ref (sr-source-id r))))
    ((sr-to r)
     (format stream "#< ~s → ~s >"
	     (%space-ref (sr-target-id r)) (%space-ref (sr-source-id r))))
    (t (format stream "#< ~s ← ~s >"
	     (%space-ref (sr-target-id r)) (%space-ref (sr-source-id r))))))

(defun make-spatial-relationship (source-id target-id to-m4 from-m4)
  (%make-sr :source-id source-id
	    :target-id target-id
	    :to to-m4
	    :from from-m4))

;;----------------------------------------------------------------------
;; Space

(defconstant +model-space+ 0)
(defconstant +relational-space+ 1)
(defconstant +hierachical-space+ 2)

(defvar *last-space-id* -1)

(deftclass (space (:constructor %make-space) (:conc-name %space-))
  (uid (incf *last-space-id*) :type :fixnum)
  (nht-id (error "id must be provided") :type :fixnum)
  (kind (error "space kind must be provided") :type (mod 3))
  (parent nil :type (or null space))
  (children nil :type (or null (array space (*))))
  (root nil :type (or null space))
  (neighbours (make-array 0 :element-type 'spatial-relationship
			  :initial-element (%make-sr))
	      :type (array spatial-relationship (*)))
  ;;
  ;; # optimization data
  ;;
  ;; distance to the root:
  ;; when transforming between hierarchical spaces with same root
  ;; it lets you start walking from the deeper one first, which means
  ;; that if they are on the same path from root the walk will find the
  ;; less deep node and thus early out.
  (depth (error "space depth must be provided") :type fixnum))

(defmethod print-object ((space space) stream)
  (ecase (%space-kind space)
    (+model-space+ (format stream "#<MODEL-SPACE ~s>" (%space-uid space)))
    (+relational-space+ (format stream "#<R-SPACE ~s>" (%space-uid space)))
    (+hierachical-space+ (format stream "#<H-SPACE ~s>" (%space-uid space)))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Constructors

;; usage idas:
;; (make-space a)
;; (make-space (a m4to m4from) b)
;; (make-space (:to a m4to) (:from a m4from))

(defun make-space* (relationships)
  (if (eq :parent (first relationships))
      (%make-heirarchical-space relationships)
      (%dispatch-relational-space relationships)))

(defun make-space (&rest relationships)
  (make-space* relationships))

(defun space! (&rest relationships)
  (make-space* relationships))

(defun disconnect-space (space)
  (declare (ignore space))
  (error "Please implement disconnect-space"))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Space cache

(defparameter *spaces-array-growth-rate* 64)

(defun make-space-array ()
  (make-array *spaces-array-growth-rate*
	      :element-type '(or null space)
	      :initial-element nil
	      :adjustable t :fill-pointer 0))

(defparameter spaces (make-space-array))

(defun %add-space-to-array (space)
  (when (>= (%space-nht-id space) (length spaces))
    (adjust-array spaces (+ *spaces-array-growth-rate* (length spaces))
		  :element-type '(or null space)
		  :initial-element nil))
  (setf (aref spaces (%space-nht-id space)) space)
  space)

(defun %space-ref (id)
  (aref spaces id))

(defmethod free ((space space))
  (free-space space))

(defun free-space (space)
  (let ((id (%space-nht-id space)))
    (when (> id 0)
      (setf (aref spaces id) nil)
      (jungl.space.routes:free-id id))
    (disconnect-space space)
    nil))

;;----------------------------------------------------------------------
;; Hierarchical Space

(defun %make-heirarchical-space (relationship)
  (dbind (&key parent (transform (m4:identity))) relationship
    (assert parent)
    (let ((space
	   (%make-space
	    :nht-id -1 :kind +hierachical-space+ :parent parent
	    :root (%space-root parent) :depth (1+ (%space-depth parent))
	    :children (make-array 0 :element-type 'spatial-relationship
				  :adjustable t :fill-pointer 0)
	    :neighbours (make-array
			 1 :element-type 'spatial-relationship
			 :initial-element (make-spatial-relationship
					   -1 -1 transform nil)))))
      (vector-push-extend space (%space-children parent))
      space)))

(defun %hspace-transform (hspace)
  (declare ;;(optimize (speed 3) (debug 0))
	   (inline sr-from %space-neighbours))
  (sr-to (aref (%space-neighbours hspace) 0)))

(defun parent-space (space)
  (or (%space-parent space)
      (ecase (%space-kind space)
	(+hierachical-space+
	 (error "You have found bug in Jungl:~%Hierarchical space without parent found: ~s" space))
	(+model-space+
	 (error "Model spaces do not have a parent space.~%~s" space))
	(+relational-space+
	 (error "Relational spaces do not have a parent space.~%~s" space)))))

(defun space-inverse-transform (space)
  (m4:affine-inverse (space-transform space)))

;;----------------------------------------------------------------------
;; Relational Space

(defun %dispatch-relational-space (relationships)
  (let ((relationships (mapcar #'parse-relationship relationships)))
    (if (model-relationship-p relationships)
	(%make-model-space (first relationships))
	(%make-relational-space relationships))))

(defun %make-relational-space (relationships)
  (let* ((id (jungl.space.routes:id!))
	 (spatial-relationships
	  (mapcar (lambda (x)
		    (dbind (target to-m4 from-m4) x
		      (make-spatial-relationship
		       id (%space-nht-id target) to-m4 from-m4)))
		  relationships)))
    (%make-space :nht-id id
		 :kind +relational-space+
		 :root nil ;; will be populated in a few lines
		 :neighbours (make-array
			      (length spatial-relationships)
			      :element-type 'spatial-relationship
			      :initial-contents spatial-relationships)
		 :depth 0)))

(defun parse-relationship (r)
  (dbind (target-space &optional (to-m4 (m4:identity)) from-m4)
      (listify r)
    (list target-space to-m4 from-m4)))

(defun model-relationship-p (relationships)
  (let ((r (first relationships)))
    (and (= (length relationships) 1)
	 (second r)
	 (not (third r)))))

;;----------------------------------------------------------------------
;; Model Space

(defun %make-model-space (relationship)
  (dbind (target-space transform) relationship
    (%make-space :nht-id -1 :kind +model-space+ :depth 0
		 :neighbours (make-array
			      1 :element-type 'spatial-relationship
			      :initial-element (make-spatial-relationship
						-1 (%space-nht-id target-space)
						transform nil)))))

(defun upgrade-from-model-space (model-space)
  (declare (ignore model-space))
  (error "implement upgrade-from-model-space"))

(defun %mspace-transform (mspace)
  (declare ;;(optimize (speed 3) (debug 0))
	   (inline sr-from %space-neighbours))
  (sr-to (aref (%space-neighbours mspace) 0)))


(defun %update-mspace-transform (mspace transform)
  (setf (sr-to (aref (%space-neighbours mspace) 0))
	transform)
  (error "implement %update-mspace-transform"))

;;----------------------------------------------------------------------
;; GPU

(varjo::def-v-type-class space-g (varjo:v-type)
  ((varjo::core :initform nil :reader varjo:core-typep)
   (varjo::glsl-string :initform "#<space>" :reader varjo:v-glsl-string)))

;; a name for the space
(defvar *current-space* (gensym "current-space"))
