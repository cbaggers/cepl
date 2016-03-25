(in-package :cepl.generics)

 ;; will return vectors
(defgeneric pos (object))
(defgeneric rot (object))
(defgeneric dir (object))
(defgeneric vec (object))
(defgeneric size (object))
(defgeneric norm (object))
(defgeneric col (object))
(defgeneric tangent (object))
(defgeneric bi-tangent (object))


(defgeneric resolution (object)) ;; returns a vecn
(defgeneric dimensions (object)) ;; returns a list

;; will return objects
(defgeneric tex (object))

;; need to go check the return type is consistent
(defgeneric action (object))
(defgeneric button (object))
(defgeneric clicks (object))
(defgeneric data (object))
(defgeneric delta (object))
(defgeneric etype (object))
(defgeneric id (object))
(defgeneric key (object))
(defgeneric repeating (object))
(defgeneric state (object))
(defgeneric timestamp (object))


(defgeneric (setf pos) (val obj))
(defgeneric (setf rot) (val obj))
(defgeneric (setf dir) (val obj))
(defgeneric (setf vec) (val obj))
(defgeneric (setf size) (val object))
(defgeneric (setf norm) (val obj))
(defgeneric (setf col) (val obj))
(defgeneric (setf tex) (val obj))
(defgeneric (setf tangent) (val object))
(defgeneric (setf bi-tangent) (val object))

;;--

(defgeneric free (object))
(defgeneric free-gpu-array (gpu-array))
(defgeneric push-g (object destination))
(defgeneric pull-g (object))
(defgeneric pull1-g (object))
(defmethod pull-g ((object t)) object)
(defmethod pull1-g ((object t)) object)
(defgeneric free-texture (texture))
(defgeneric make-gpu-array (initial-contents &key))

(defgeneric internal-format (object))
(defgeneric element-type (array))
(defgeneric element-byte-size (array))
