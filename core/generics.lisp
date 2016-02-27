(in-package :cepl-generics)

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

 ;; will return a list of fixnums
(defgeneric resolution (object))
;;(defgeneric dimensions (object)) ;; prefer this over resolution, yeah
                                   ;; this is the naming from array

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
(defgeneric (setf norm) (val obj))
(defgeneric (setf col) (val obj))
(defgeneric (setf tex) (val obj))
(defgeneric (setf tangent) (val object))
(defgeneric (setf bi-tangent) (val object))
