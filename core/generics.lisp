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
(defgeneric dimensions (object))
(defgeneric backed-by (object))
(defgeneric lisp-type->pixel-format (type))
(defmethod pull-g ((object t)) object)
(defmethod pull1-g ((object t)) object)
(defgeneric free-texture (texture))
(defgeneric make-gpu-array (initial-contents &key)
  (:documentation "This function creates a gpu-array which is very similar
   to a c-array except that the data is located in the memory
   of the graphics card and so is accessible to shaders.
   You can either provide and type and length or you can
   provide a c-array and the data from that will be used to
   populate the gpu-array with.

   Access style is optional but if you are comfortable with
   opengl, and know what type of usage pattern thsi array will
   have, you can set this to any of the following:
   (:stream-draw :stream-read :stream-copy :static-draw
    :static-read :static-copy :dynamic-draw :dynamic-read
    :dynamic-copy)"))
