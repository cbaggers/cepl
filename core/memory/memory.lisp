(in-package :cepl.memory)

(defgeneric free (object)
  (:method ((object null))
    (declare (ignore object))
    (error "CEPL: free was called on nil")))
(defgeneric initialized-p (object))

(defgeneric push-g (object destination))
(defgeneric pull-g (object))
(defgeneric pull1-g (object))
(defgeneric copy-g (source destination))

(defmethod pull-g ((object t)) object)
(defmethod pull1-g ((object t)) object)
