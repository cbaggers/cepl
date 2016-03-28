(in-package :cepl.memory)

(defgeneric free (object))

(defgeneric push-g (object destination))
(defgeneric pull-g (object))
(defgeneric pull1-g (object))
(defmethod pull-g ((object t)) object)
(defmethod pull1-g ((object t)) object)
