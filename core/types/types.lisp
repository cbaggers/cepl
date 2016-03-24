(in-package :cepl.types)

(deftype uploadable-lisp-seq () '(or list vector array))

(defgeneric lisp-type->pixel-format (type))
