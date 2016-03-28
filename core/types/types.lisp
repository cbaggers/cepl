(in-package :cepl.types)

(defgeneric lisp-type->pixel-format (type))

(defgeneric element-type (array))

(defgeneric element-byte-size (array))
