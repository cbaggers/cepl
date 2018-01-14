(in-package :cepl.measurements)


(docs:define-docs
  (defgeneric dimensions
      "
Returns the list containing the dimensions of the given value
")

  (defgeneric resolution
      "
Returns the resolution of the given value as a vec*
")
  (defgeneric origin
      "
Returns the origin of the given value as a vec
"))
