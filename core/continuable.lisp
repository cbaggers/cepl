(in-package :cepl)

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continuable: Continue")))

(docs:define-docs
  (defmacro continuable
      "
Helper macro that we can use to allow us to continue from an
error. Remember to hit C in slime or pick the restart so
errors don't kill the app.
"))
