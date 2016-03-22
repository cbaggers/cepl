(in-package :cepl.pipelines)

(defmacro defmacro-g (name lambda-list &body body)
  `(varjo:v-defmacro ,name ,lambda-list ,@body))

(defmacro define-compiler-macro-g (name lambda-list &body body)
  `(varjo:v-define-compiler-macro ,name ,lambda-list ,@body))
