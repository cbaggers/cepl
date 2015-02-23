(in-package :cgl)

(defmacro defsmacro (name lambda-list &body body)
  `(varjo::v-defmacro ,name ,lambda-list ,@body))
