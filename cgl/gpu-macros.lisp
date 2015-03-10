(in-package :cgl)

(defmacro defmacro-g (name lambda-list &body body)
  `(varjo::v-defmacro ,name ,lambda-list ,@body))
