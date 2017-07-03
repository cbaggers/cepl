(in-package :cepl.pipelines)

(defvar *pipeline-body-context-var* 'mapg-context)

(defun mapg-context-p (ctx)
  (and (symbolp ctx)
       (null (symbol-package ctx))
       (search "MAPG-CTX" (symbol-name ctx))))
