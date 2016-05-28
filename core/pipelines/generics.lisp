(in-package :cepl.pipelines)

;;--------------------------------------------------

(defgeneric %recompile-gpu-function-and-pipelines (key))
(defgeneric inject-func-key (spec))
(defgeneric func-key= (x y))
(defgeneric gpu-func-spec (key &optional error-if-missing))
(defgeneric %unsubscibe-from-all (spec))
(defgeneric funcs-that-use-this-func (key))
(defgeneric %funcs-this-func-uses (key &optional (depth)))
(defgeneric pipelines-that-use-this-as-a-stage (func-key))
(defgeneric recompile-pipelines-that-use-this-as-a-stage (key))
(defgeneric func-key (source))
