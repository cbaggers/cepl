(v-defun some-func ((x :int))
  x)

(v-defmacro some-func (x)
  (let ((g (gensym)))
    `(labels-no-implicit ((,g ((x :int)) x))
       (,g ,x))))
