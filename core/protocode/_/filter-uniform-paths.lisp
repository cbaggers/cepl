;; * collect all opengl names
;; * Run this on the glsl for each name
;; * feed it through the tools we have
;; * we can then know what we need for each uniform
(defun try-thing (uniform-args glsl-shaders package)
  (let ((merged-shaders (format nil "~{~a~}" glsl-shaders)))
    (loop :for uniform :in uniform-args :collect
       (destructuring-bind (lisp-name varjo-type glsl-name place)
           (varjo:flesh-out-arg uniform)
         (declare (ignorable lisp-name varjo-type glsl-name place))
         (loop :for (start end)
            :on (cl-ppcre:all-matches (format nil "~a(.*?)[, \(\)]" glsl-name)
                                      merged-shaders) :by #'cddr :collect
            (let ((path (parse-uniform-path (list (subseq merged-shaders
                                                          start (1- end)))
                                            package))
                  (uniform-type (second uniform)))
              (get-path-offset path uniform-type)))))))
