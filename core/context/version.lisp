(in-package :cepl.context)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------

(defun+ get-best-glsl-version (cepl-context)
  (let ((gl-ctx (when cepl-context
                  (cepl.context::%cepl-context-gl-context cepl-context))))
    (if gl-ctx
        (assocr (cons (major-version gl-ctx)
                      (minor-version gl-ctx))
                '(((2 . 0) . :110)
                  ((2 . 1) . :120)
                  ((3 . 0) . :130)
                  ((3 . 1) . :140)
                  ((3 . 2) . :150)
                  ((3 . 3) . :330)
                  ((4 . 0) . :400)
                  ((4 . 1) . :410)
                  ((4 . 2) . :420)
                  ((4 . 3) . :430)
                  ((4 . 4) . :440)
                  ((4 . 5) . :450))
                :test #'equal)
        (last1 varjo:*supported-versions*))))

;;------------------------------------------------------------

(defun+ split-float-version (float)
  (let* ((fix (round float .1)))
    (multiple-value-bind (maj min) (floor fix 10)
      (list maj min))))

;;------------------------------------------------------------
