(in-package base-matrices)

(defmacro m! (&rest components)
  (let* ((len (length components))
         (dimension (floor (sqrt len))))
    (if (or (not (eq len (expt dimension 2)))
            (< dimension 2))
        (error (format nil "Incorrect number of components for a matrix~s" dimension))
        `(,(cepl-utils:symbolicate-package
                        (format nil "MATRIX~s" dimension)
                        "MAKE-MATRIX"
                        (cepl-utils:mkstr dimension)) 
           ,@(loop for i in components
                collect
                  (if (numberp i)
                      (coerce i 'single-float)
                      `(coerce ,i 'single-float)))))))
