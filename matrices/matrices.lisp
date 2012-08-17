
(in-package :matrices)

;----------------------------------------------------------------

;; Reader macro used to make matrices of any size
(set-dispatch-macro-character #\# #\m
   #'(lambda (stream char-a char-b)
       (declare (ignore char-a)
		(ignore char-b))
       (let* ((attrs (loop for i in (read stream t nil t)
			collect 
			  (if (numberp i)
			      (coerce i 'single-float)
			      `(coerce ,i 'single-float))))
	      (dimension (floor (sqrt (length attrs))))
	      (command (cepl-utils:symbolicate-package
			(format nil "MATRIX~s" dimension)
			"MAKE-MATRIX"
			(cepl-utils:mkstr dimension))))
	 (cons command attrs))))
