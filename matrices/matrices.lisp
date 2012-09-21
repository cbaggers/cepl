;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

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
