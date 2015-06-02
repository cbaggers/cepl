;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package base-matrices)

(defmacro m! (&rest components)
  (let ((dimension (floor (sqrt (length components)))))
    (if (or (> dimension 4)
            (< dimension 3))
        (error "Incorrect number of components for a matrix")
        `(,(cepl-utils:symb-package
	    (format nil "MATRIX~s" dimension)
	    "MAKE-MATRIX"
	    (cepl-utils:mkstr dimension))
	  ,@(loop for i in components
	       collect
		 (if (numberp i)
		     (coerce i 'single-float)
		     `(coerce ,i 'single-float)))))))
