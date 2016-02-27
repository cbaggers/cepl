;;;; package.lisp

(defpackage (#| TMPL_VAR name |#)
  (:use #:cl #:cepl #:temporal-functions
	#:varjo-lang #:rtg-math
	(#| TMPL_IF skitter-sdl-p |#):skitter.sdl2.keys :skitter.sdl2.mouse-buttons(#| /TMPL_IF |#)))
