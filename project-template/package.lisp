;;;; package.lisp

(uiop:define-package (#| TMPL_VAR name |#)
  (:use #:cl #:cepl #:varjo-lang #:rtg-math
	(#| TMPL_IF skitter-sdl-p |#):skitter.sdl2.keys :skitter.sdl2.mouse-buttons(#| /TMPL_IF |#)
        (#| TMPL_IF livesupport-p |#):livesupport(#| /TMPL_IF |#)))
