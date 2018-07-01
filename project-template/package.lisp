;;;; package.lisp

(uiop:define-package #:(#| TMPL_VAR name |#)
    (:use #:cl #:cepl #:rtg-math #:vari
          (#| TMPL_IF skitter-sdl-p |#):cepl.skitter(#| /TMPL_IF |#)
          (#| TMPL_IF livesupport-p |#):livesupport(#| /TMPL_IF |#)))
