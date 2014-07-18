;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; package.lisp
(defpackage :cepl
  (:use :cl
        :declarative-values
        :base-vectors
        :base-matrices
        :base-maths
        :interpolation
        :base-time
        :conditional-functions
        :base-macros)
  (:import-from :cepl-gl
                :cls
                :pixel-format
                :pixel-format-of
                :describe-pixel-format
                :defpipeline
                :defvshader
                :deffshader
                :defgshader
                :defshader
                :defsfun
                :defsmacro
                :defglstruct
                :gl-pull
                :gl-pull-1
                :gl-push
                :make-c-array
                :with-c-array                
                :free-c-array
                :aref-c
                :c-populate
                :make-gpu-array
                :make-gpu-arrays
                :gl-subseq
                :with-gpu-array-as-c-array
                :make-vertex-stream
                :make-texture                
                :with-texture-bound
                :p-n-t
                :texref)
  (:export :repl
           :%repl
           :case-events
           :collect-event-types
           :evt->
           :evt+>
           ;;---
           :cls
           :pixel-format
           :pixel-format-of
           :describe-pixel-format
           :defpipeline
           :defvshader
           :deffshader
           :defgshader
           :defshader
           :defsfun
           :defsmacro
           :defglstruct
           :gl-pull
           :gl-pull-1
           :gl-push
           :make-c-array
           :with-c-array                
           :free-c-array
           :aref-c
           :c-populate
           :make-gpu-array
           :make-gpu-arrays
           :gl-subseq
           :with-gpu-array-as-c-array
           :make-vertex-stream
           :make-texture                
           :with-texture-bound
           :p-n-t
           :texref))
