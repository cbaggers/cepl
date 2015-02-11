;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; package.lisp
(in-package #:cl-user)

(defpackage :cepl
  (:use :cl
        :base-vectors
        :base-matrices
        :base-maths
        :base-macros
        :temporal-functions
        :cepl-camera
        :live)
  (:import-from :cepl-gl
                :cls
                :pixel-format
                :pixel-format-of
                :describe-pixel-format
                :with-instances
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
                :g-pn
                :g-pc
                :g-pt
                :g-pnc
                :g-pnt
                :g-pntc
                :texref
                ;;---
                :gmap
                ;;---
                :make-fbo
                :make-fbos
                :with-bind-fbo
                :with-fbo-slots
                :fbo-attach
                :attachment-compatible
                :fbo-detach
                ;;---
                :def-gl-equivalent)
  (:import-from :utils
                :deferror
                :print-mem)
  (:import-from :cepl.events.sdl
                :case-events)
  (:export :cepl-gl
           :cls
           :pixel-format
           :pixel-format-of
           :describe-pixel-format
           :with-instances
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
           :g-pn
           :g-pc
           :g-pt
           :g-pnc
           :g-pnt
           :g-pntc
           :texref
           ;;---
           :gmap
           ;;---
           :make-fbo
           :make-fbos
           :with-bind-fbo
           :with-fbo-slots
           :fbo-attach
           :attachment-compatible
           :fbo-detach
           ;;---
           :def-gl-equivalent
           :repl
           ;;---
           :case-events
           :collect-event-types
           :evt+>
           :evt->
           :update-swank
           :peek))
