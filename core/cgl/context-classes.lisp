;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cgl)

(cells:defmodel gl-context ()
  ((cache :cell nil :initform (make-hash-table))
   (handle :cell nil :initarg :handle :reader handle)
   (window :cell nil :initarg :window :reader window)
   (fbo :cell nil :initarg :window :reader fbo)
   (gl-initialized :cell t :initform (cells:c-in nil) :reader gl-initialized)))
