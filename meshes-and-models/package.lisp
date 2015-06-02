;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage :model-parsers
  (:use :cl)
  (:export :load-file
           :meshes->lists
           :mesh->lists
           :mesh-list->gpu
           :mesh->gpu
           :scene-meshes->gpu
           :calc-type)
  (:import-from :vector2
                :make-vector2)
  (:import-from :vector3
                :make-vector3)
  (:import-from :vector4
                :make-vector4))

(defpackage :meshes
  (:use :cl :cffi :base-macros :cepl-utils :base-vectors :cepl-generics
        :fn :split-sequence :cgl)
  (:export :mesh
           :vertices
           :indicies
           :primitive-type
           :transform-mesh
           :transform-mesh-with-matrix
           :polygonize
           :flatten-index))
