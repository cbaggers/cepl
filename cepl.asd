;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; cepl.asd

(asdf:defsystem #:cepl
                :serial t
                :depends-on (#:cl-opengl
                             #:cl-devil
                             #:swank
                             #:sdl2
                             #:varjo
                             #:temporal-functions
                             #:cl-utilities
                             #:cl-ppcre
                             #:symbol-munger
                             #:cells
                             #:classimp
                             #:fn_)
                :components
                (
                 (:module "cepl-utils"
                          :serial t
                          :components ((:file "package")
                                       (:file "cepl-utils")))
                 (:module "base-macros"
                          :serial t
                          :components ((:file "package")            
                                       (:file "base-macros")))
                 (:module "declarative-values"
                          :serial t
                          :components ((:file "package")
                                       (:file "declarative-values")))
                 (:module "time"
                          :serial t
                          :components ((:file "package")
                                       (:file "base-time-backend")
                                       (:file "base-time")
                                       (:file "conditional-funcs")
                                       (:file "tiny-time-manager")))
                 (:module "cgl"
                          :serial t
                          :components((:file "package")
                                      (:file "cl-opengl-replacements")
                                      (:file "context")
                                      (:file "generics")
                                      (:file "pixel-format")
                                      (:file "cffi-extra-primitive-types")
                                      (:file "gl-extras")
                                      (:file "c-values")
                                      (:file "c-arrays")
                                      (:file "structs")
                                      (:file "buffers")
                                      (:file "equivalent-types")
                                      (:file "buffer-gpu-arrays")
                                      (:file "vaos")
                                      (:file "vertex-streams")
                                      (:file "uniforms")
                                      (:file "shaders")
                                      (:file "misc")
                                      (:file "textures")
                                      (:file "types")
                                      (:file "gmap")
                                      (:file "framebuffer")
                                      (:file "default-data")
                                      ))
                 (:module "ugly"
                          :serial t
                          :components((:file "swatch")
                                      (:file "particles")))
                 (:module "maths"
                          :serial t
                          :components ((:file "package")
                                       (:file "base-maths")
                                       (:file "maths")
                                       (:module "vectors"
                                                :serial t
                                                :components ((:file "package")
                                                             (:file "base-vectors")
                                                             (:file "vector2")
                                                             (:file "vector3")
                                                             (:file "vector4")
                                                             (:file "vectors")))
                                       (:module "matrices"
                                                :serial t
                                                :components ((:file "package")
                                                             (:file "base-matrices")
                                                             (:file "matrix3")
                                                             (:file "matrix4")
                                                             (:file "matrices")))
                                       (:module "quaternions"
                                                :serial t
                                                :components ((:file "package")
                                                             (:file "quaternions")))
                                       (:module "interpolation"
                                                :serial t
                                                :components ((:file "package")
                                                             (:file "interpolation")))))
                 (:module "cepl-camera"
                          :serial t
                          :components ((:file "package")            
                                       (:file "cepl-camera")))
                 (:modules "images"
                           :serial t
                           :components ((:file "devil-helper")))
                 (:module "meshes-and-models"
                          :serial t
                          :components ((:file "package")
                                       (:file "primitives")
                                       (:file "mesh")
                                       (:file "classimp-heplers")))
                 (:module "events"
                          :serial t
                          :components ((:file "event-base")
                                       (:file "events")
                                       (:file "sdl-event-sources")))
                 (:module "live"
                          :serial t
                          :components ((:file "bootstrapping")))
                 (:module "time"
                          :serial t
                          :components ((:file "time")))
                 (:module "cepl"
                          :serial t
                          :components ((:file "package")                         
                                       (:file "sdl-extras")
                                       (:file "cepl")))))
