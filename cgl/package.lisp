;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; package.lisp --- The cepl-gl package
;;;;
;;;; Copyright (c) 2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================
(in-package #:cl-user)

(defpackage :cepl-gl
  (:use :cl :cffi :base-macros :cepl-utils :varjo)
  (:nicknames :cgl)
  (:import-from :cl-opengl
                :clear-color
                :enable
                :disable
                :cull-face
                :front-face
                :depth-mask
                :depth-func
                :depth-range
                :clear
                :clear-depth
                :flush
                :viewport
                :delete-shader)
  (:shadow :float)
  (:export :gl-context
           :clear-gl-context-cache
           :gl-free
           :update-display
           :valid-pixel-format-p
           :pixel-format
           :internal-format-from-pixel-format
           :pixel-format-from-internal-format
           :pixel-format-of
           :describe-pixel-format
           :defglstruct
           :c-array-byte-size
           :gl-calc-byte-size
           :make-c-array-from-pointer
           :with-c-array
           :free-c-array
           :clone-c-array
           :make-c-array
           :aref-c
           :aref-c*
           :c-populate
           :gl-subseq
           :gl-pull
           :gl-pull-1
           :gl-push
           :dimensions ; [TODO] this isnt really inline with array-dimensions
           :backed-by ; [TODO] is this the right name for the job?
           :element-type
           :1d-p
           :bind-buffer
           :force-bind-buffer
           :unbind-buffer
           :gen-buffer
           :buffer-data-raw
           :buffer-data
           :buffer-sub-data
           :multi-buffer-data
           :buffer-reserve-block-raw
           :buffer-reserve-block
           :buffer-reserve-blocks
           :make-gpu-array
           :make-gpu-arrays
           :with-gpu-array-as-c-array
           :suitable-array-for-index-p
           :bind-vao
           :bind-vertex-array
           :make-vao-from-formats
           :make-vao
           :make-raw-vertex-stream
           :make-vertex-stream
           :make-texture
           :bind-texture
           :with-texture-bound
           :mutable-texturep
           :upload-c-array-to-gpuarray-t ; this is a crap name
           :calc-sampler-type
           :dimensions-at-mipmap-level
           :establish-texture-type
           :gl-texture
           :gpu-array-t
           :texref
           :defpipeline
           :defvshader
           :deffshader
           :defgshader
           :defshader
           :defsmacro
           :defsfun
           :free-managed-resources
           :free-buffer
           :free-buffers
           :free-vertex-stream
           :free-texture
           :free-textures
           :free-gpu-array
           :free-vao
           :p-n-t
           ;;----------
           :delete-shader
           :clear-color
           :cls
           :enable
           :disable
           :cull-face
           :front-face
           :depth-mask
           :depth-func
           :depth-range
           :clear
           :clear-depth
           :flush
           :viewport))

(defpackage :%cgl
  (:use :cl :varjo :cgl))
